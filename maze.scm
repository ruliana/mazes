(add-to-load-path "./")
(define-module (maze)
  #:use-module (oop goops)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 match)
  #:use-module (ice-9 q)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-88) ;; keywords
  #:use-module (srfi srfi-69) ;; hashtable
  #:use-module (sugar)
  #:use-module (render)
  #:use-module (random)
  #:use-module (base))

;; MAZE ALGORITHMS

;; Binary tree

(def (binary-tree! (self <grid>))
  (def (make-hole cell)
    (cond
     [(and (east cell) (north cell)) (carve (sample north east) cell)]
     [(east cell) (carve east cell)]
     [(north cell) (carve north cell)]))
  (for-each make-hole self)
  self)

;; Sidewider
(def (sidewinder! (self <grid>))
  (sidewinder! self 0.7))

(def (sidewinder! (self <grid>) (horizontal-probability <real>))
  (def (carve-row row run)
    (return #f if (empty? row))
    (var [cell (car row)
          rest (cdr row)]
      (cond
       [(and (east cell) (chance horizontal-probability))
        (carve-row rest (cons (carve east cell) run))]
       [(not (north cell))
        (carve-row rest (cons (carve east cell) run))]
       [else (carve north (sample (cons cell run)))
             (carve-row rest '())])))
  (for-each-row (Λ carve-row <> '()) self)
  self)

;; Aldous Broder
(def (aldous-broder! (self <grid>) sample-neighbor)
  (let loop ([unvisited (sub1 (size self))]
             [cell (random-cell self)])
    (var [neighbor (sample-neighbor cell)]
      (cond
       [(zero? unvisited) self]
       [(links? neighbor)
        (link cell neighbor)
        (loop (sub1 unvisited) neighbor)]
       [else (loop unvisited neighbor)]))))

;; Wilson's
(def (wilson! (self <grid>) sample-neighbor)

  (def (solidify-path unvisited path)
    (return #f if (empty? path))
    (for (:consecutive cell1 cell2 path)
      (link cell1 cell2)))

  (def (carve unvisited)
    (return self if (empty? unvisited))

    (def (build-path path cell counter)
      (return path unless (member cell unvisited))
      (return '() if (> counter 10))
      (var [new-cell (sample-neighbor cell)
            remaining (member new-cell path)]
        (if remaining
            (build-path remaining new-cell (add1 counter))
            (build-path (cons new-cell path) new-cell (add1 counter)))))

    (var [cell (sample unvisited)
          path (build-path (list cell) cell 0)
          unvisited (filter-out (Λ member <> path) unvisited)]
      (solidify-path unvisited path)
      (carve unvisited)))

  (var [all-cells (cells self)
        unvisited (rest all-cells)]
    (carve unvisited)))

;; Randow walkers for textures in Aldous Broder and Wilson
(var sample-neighbor-vert-nw (make-neighbor-sampler 0.8 0.0 0.2 0.0))
(var sample-neighbor-vert-sw (make-neighbor-sampler 0.0 0.8 0.2 0.0))
(var sample-neighbor-horz-nw (make-neighbor-sampler 0.2 0.0 0.8 0.0))
(var sample-neighbor-horz-sw (make-neighbor-sampler 0.0 0.2 0.8 0.0))
(var sample-neighbor-horz-nm (make-neighbor-sampler 0.04 0.0  0.48 0.48))
(var sample-neighbor-horz-sm (make-neighbor-sampler 0.0  0.04 0.48 0.48))
(var sample-neighbor-random (make-neighbor-sampler 0.25 0.25 0.25 0.25))

(def (sample-neighbor-multitexture (grid <grid>))
  (def (sample-neighbor cell)
    (var [col1? (columner grid 5 1)
          col2? (columner grid 5 2)
          col3? (columner grid 5 3)
          col4? (columner grid 5 4)
          col5? (columner grid 5 5)
          row1? (rowner grid 3 1)
          row2? (rowner grid 3 2)
          row3? (rowner grid 3 3)]
      (cond
        [(col1? cell) (sample-neighbor-vert-nw cell)]
        [(col2? cell) (sample-neighbor-horz-nm cell)]
        [(and (row2? cell) (col3? cell)) (sample-neighbor-random cell)]
        [(col3? cell) (sample-neighbor-horz-nm cell)]
        [(col4? cell) (sample-neighbor-horz-nm cell)]
        [(col5? cell) (sample-neighbor-vert-sw cell)])))
  sample-neighbor)

;; Breadth First Search
;; A "maze solver"
;;
;; It actually computes the distance of every cell
;; from a root.

(class <distance> ()
  (content getter: content))

(def (initialize (self <distance>) initargs)
  (slot-set! self 'content (make-hash-table eq?)))

(def (ref (self <distance>) (cell <cell>))
  (hash-table-ref/default (content self) cell #f))

(def (set (self <distance>) (cell <cell>) (distance <number>))
  (hash-table-set! (content self) cell distance))

(def (longest (self <distance>))
  (apply max (hash-table-values (content self))))

(def (path (self <distance>))
  (map left
    (sort (hash-table->alist (content self))
          (λ (a b) (< (right a) (right b))))))

(def (distances (grid <grid>) (source <coord>))
  (var root (ref grid source)
       distance (make <distance>)
       frontier (enq! (make-q) root))
  (def (head-visited? lst) (ref distance (1st lst)))
  (def (visit cell)
    (visit-neighbors (links cell) (ref distance cell)))
  (def (visit-neighbors neighs dist)
    (match neighs
      [() #f]
      [(? head-visited? (head tail ...))
       (visit-neighbors tail dist)]
      [(head tail ...)
       (set distance head (1+ dist))
       (enq! frontier head)
       (visit-neighbors tail dist)]))

  (set distance root 0)
  (do ()
      [(q-empty? frontier) distance]
      (visit (deq! frontier)))
  distance)

(def (shortest-path (grid <grid>) (source-coord <coord>) (target-coord <coord>))
  (var distance (distances grid source-coord)
       path (make <distance>)
       source (ref grid source-coord)
       target (ref grid target-coord))
  (set path target (ref distance target))
  (let loop ([current target])
    (if (eq? current source) path
        (var [current-distance (ref distance current)
              follow (filter (λ (e) (< (right e) current-distance))
                             (map (λ (e) (pair e (ref distance e)))
                                  (links current)))]
          (match follow
            [() path]
            [((next . dist) . _)
             (set path next dist)
             (loop next)])))))

;; Display maze
(def (display-maze-ascii algorithm rows cols)
  (display (->string (algorithm (make <grid> rows: rows cols: cols)))))

(def (display-maze-graph file-name algorithm r c)
  (display-maze-graph file-name algorithm r c (coord 16 32)))

(def (display-maze-graph-bw file-name algorithm r c)
  (var grid (algorithm (make <grid> rows: r cols: c)))
  (->svg file-name
         (rows grid)
         (cols grid)
         (map ->bits (cells grid)))
  (values grid #f))

(def (display-maze-graph file-name algorithm r c cell-coord-color-start)
  (var maze (algorithm (make <grid> rows: r cols: c)))
  (colorize maze file-name cell-coord-color-start))

(def (colorize (grid <grid>) (file-name <string>) (from <coord>))
  (var distance (distances grid from)
       max-distance (* 0.5 (longest distance)))
  (->svg file-name
        (rows grid)
        (cols grid)
        (map ->bits (cells grid))
        (map (λ (e) (- 1 (/ (or (ref distance e) max-distance) max-distance))) (cells grid))
        (row from) (col from))
  (values grid distance))

(define-values (maze distance)
  (display-maze-graph-bw "./labyrinth1.svg"
                         (λ (g) (wilson! g (sample-neighbor-multitexture g)))
                         21 63))

;; (define-values (maze distance)
;;   (display-maze-graph "./labyrinth2.svg"
;;                       (λ (g) (aldous-broder! g sample-neighbor-random))
;;                       32 64))

;; (def (animation (algorithm <generic>) (rows <integer>) (cols <integer>))
;;   (var half-row (/ (- cols 1) 2)
;;        rows-idx (sub1 rows)
;;        cols-idx (sub1 cols)
;;        maze (algorithm (make <grid> rows: rows cols: cols)))
;;   (for (:parallel
;;         (: point (path (shortest-path maze (coord 0 0) (coord rows-idx cols-idx))))
;;         (:integers idx))
;;        (colorize maze (format #f "./images/maze_~5,'0d.svg" idx) (coord point))))

;; (animation wilson! 32 64)
