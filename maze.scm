(add-to-load-path "./")
(use-modules (oop goops)
             (ice-9 optargs)
             (ice-9 match)
             (ice-9 q)
             (ice-9 format)
             (srfi srfi-88) ;; keywords
             (srfi srfi-69) ;; hashtable
             (render)
             (random)
             (base)
             (sugar))

;; MAZE ALGORITHMS

;; Binary tree

(define-method (binary-tree! (self <grid>))
  (define (make-hole cell)
    (cond
     [(and (east cell) (north cell)) (carve (sample north east) cell)]
     [(east cell) (carve east cell)]
     [(north cell) (carve north cell)]))
  (for-each make-hole self)
  self)

;; Sidewider
(define-method (sidewinder! (self <grid>))
  (sidewinder! self 0.7))

(define-method (sidewinder! (self <grid>) (horizontal-probability <real>))
  (define (carve-row row run)
    (if (not (empty? row))
        (let ([cell (car row)]
              [rest (cdr row)])
          (cond
           [(and (east cell) (chance horizontal-probability))
            (carve-row rest (cons (carve east cell) run))]
           [(not (north cell))
            (carve-row rest (cons (carve east cell) run))]
           [else (carve north (sample (cons cell run)))
                 (carve-row rest '())]))))
  (for-each-row (Λ carve-row <> '()) self)
  self)

;; Breadth First Search
;; A "maze solver"
;;
;; It actually computes the distance of every cell
;; from a root.

(define-class <distance> ()
  (content getter: content))

(define-method (initialize (self <distance>) initargs)
  (slot-set! self 'content (make-hash-table eq?)))

(define-method (ref (self <distance>) (cell <cell>))
  (hash-table-ref/default (content self) cell #f))

(define-method (set (self <distance>) (cell <cell>) (distance <number>))
  (hash-table-set! (content self) cell distance))

(define-method (longest (self <distance>))
  (apply max (hash-table-values (content self))))

(define-method (path (self <distance>))
  (map left
    (sort (hash-table->alist (content self))
          (λ (a b) (< (right a) (right b))))))

(define-method (distances (grid <grid>) (source <coord>))
  (var root (ref grid source)
       distance (make <distance>)
       frontier (enq! (make-q) root))
  (define (head-visited? lst) (ref distance (1st lst)))
  (define (visit cell)
    (visit-neighbors (links cell) (ref distance cell)))
  (define (visit-neighbors neighs dist)
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

(define-method (shortest-path (grid <grid>) (source-coord <coord>) (target-coord <coord>))
  (var distance (distances grid source-coord)
       path (make <distance>)
       source (ref grid source-coord)
       target (ref grid target-coord))
  (set path target (ref distance target))
  (let loop ([current target])
    (if (eq? current source) path
        (let* ([current-distance (ref distance current)]
               [follow (filter (λ (e) (< (right e) current-distance))
                               (map (λ (e) (pair e (ref distance e)))
                                    (links current)))])
          (match follow
            [() path]
            [((next . dist) . _)
             (set path next dist)
             (loop next)])))))


;; Display maze
(define (display-maze-ascii algorithm rows cols)
  (display (->string (algorithm (make <grid> rows: rows cols: cols)))))

(define* (display-maze-graph file-name algorithm r c optional: (cell-coord-color-start (coord 0 0)))
  (var maze (algorithm (make <grid> rows: r cols: c)))
  (colorize maze file-name cell-coord-color-start))

(define-method (colorize (grid <grid>) (file-name <string>) (from <coord>))
  (var distance (distances grid from)
       max-distance (* 0.5 (longest distance)))
  (->svg file-name
        (rows grid)
        (cols grid)
        (map ->bits (cells grid))
        (map (λ (e) (- 1 (/ (or (ref distance e) max-distance) max-distance))) (cells grid))
        (row from) (col from))
  (values grid distance))

;; (define-values (maze distance) (display-maze-graph "./labyrinth.svg" (Λ sidewinder! <> 0.9) 10 10))

;; (for (: e (path (shortest-path maze (coord 0 0) (coord 9 9))))
;;      (format #t "~a\n" (display e #f)))

(let ()
  (var rows 31
       cols 31
       half-row (/ (- cols 1) 2)
       rows-idx (- rows 1)
       maze (sidewinder! (make <grid> rows: rows cols: cols) 0.9))
  (for (:parallel
        (: point (path (shortest-path maze (coord 0 half-row) (coord rows-idx half-row))))
        (:integers idx))
       (colorize maze (format #f "./images/lab_~5,'0d.svg" idx) (coord point))))
