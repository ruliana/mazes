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
  (root init-word: #:root getter: root)
  (content getter: content))

(define-method (initialize (self <distance>) initargs)
  (let-keywords initargs #f
                ([root #f]
                 [content (make-hash-table eq?)])
    (slot-set! self 'content content)
    (slot-set! self 'root root)
    (hash-table-set! content root 0)))

(define-method (ref (self <distance>) (cell <cell>))
  (hash-table-ref/default (content self) cell #f))

(define-method (set (self <distance>) (cell <cell>) (distance <number>))
  (hash-table-set! (content self) cell distance))

(define-method (longest (self <distance>))
  (apply max (hash-table-values (content self))))

(define-method (distances (root <cell>))
  (define distance (make <distance> root: root))
  (define frontier (enq! (make-q) root))
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
  (do ()
      [(q-empty? frontier) distance]
      (visit (deq! frontier))))

;; Display maze
(define (display-maze-ascii algorithm rows cols)
  (display (->string (algorithm (make <grid> rows: rows cols: cols)))))

(define* (display-maze-graph file-name algorithm r c optional: (cell-coord-color-start '(0 0)))
  (let* ([maze (algorithm (make <grid> rows: r cols: c))])
    (apply colorize maze file-name cell-coord-color-start)))

(define-method (colorize (grid <grid>) (file-name <string>) (from-row <integer>) (from-col <integer>))
  (let* ([distance (distances (ref grid from-row from-col))]
         [max-distance (* 0.5 (longest distance))])
    (->svg file-name
          (rows grid)
          (cols grid)
          (map ->bits (cells grid))
          (map (λ (e) (- 1 (/ (or (ref distance e) max-distance) max-distance))) (cells grid))
          from-row from-col)
    (values grid distance)))

;; (define-values (maze distance) (display-maze-graph "./labyrinth.svg" (Λ sidewinder! <> 0.9) 38 80))
(var rows = 10
     cols = 10
     maze = (sidewinder! (make <grid> rows: rows cols: cols) 0.9)
     (for (:parallel
           (: coord (lst (: row 0 rows)
                         (: col 0 cols)
                         (list row col)))
           (:integers idx))
       ;;(format #t "~a ~a ~a\n" (1st coord) (2nd coord) idx)
       (colorize maze (format #f "./images/lab_~5,'0d.svg" idx) (1st coord) (2nd coord))))
