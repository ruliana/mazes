(add-to-load-path "./")
(use-modules (oop goops)
             (ice-9 optargs)
             (ice-9 match)
             (ice-9 q)
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

;; Djikstra
;; Not a maze creator, but a maze solver

(define-class <distance> ()
  (root init-word: #:root getter: root)
  (cells getter: cells))

(define-method (initialize (self <distance>) initargs)
  (let-keywords initargs #f
                ([root #f]
                 [cells (make-hash-table eq?)])
    (slot-set! self 'cells cells)
    (slot-set! self 'root root)
    (hash-table-set! cells root 0)))

(define-method (ref (self <distance>) (cell <cell>))
  (hash-table-ref/default (cells self) cell #f))

(define-method (set (self <distance>) (cell <cell>) (distance <number>))
  (hash-table-set! (cells self) cell distance))

(define-method (distances (root <cell>))
  (define distance (make <distance> root: root))
  (define (head-visited? lst) (ref distance (first lst)))
  (define (visit cell)
    (visit-neighbors (links cell) '() (ref distance cell)))
  (define (visit-neighbors neighs new-frontier dist)
    (match neighs
      [() new-frontier]
      [(? head-visited? (head tail ...))
       (visit-neighbors tail new-frontier dist)]
      [(head tail ...)
       (set distance head (1+ dist))
       (visit-neighbors tail (cons head new-frontier) dist)]))
  (let loop ([frontier (list root)])
    (match frontier
      [() distance]
      [(head tail ...)
       (loop (append tail (visit head)))])))

;; Display maze
(define (display-maze-ascii algorithm rows cols)
  (display (->string (algorithm (make <grid> rows: rows cols: cols)))))

(define (display-maze-graph file-name algorithm r c)
  (let* ([maze (algorithm (make <grid> rows: r cols: c))]
         [distance (distances (ref maze 0 0))])
    (->svg file-name
           (rows maze)
           (cols maze)
           (map ->bits (cells maze))
           (map (λ (e) (- 1 (/ (or (ref distance e) 80) 80))) (cells maze)))
    (values maze distance)))

(define (test)
  (let ([maze (sidewinder! (make <grid> rows: 3 cols: 3))])
     (list maze (distances (ref maze 0 0)))))

(define-values (maze distance) (display-maze-graph "./labyrinth.svg" (Λ sidewinder! <> 0.9) 30 60))
(let loop ([items (cells maze)])
  (match items
    [() #f]
    [(head tail ...)
     (format #t "~a ~a\n" (display head #f) (ref distance head))
     (loop tail)]))
