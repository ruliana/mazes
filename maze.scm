(add-to-load-path "./")
(use-modules (oop goops)
             (srfi srfi-1)   ;; sane lists
             (srfi srfi-26)  ;; cut
             (srfi srfi-42)  ;; list comprehension
             (srfi srfi-69)  ;; hash-table
             (srfi srfi-88)  ;; keywords
             (render))

;; Upper lambda as an alias for "cut"
(define-syntax Λ (identifier-syntax cut))

;; sane name for "null?"
(define empty? null?)

(define rest cdr)
(define first car)

(define-method (sample (lst <list>))
  (if (empty? lst)
      #f
      (list-ref lst (random (length lst)))))

(define-method (sample . args)
  (sample args))

(define-method (chance (fraction <number>))
  (< (random:uniform) fraction))

(define-method (size (self <string>)) (string-length self))
(define-method (size (self <list>)) (length self))

;; Cell
(define-class <cell> ()
  (row init-keyword: #:row getter: row)
  (col init-keyword: #:col getter: col)
  (north init-value: #f accessor: north)
  (south init-value: #f accessor: south)
  (west init-value: #f accessor: west)
  (east init-value: #f accessor: east)
  (links init-form: (make-hash-table eq?)))

(define-method (->bits (self <cell>))
  (let loop ([rslt #b1111]
             [directions (list north south west east)]
             [bit-list (list #b0111 #b1011 #b1101 #b1110)])
    (cond [(empty? directions) rslt]
          [(link? self ((car directions) self))
           (loop (logand rslt (car bit-list))
                 (rest directions)
                 (rest bit-list))]
          [else
           (loop rslt
                 (rest directions)
                 (rest bit-list))])))

(define-generic link)

(define-method (link (first <cell>) (second <cell>))
  (hash-table-set! (slot-ref first 'links) second #t)
  (hash-table-set! (slot-ref second 'links) first #t))

;; If any of them are not cells, just ignore
(define-method (link first second) #f)


(define-method (link? (first <cell>) (second <cell>))
  (hash-table-exists? (slot-ref first 'links) second))

(define-method (link? first second) #f)


(define-method (unlink (first <cell>) (second <cell>))
  (hash-table-delete! (slot-ref first 'links) second)
  (hash-table-delete! (slot-ref second 'links) first))

(define-method (links (self <cell>))
  (hash-table-keys (slot-ref self 'links)))

(define-method (neighbors (self <cell>))
  (filter-map (Λ <> self) (list north south west east)))


(define-method (carve (proc <accessor>) (cell <cell>))
  (link cell (proc cell))
  cell)

;; No cell, no carving
(define-method (carve (proc <accessor>) anything)
  anything)


(define-method (display (self <cell>) port)
  (format port "[~a ~a]" (row self) (col self)))

(define-method (write (self <cell>) port)
  (format port "<cell> row: ~a col: ~a" (row self) (col self)))

(define-method (show (self <cell>))
  (define (pass . directions)
    (and-map (λ (f) (link? self (f self))) directions))
  (cond
   [(pass north south west east) "╬"]
   [(pass north south west     ) "╣"]
   [(pass north south      east) "╠"]
   [(pass north       west east) "╩"]
   [(pass       south west east) "╦"]
   [(pass north       west     ) "╝"]
   [(pass north            east) "╚"]
   [(pass       south      east) "╔"]
   [(pass       south west     ) "╗"]
   [(pass north south          ) "║"]
   [(pass             west east) "═"]
   [else " "]))

;; Grid

(define-class <grid> ()
  (rows init-keyword: #:rows getter: rows)
  (cols init-keyword: #:cols getter: cols)
  (grid getter: cells))

(define-method (initialize (self <grid>) initargs)
  (define pos (Λ ref self <> <>))

  (define (create-grid)
    (list-ec (: r (rows self))
             (: c (cols self))
             (make <cell> row: r col: c)))

  (define (connect-neighbors cell)
    (define row (slot-ref cell 'row))
    (define col (slot-ref cell 'col))
    (set! (north cell) (pos (- row 1) col))
    (set! (south cell) (pos (+ row 1) col))
    (set! (west cell) (pos row (- col 1)))
    (set! (east cell) (pos row (+ col 1))))

  (next-method)
  (slot-set! self 'grid (create-grid))
  (map connect-neighbors (slot-ref self 'grid)))

(define-method (ref (self <grid>) (row <integer>) (col <integer>))
  (if (and (< -1 row (rows self))
           (< -1 col (cols self)))
      (let ([g (slot-ref self 'grid)]
            [index (+ col (* row (cols self)))])
        (list-ref g index))
      #f))

(define-method (random-cell (self <grid>))
  (ref self
       (random (rows self))
       (random (cols self))))

(define-method (size (self <grid>))
  (* (rows self)
     (cols self)))

(define-generic for-each)
(define-method (for-each (proc <procedure>) (self <grid>))
  (for-each proc (cells self)))

(define-method (for-each-row (proc <procedure>) (self <grid>))
  (let loop ([rows (cells self)])
    (if (not (empty? rows))
        (begin
          (proc (take rows (cols self)))
          (loop (drop rows (cols self)))))))

(define-method (->string (self <grid>))
  (define glyphs (map show (cells self)))
  (let loop ([lst glyphs]
             [counter 1]
             [rslt '()])
    (cond
     [(empty? lst)
      (string-join (reverse rslt) "")]
     [(zero? (modulo counter (cols self)))
      (loop (cdr lst) (1+ counter) (cons "\n" (cons (car lst) rslt)))]
     [else (loop (cdr lst) (1+ counter) (cons (car lst) rslt))])))

(define (display-maze-ascii algorithm rows cols)
  (display (->string (algorithm (make <grid> rows: rows cols: cols)))))

(define (display-maze-graph file-name algorithm r c)
  (let ([maze (algorithm (make <grid> rows: r cols: c))])
    (->svg file-name (rows maze) (cols maze) (map ->bits (cells maze)))))


;; MAZE ALGORITHMS

;; Surprisingly, Guile don't use a random state by default.
(set! *random-state* (random-state-from-platform))

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

(display-maze-graph "./labyrinth.svg" (Λ sidewinder! <> 0.95) 36 80)
;; (display-maze-graph "./labyrinth.svg" binary-tree! 36 80)
;; (display-maze-graph "./labyrinth.svg" binary-tree! 8 8)
