;; Basic maze objects to be used in the algorithms
(add-to-load-path "./")
(define-module (base)
  use-module: (syntax base)
  use-module: (collections base)
  use-module: (ice-9 match)
  use-module: (random)
  use-module: (render)
  duplicates: (merge-generics last))

(export <coord>
        coord row col)
(export <cell>
        row col
        north south west east
        ->bits
        link link? unlink links
        no-links? any-link?
        make-neighbor-sampler
        neighbors carve)
(export <grid>
        rows cols
        ref cells
        random-cell
        size
        ->string
        for-each for-each-row
        columner rowner)
(export display write show str)
(export exclude-cells)


;; Row Col Coordinates
(class <coord> ()
  (row init-keyword: #:row getter: row)
  (col init-keyword: #:col getter: col))

(def (coord (row <integer>) (col <integer>))
  (make <coord> row: row col: col))


;; Cell
(class <cell> ()
  (row init-keyword: #:row getter: row)
  (col init-keyword: #:col getter: col)
  (north init-value: #f accessor: north)
  (south init-value: #f accessor: south)
  (west init-value: #f accessor: west)
  (east init-value: #f accessor: east)
  (links init-thunk: dict))

(def (coord (self <cell>))
  (coord (row self) (col self)))

(def (->bits (self <cell>))
  (let loop ([rslt #b1111]
             [directions (list north south west east)]
             [bit-list (list #b0111 #b1011 #b1101 #b1110)])
    (cond [(empty? directions) rslt]
          [(link? self ((head directions) self))
           (loop (logand rslt (head bit-list))
                 (tail directions)
                 (tail bit-list))]
          [else
           (loop rslt
                 (tail directions)
                 (tail bit-list))])))

(def (link (first <cell>) (second <cell>))
  (put! second #t (slot-ref first 'links))
  (put! first #t (slot-ref second 'links)))

;; If any of them are not cells, just ignore
(def (link first second) #f)

(def (link? (first <cell>) (second <cell>))
  (key? second (slot-ref first 'links)))

(def (link? first second) #f)

(def (unlink (first <cell>) (second <cell>))
  (delete! second (slot-ref first 'links))
  (delete! first (slot-ref second 'links)))

(def (links (self <cell>))
  (keys (slot-ref self 'links)))

(def (no-links? (self <cell>))
  (empty? (links self)))

(def (any-link? (self <cell>))
  (not (empty? (links self))))

(def (neighbors (self <cell>))
  (for-list (: dir (list north south west east))
            (:let neigh (dir self))
            (if neigh)
            neigh))

(def (make-neighbor-sampler (probas <list>))
  (define (choose cell)
     (var [direction (prob-sample probas (list north south west east))
           next (direction cell)]
       (if next next (choose cell))))
  choose)

(def (carve (proc <accessor>) (cell <cell>))
  (link cell (proc cell))
  cell)

;; No cell, no carving
(def (carve (proc <accessor>) anything)
  anything)

(def (display (self <cell>))
  (display self #f))

(def (display (self <cell>) port)
  (format port "~a" (str self)))

(def (str (self <cell>))
  (format #f "[~a ~a]" (row self) (col self)))

(def (write (self <cell>) port)
  (format port "<cell> row: ~a col: ~a" (row self) (col self)))

(def (show (self <cell>))
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

(class <grid> ()
  (rows init-keyword: #:rows getter: rows)
  (cols init-keyword: #:cols getter: cols)
  (grid getter: cells))

(def (initialize (self <grid>) initargs)
  (define pos (Λ ref self <> <>))

  (define (create-grid)
    (for-vector (: r (rows self))
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

(def (ref (self <grid>) (row <integer>) (col <integer>))
  (if (and (< -1 row (rows self))
           (< -1 col (cols self)))
      (let ([g (slot-ref self 'grid)]
            [index (+ col (* row (cols self)))])
        (vector-ref g index))
      #f))

(def (ref (self <grid>) (coord <coord>))
  (ref self (row coord) (col coord)))

(def (random-cell (self <grid>))
  (ref self
       (random (rows self))
       (random (cols self))))

(def (size (self <grid>))
  (* (rows self)
     (cols self)))

(define-generic for-each)
(def (for-each (proc <applicable>) (self <grid>))
  (for-each proc (cells self)))

(def (for-each (proc <applicable>) (self <vector>))
  (for (: cell self) (proc cell)))

(def (for-each-row (proc <applicable>) (self <grid>))
  (let loop ([rows (cells self)])
    (if (not (empty? rows))
        (begin
          (proc (take (cols self) rows))
          (loop (drop (cols self) rows))))))

(def (->string (self <grid>))
  (define glyphs (map show (cells self)))
  (let loop ([lst (vector->list glyphs)]
             [counter 1]
             [rslt '()])
    (cond
     [(empty? lst)
      (string-join (reverse rslt) "")]
     [(zero? (modulo counter (cols self)))
      (loop (cdr lst) (1+ counter) (cons "\n" (cons (car lst) rslt)))]
     [else (loop (cdr lst) (1+ counter) (cons (car lst) rslt))])))


;; Where am I in the grid?

(def (columner (grid <grid>) (all-cols <integer>) (this-col <integer>))
  (var [lower-bound (* (sub1 this-col) (/ (cols grid) all-cols))
        upper-bound (* this-col (/ (cols grid) all-cols))])
  (λ (cell)
    (and (>= (col cell) lower-bound)
         (<  (col cell) upper-bound))))

(def (rowner (grid <grid>) (all-rows <integer>) (this-row <integer>))
  (var [lower-bound (* (sub1 this-row) (/ (rows grid) all-rows))
        upper-bound (* this-row (/ (rows grid) all-rows))])
  (λ (cell)
    (and (>= (row cell) lower-bound)
         (<  (row cell) upper-bound))))


;; More helpers
(def (exclude-cells (from <list>) (exclude <list>))
  (for-list (: it from)
            (if (not (member it exclude)))
            it))
