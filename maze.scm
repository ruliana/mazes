(add-to-load-path "./")
(use-modules (oop goops)
             (srfi srfi-88)  ;; keywords
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

(display-maze-graph "./labyrinth.svg" (Λ sidewinder! <> 0.95) 36 80)
;; (display-maze-graph "./labyrinth.svg" binary-tree! 36 80)
;; (display-maze-graph "./labyrinth.svg" binary-tree! 8 8)
