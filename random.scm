;; Utilities for randomization
(define-module (random))
(export sample chance)

(use-modules (oop goops)
             (sugar))

;; Surprisingly, Guile don't use a random state by default.
(set! *random-state* (random-state-from-platform))

(define-method (sample (lst <list>))
  (if (empty? lst)
      #f
      (list-ref lst (random (length lst)))))

(define-method (sample . args)
  (sample args))

(define-method (chance (fraction <number>))
  (< (random:uniform) fraction))
