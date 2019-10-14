;; Utilities for randomization
(define-module (random)
  use-module: (syntax base)
  use-module: (collections base)
  use-module: (ice-9 match)
  duplicates: (merge-generics last))

(export sample chance prob-sample)

;; Surprisingly, Guile don't use a random state by default.
(set! *random-state* (random-state-from-platform))

(def (sample lst <list>)
  (if (empty? lst)
      #f
      (list-ref lst (random (length lst)))))

(define (sample . args)
  (sample args))

(def (chance fraction <number>)
  (< (random:uniform) fraction))

(def (prob-sample probabilities <list> results <list>)
  (let* ([value (random:uniform)]
         [in-rand (Λ < value <>)])
    (let loop ([probas (cumulative-sum probabilities)]
               [results results])
      (match (list probas results)
        [(() _) #f]
        [(((? in-rand prob) a ...) (thing b ...)) thing]
        [((h1 t1 ...) (h2 t2 ...)) (loop t1 t2)]))))
