;; A lot of sugary to make Guile more suitable
;; to my personal taste.
;;
;; This is a mix of preferences with abstractions
;; for some common patterns. Therefore, expect a
;; lot of ranting in the comments. I made that todo
;; remember why I thought it was a good idea at that
;; moment. Later, probably, I'll find those comments
;; kinda naive.
(define-module (sugar)
  #:use-module (oop goops)     ;; Screw OOP, I need function overloading! :D
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-88)  ;; Sane keywords
  #:use-module ((srfi srfi-1)  ;; Great list operators
                ;; Weird trick to be able to use a word ending with ":"
                ;; and srfi-88 at same time.
                #:renamer (symbol-prefix-proc (string->symbol "srfi1:")))
  #:use-module (srfi srfi-26)  ;; Cut
  #:use-module (srfi srfi-42)  ;; List comprehension
  #:duplicates (merge-generics))

(export Λ var)
(export empty? rest
        pair left right
        1st 2nd 3rd 4th 5th
        first last
        zip
        take drop
        filter-map
        filter-out
        size)

;; Upper lambda as an alias for "cut".
;;
;; I'd rather use λ than "lambda". We not
;; in the 80s anymore, are we?
(define-syntax Λ (identifier-syntax cut))

(define 1st srfi1:first)
(define 2nd srfi1:second)
(define 3rd srfi1:third)
(define 4th srfi1:fourth)
(define 5th srfi1:fifth)

(define zip srfi1:zip)

;; For using with pairs
(define pair cons)
(define left car)
(define right cdr)


;; List comprehension with sane names
(export for
        lst
        :consecutive)
(re-export :
           :range
           :integers
           :parallel)

(define-syntax for (identifier-syntax do-ec))
(define-syntax lst (identifier-syntax list-ec))

(define-syntax :consecutive
  (syntax-rules ()
    [(_ cc v1 v2 coll)
     (:parallel cc
                (: v1 coll)
                (: v2 (rest coll)))]
    [(_ cc v1 v2 v3 coll)
     (:parallel cc
               (: v1 coll)
               (: v2 (rest coll))
               (: v3 (rest (rest coll))))]))

;; GOOPS as default way to do things
;;
;; That means names that are shorter, more common
;; in other languages, and maybe some extra default
;; definitions.
(export class
        def)
(re-export make
           define-generic
           slot-ref slot-set!
           initialize
           <procedure> <accessor> <method> <generic>
           <object> <class>
           <real> <number> <integer>
           <string> <list>)

(define-syntax class (identifier-syntax define-class))

(define-syntax prepare-if
  (syntax-rules (return if unless)
    [(_ (return this if that) body ...)
     (if that this (prepare-if body ...))]
    [(_ (return this unless that) body ...)
     (if that (prepare-if body ...) this)]
    [(_ body ...)
     (begin body ...)]))

(define-syntax def
  (syntax-rules (return if)
    [(_ (definition ...)
        body ...)
     (define-method (definition ...)
       (prepare-if body ...))]))



;; SRFI-1
;; But with a sane parameter order and more intention
;; revealing names.
;;
;; Also, use function overloading, so we can apply
;; the same thing to vectors, hashtables, streams
;; strings and other collection like types.

(define-method (empty? (lst <list>)) (null? lst))

(define-method (rest (lst <list>)) (cdr lst))

(define-method (first (lst <list>)) (car lst))

(define-method (last (lst <list>)) (srfi1:last lst))

(define-method (take (qty <integer>) (lst <list>))
  (srfi1:take lst qty))

(define-method (drop (qty <integer>) (lst <list>))
  (srfi1:drop lst qty))

(define-method (filter-map (f <procedure>) (lst <list>))
  (srfi1:filter-map f lst))

(define-method (filter-out (f <procedure>) (lst <list>))
  (filter (negate f) lst))

(define-generic size)
(define-method (size (self <list>)) (length self))
(define-method (size (self <string>)) (string-length self))


;; Simplified let
;;
;; I'm still experimenting with alternatives.
;; The simplest is make it an alias for "define"
;; The most complex is to make is an alias for "match-let*"
;;
;; (var [x 1])                <= define
;; (var [x 1] something)      <= let
;; (var loop [x 1] something) <= named let optional
(export internal-let)
(define-syntax internal-let
  (syntax-rules ()
    [(_ (done ...) () expr ...)
     (let* (done ...) expr ...)]
    [(_ (done ...) (v val remaining ...) expr ...)
     (internal-let (done ... (v val)) (remaining ...) expr ...)]))

(define-syntax var
  (syntax-rules ()
    [(_ () expr0 expr* ...)
     (let () expr0 expr* ...)]

    [(_ (v val))
     (define v val)]

    [(_ (v val rest ...))
     (begin (define v val)
            (var (rest ...)))]

    [(_ (args ...) expr0 expr* ...)
     (internal-let
      ()
      (args ...)
      expr0 expr* ...)]

    [(_ args ...)
     (var (args ...))]))


;; Small helpers
(export add1 sub1)

(define add1 1+)
(define sub1 1-)


;; Printing
(export str print)

(define-method (str thing)
  (format #f "~a" thing))

(define-method (str thing1 thing2 . things)
  (string-join (map str `(,thing1 ,thing2 ,@things))))

(define-method (print . things)
  (format #t "~a\n" (apply str things)))


;; Helpers for randomization
(export prob-sample)
(define (prob-sample . probability-pairs)
  (define (cumulative-probas probability-pairs)
    (let loop ([probability-pairs (filter-out (λ (e) (zero? (car e)))
                                              probability-pairs)]
               [rslt '()]
               [sum 0])
      (match probability-pairs
        [() (reverse rslt)]
        [((prob thing) tail ...)
         (loop tail
               (cons (list (+ sum prob) thing) rslt)
               (+ sum prob))])))
  (let* ([probas (cumulative-probas probability-pairs)]
         [value (random:uniform)]
         [in-rand (Λ < value <>)])
    (let loop ([probas probas])
      (match probas
        [() #f]
        [(((? in-rand prob) thing) tail ...) thing]
        [(head tail ...) (loop tail)]))))
