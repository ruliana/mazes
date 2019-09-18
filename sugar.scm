;; A lot of sugary to make Guile more suitable
;; to my personal taste.
;;
;; This is a mix of preferences with abstractions
;; for some common patterns. Therefore, expect a
;; lot of ranting in the comments. I made that todo
;; remember why I thought it was a good idea at that
;; moment. Later, probably, I'll find those comments
;; kinda naive.
(define-module (sugar))

(use-modules ((srfi srfi-1)  ;; Great list operators
              ;; Weird trick to be able to use a word ending with ":"
              ;; and srfi-88 at same time.
              renamer: (symbol-prefix-proc (string->symbol "srfi1:")))
             (srfi srfi-26)  ;; Cut
             (srfi srfi-42)  ;; List comprehension
             (srfi srfi-88)  ;; Sane keywords
             (oop goops))    ;; Screw OOP, I need function overloading!

(export Λ var)
(export empty? rest
        1st 2nd 3rd 4th 5th
        take drop
        filter-map)

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

;; For using with pairs
(define pair cons)
(define left car)
(define right cdr)


;; List comprehension with sane names
(export for
        lst)
(re-export :
           :range
           :integers
           :parallel)

(define-syntax for (identifier-syntax do-ec))
(define-syntax lst (identifier-syntax list-ec))


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

(define-method (take (qty <integer>) (lst <list>))
  (srfi1:take lst qty))

(define-method (drop (qty <integer>) (lst <list>))
  (srfi1:drop lst qty))

(define-method (filter-map (f <procedure>) (lst <list>))
  (srfi1:filter-map f lst))

;; This version is better for multilines.
;; Should we assume it as default?
(define-method (filter-map (lst <list>) (f <procedure>))
  (srfi1:filter-map f lst))

(define-method (size (self <list>)) (length self))
(define-method (size (self <string>)) (string-length self))


;; Simplified let
;;
;; I'm still experimenting with alternatives.
;; The simplest is make it an alias for "define"
;; The most complex is to make is an alias for "match-let*"
;;
;; Trying the second alternative first because I like local
;; state.
(define-syntax var
  (syntax-rules (=)
    [(_ v = value body ...)
     (let ([v value])
       (var body ...))]
    [(_ body0 body ...)
     (begin body0 body ...)]))
