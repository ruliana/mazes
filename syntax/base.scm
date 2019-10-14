(define-module (syntax base)
  use-module: (ice-9 match)
  use-module: (ice-9 optargs)
  use-module: (srfi srfi-71)  ;; Multivalued let
  use-module: (srfi srfi-88)  ;; Sane keywords
  use-module: (srfi srfi-26)  ;; Cut
  use-module: (oop goops)     ;; Screw OOP, I need function overloading! :D
  duplicates: (merge-generics last))

(re-export make
           (class . raw-class)
           (define-class . class)
           define-generic
           slot-ref slot-set!
           initialize
           equal? eqv? =
           ;; Introspection
           class-of
           class-name
           class-direct-supers
           ;; Base hierarchy
           <applicable-struct> <applicable-struct-class>
           <procedure> <accessor> <method> <generic> <applicable>
           <object> <class>
           <top>
           <real> <number> <integer>
           <string> <list> <vector>
           <syntax>)

(export def)
(define-syntax prepare-if
  (syntax-rules (return if unless)
    [(_ (return this if that) body ...)
     (if that this (prepare-if body ...))]
    [(_ (return this unless that) body ...)
     (if that (prepare-if body ...) this)]
    [(_ body ...)
     (begin body ...)]))

(define (partition-syntax-args arg-list)
  (define (class-like-name? sym)
    (and (symbol? sym)
      (let ([name (symbol->string sym)])
        (and (string-prefix? "<" name)
             (string-suffix? ">" name)))))
  (let loop ([args arg-list]
             [arguments '()]
             [keywords '()])
    (match args
      [() (values (reverse arguments) (reverse keywords))]
      ;; Keyword
      [(or ((? keyword? arg1) arg2 rest ...)
           (((? keyword? arg1) arg2) rest ...))
       (loop rest arguments (cons (list arg1 arg2) keywords))]
      ;; Class declaration
      [(or (arg1 (? class-like-name? arg2) rest ...)
           ((arg1 (? class-like-name? arg2)) rest ...))
       (loop rest (cons (list arg1 arg2) arguments) keywords)]
      ;; A simple arg
      [(arg rest ...)
       (loop rest (cons arg arguments) keywords)])))

(define-syntax def
  (lambda (stx)
    (define-syntax-rule (map-match (pat action) ...)
        (λ (arg) (map (match-lambda (pat action) ...) arg)))
    (define (d->stx value) (datum->syntax stx value))
    (define (k->stx value) (d->stx (keyword->symbol value)))
    (define arguments->syntax
      (map-match
        [(a b) (list (d->stx a) (d->stx b))]
        [a (d->stx a)]))
    (define keywords->syntax
      (map-match
        [(k v) (list (k->stx k) (d->stx v))]))
    (syntax-case stx (return if)
      [(_ (name args ...) body ...)
       (let* ([as ks (partition-syntax-args (syntax->datum #'(args ...)))])
         (with-syntax ([(arguments ...) (arguments->syntax as)]
                       [(keywords ...) (keywords->syntax ks)])
           #'(define-method (name arguments ... . optionals)
               (let-keywords optionals #f
                             (keywords ...)
                             (prepare-if body ...)))))])))


;; Simplified let
;;
;; I'm still experimenting with alternatives.
;; The simplest is make it an alias for "define"
;; The most complex is to make is an alias for "match-let*"
;;
;; (var [x 1])                <= define
;; (var [x 1] something)      <= let
(export var)
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


;; Upper lambda as an alias for "cut".
;;
;; I'd rather use λ than "lambda", too.
(re-export (cut . Λ))


;; Minor utilities
(re-export (1+ . add1)
           (1- . sub1))


;; Printing and str formating
(export print)
(define* (print . args)
  (apply format #t (string-append (apply string-append (make-list (length args) "~a")) "\n") args))

(re-export (string-append . str))
