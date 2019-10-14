(define-module (collections base)
  use-module: (syntax base)
  use-module: (ice-9 match)
  use-module: (ice-9 optargs)
  use-module: ((srfi srfi-1)  ;; Great list operators
               ;; Weird trick to be able to use a word ending with ":"
               ;; and srfi-88 at same time.
               renamer: (symbol-prefix-proc (string->symbol "srfi1:")))
  use-module: (srfi srfi-42)  ;; List comprehension
  use-module: (srfi srfi-43)  ;; Vector stuff
  use-module: (srfi srfi-69)  ;; Hashtable stuff
  use-module: (srfi srfi-71)  ;; Extended let
  duplicates: (merge-generics last))

;; Collections:
;; list - ordered, indexed, finite
;; vector - ordered, indexed, finite
;; dict - unordered, indexed, finite
;; string - ordered, indexed, finite
;; queue (TODO) - ordered, unindexed, finite
;; stream (TODO) - ordered, indexed, infinite
;; array (TODO) - ordered, indexed, finite
;; set (TODO) - unordered, unindexed, finite
;; set and update (TODO)
;; ordered dict and set (TODO)

(re-export (cons . pair)
           (car . left)
           (cdr . right))


;; Hash table lacks a lot of features of modern
;; dictionaries, like insertion order and equality.
;; We are going to need a better implementation.
(export <dict> dict)

;; For some reason the hash-table class from srfi-69 is not defined here.
(define <dict> (var [hsh (make-hash-table)] (class-of hsh)))

(define (dict . args)
  (let loop ([rslt (make-hash-table equal?)]
             [args args])
    (match args
      ['() rslt]
      [(k v rest ...) (loop (put! k v rslt) rest)])))

;; Why can't I define a generic equal?
;;(export equal?)
;; (def (equal? (a <dict>) (b <dict>))
;;   (return #f unless (= (size a) (size b)))
;;   (every?-ec (: k (keys a))
;;              (equal? (ref k a) (ref k b))))

(export keys)
(def (keys (dic <dict>))
  (hash-table-keys dic))

;; I'd like to use "values", but the name is already taken
;; by a more essential building block in scheme.
(export vals)
(def (vals (dic <dict>))
  (hash-table-values dic))

(export key?)
(def (key? k (dic <dict>))
  (hash-table-exists? dic k))

(export delete!)
(def (delete! k (dic <dict>))
  (hash-table-delete! dic k))

(export size)
(def (size (lst <list>)) (length lst))
(def (size (vec <vector>)) (vector-length vec))
(def (size (dic <dict>)) (hash-table-size dic))
(def (size (str <string>)) (string-length str))

(export empty?)
(def (empty? collection) (zero? (size collection)))
(def (empty? lst <list>) (null? lst))

(export ref)
(def (ref (k <integer>) (lst <list>) default: #f)
  (let loop ([k k]
             [lst lst])
    (cond
     [(null? lst) default]
     [(zero? k) (car lst)]
     [else (loop (- k 1) (cdr lst))])))

(def (ref (k <integer>) (vec <vector>) default: #f)
  (if (< k (size vec))
      (vector-ref vec k)
      default))

(def (ref (k <top>) (dic <dict>) default: #f)
  (hash-table-ref/default dic k default))

(def (ref (k <integer>) (str <string>) default: #f)
  (if (< k (size str))
      (substring str k (+ k 1))
      default))

;; Let's not mess with "set!" for now.
(export put!)
(def (put! (k <top>) (v <top>) (dic <dict>))
  (hash-table-set! dic k v)
  dic)

(def (put! (k <integer>) (v <top>) (vec <vector>))
  (vector-set! vec k v)
  vec)

(export 1st)
(def (1st collection) (ref 0 collection))
(def (1st collection <dict>) (ref 1 collection))

(export 2nd)
(def (2nd collection) (ref 1 collection))
(def (2nd collection <dict>) (ref 2 collection))

(export 3rd)
(def (3rd collection) (ref 2 collection))
(def (3rd collection <dict>) (ref 3 collection))

(export 4th)
(def (4th collection) (ref 3 collection))
(def (4th collection <dict>) (ref 4 collection))

(export 5th)
(def (5th collection) (ref 4 collection))
(def (5th collection <dict>) (ref 5 collection))

(export 6th)
(def (6th collection) (ref 5 collection))
(def (6th collection <dict>) (ref 6 collection))

(export 7th)
(def (7th collection) (ref 6 collection))
(def (7th collection <dict>) (ref 7 collection))

(export 8th)
(def (8th collection) (ref 7 collection))
(def (8th collection <dict>) (ref 8 collection))

(export 9th)
(def (9th collection) (ref 8 collection))
(def (9th collection <dict>) (ref 9 collection))


(export take)
(def (take n <integer> lst <list>)
  (cond
   [(>= (abs n) (size lst)) lst]
   [(zero? n) '()]
   [(negative? n) (drop (+ n (size lst)) lst)]
   [(empty? lst) '()]
   [else (srfi1:take lst n)]))

(def (take n <integer> vec <vector>)
  (cond
   [(>= (abs n) (size vec)) vec]
   [(zero? n) #()]
   [(negative? n) (drop (+ n (size vec)) vec)]
   [(empty? vec) #()]
   [else (vector-copy vec 0 n)]))

(def (take n <integer> str <string>)
  (cond
   [(>= (abs n) (size str)) str]
   [(zero? n) ""]
   [(negative? n) (drop (+ n (size str)) str)]
   [(empty? str) ""]
   [else (substring str 0 n)]))


(export drop)
(def (drop n <integer> lst <list>)
  (cond
   [(>= (abs n) (size lst)) '()]
   [(zero? n) lst]
   [(negative? n) (take (+ n (size lst)) lst)]
   [(empty? lst) lst]
   [else (srfi1:drop lst n)]))

(def (drop n <integer> vec <vector>)
  (cond
   [(>= (abs n) (size vec)) #()]
   [(zero? n) vec]
   [(negative? n) (take (+ n (size vec)) vec)]
   [(empty? vec) vec]
   [else (vector-copy vec n)]))

(def (drop n <integer> str <string>)
  (cond
   [(>= (abs n) (size str)) ""]
   [(zero? n) str]
   [(negative? n) (take (+ n (size str)) str)]
   [(empty? str) str]
   [(>= n (size str)) ""]
   [else (substring str n)]))


(export head)
(def (head lst <list>) (if (empty? lst) #f (car lst)))
(def (head vec <vector>) (if (empty? vec) #f (vector-ref vec 0)))
(def (head str <string>) (if (empty? str) #f (substring str 0 1)))

(export last)
(def (last lst <list>) (if (empty? lst) #f (srfi1:last lst)))
(def (last vec <vector>) (if (empty? vec) #f (vector-ref vec (- (size vec) 1))))
(def (last str <string>) (if (empty? str) #f (substring str (- (size str) 1))))

(export tail)
(def (tail collection ) (drop 1 collection))

(export init)
(def (init collection) (take (- (size collection) 1) collection))


(re-export :
           :integers
           :list
           :string
           :vector
           :range
           :real-range
           :char-range
           :port
           :parallel
           (:parallel . :par)
           :do
           :let
           :while
           :until
           :dispatched
           :generator-proc
           (do-ec . for)
           (list-ec . for-list)
           (vector-ec . for-vector)
           (string-append-ec . for-string)
           (sum-ec . for-sum)
           (product-ec . for-product)
           (min-ec . for-min)
           (max-ec . for-max)
           (any?-ec . for-any?)
           (every?-ec . for-all?)
           (fold-ec . for-fold)
           dispatch-union)
(export for-first
        for-last
        :consecutive)

(define-syntax for-first
  (syntax-rules ()
    [(_ (gen ...) rest ...)
     (first-ec #f (gen ...) rest ...)]
    [(_ rest ...)
     (first-ec rest ...)]))

(define-syntax for-last
  (syntax-rules ()
    [(_ (gen ...) rest ...)
     (last-ec #f (gen ...) rest ...)]
    [(_ rest ...)
     (last-ec rest ...)]))

(define-syntax :consecutive
  (syntax-rules ()
    [(_ cc v1 v2 coll)
     (:parallel cc
                (: v1 coll)
                (: v2 (tail coll)))]
    [(_ cc v1 v2 v3 coll)
     (:parallel cc
                (: v1 coll)
                (: v2 (tail coll))
                (: v3 (tail (tail coll))))]
    [(_ cc v1 v2 v3 v4 coll)
     (:parallel cc
                (: v1 coll)
                (: v2 (tail coll))
                (: v3 (tail (tail coll)))
                (: v4 (tail (tail (tail coll)))))]))


(export flat-map
        (generic-map . map)
        (generic-filter . filter)
        (generic-filter-out . filter-out)
        (generic-filter-map . filter-map))

(def (generic-map f <applicable> lst <list>)
  (map f lst))
(def (generic-map f <applicable> lst1 <list> lst2 <list>)
  (map f lst1 lst2))

(def (generic-map f <applicable> vec <vector>)
  (vector-map (λ (i e) (f e)) vec))
(def (generic-map f <applicable> vec1 <vector> vec2 <vector>)
  (vector-map (λ (i e1 e2) (f e1 e2)) vec1 vec2))

(def (generic-map f <applicable> dic <dict>)
  (define (acc k v rslt)
    (let ([nk nv (f k v)])
      (put! nk nv rslt)
      rslt))
  ;; TODO copy "equal?" and "hash" from the source dict
  (hash-table-fold dic acc (dict)))


(def (generic-map f <applicable> coll <string>)
  (apply str (map f (map string (string->list coll)))))
(def (generic-map f <applicable> str1 <string> str2 <string>)
  (apply str (map f (map string (string->list str1)) (map string (string->list str2)))))


(def (generic-filter f <applicable> lst <list>)
  (filter f lst))

(def (generic-filter f <applicable> vec <vector>)
  (list->vector (filter f (vector->list vec))))


(def (generic-filter-out f <applicable> lst <list>)
  (filter (negate f) lst))

(def (generic-filter-out f <applicable> vec <vector>)
  (list->vector (filter (negate f) (vector->list vec))))


(def (generic-filter-map f <applicable> lst <list>)
  (filter identity (map f lst)))

(def (generic-filter-map f <applicable> vec <vector>)
  (list->vector (filter identity (map f (vector->list vec)))))


;; Other utilities
(export cumulative-sum)
(def (cumulative-sum lst <list>)
  (let loop ([rslt '()]
             [current 0]
             [lst lst])
    (match lst
      [() (reverse rslt)]
      [(a rest ...) (loop (cons (+ a current) rslt)
                          (+ a current)
                          (cdr lst))])))
