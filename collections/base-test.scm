(define-module (syntax base-test)
  use-module: (syntax base)
  use-module: (collections base)
  use-module: (srfi srfi-64)) ;; Tests

(test-begin "size")
(test-equal 0 (size '()))
(test-equal 0 (size #()))
(test-equal 0 (size (dict)))
(test-equal 0 (size ""))

(test-equal 1 (size '("a")))
(test-equal 1 (size #("a")))
(test-equal 1 (size (dict "a" 1)))
(test-equal 1 (size "a"))
(test-end)

(test-begin "empty?")
(test-equal #t (empty? '()))
(test-equal #t (empty? #()))
(test-equal #t (empty? (dict)))
(test-equal #t (empty? ""))

(test-equal #f (empty? '("a")))
(test-equal #f (empty? #("a")))
(test-equal #f (empty? (dict "a" 1)))
(test-equal #f (empty? "a"))
(test-end)


(test-begin "ref")
(test-equal "a" (ref 0 '("a")))
(test-equal "a" (ref 0 #("a")))
(test-equal "a" (ref "b" (dict "b" "a")))
(test-equal "a" (ref 0 "a"))

(test-equal #f (ref 1 '("a")))
(test-equal #f (ref 1 #("a")))
(test-equal #f (ref 1 "a"))

(test-equal #f (ref 9 '("a")))
(test-equal #f (ref 9 #("a")))
(test-equal #f (ref "z" (dict "b" "a")))
(test-equal #f (ref 9 "a"))

(test-equal "x" (ref 9 '("a") default: "x"))
(test-equal "x" (ref 9 #("a") default: "x"))
(test-equal "x" (ref "z" (dict "b" "a") default: "x"))
(test-equal "x" (ref 9 "a" default: "x"))
(test-end)


(test-begin "ref-shortcuts")
(def (test-all expects applies sources)
  (for (:parallel
        (: e expects)
        (: a applies)
        (: s sources))
    (test-equal e (a s))))

(test-all '("a" "b" "c" "d" "e" "f" "g" "h" "i")
          (list 1st 2nd 3rd 4th 5th 6th 7th 8th 9th)
          (list '("a" "b" "c" "d" "e" "f" "g" "h" "i")
                #("a" "b" "c" "d" "e" "f" "g" "h" "i")
                (dict 1 "a" 2 "b" 3 "c" 4 "d" 5 "e" 6 "f" 7 "g" 8 "h" 9 "i")
                "abcdefghi"))

(test-end)


(test-begin "take")
(test-equal '() (take 1 '()))
(test-equal #() (take 1 #()))
(test-equal "" (take 1 ""))

(test-equal '() (take 0 '(1 2 3)))
(test-equal #() (take 0 #(1 2 3)))
(test-equal "" (take 0 "abc"))

(test-equal '(1) (take 1 '(1 2 3)))
(test-equal #(1) (take 1 #(1 2 3)))
(test-equal "a" (take 1 "abc"))

(test-equal '(1 2) (take 2 '(1 2 3)))
(test-equal #(1 2) (take 2 #(1 2 3)))
(test-equal "ab" (take 2 "abc"))

(test-equal '(1 2 3) (take 99 '(1 2 3)))
(test-equal #(1 2 3) (take 99 #(1 2 3)))
(test-equal "abc" (take 99 "abc"))

(test-equal '(3) (take -1 '(1 2 3)))
(test-equal #(3) (take -1 #(1 2 3)))
(test-equal "c" (take -1 "abc"))

(test-equal '(2 3) (take -2 '(1 2 3)))
(test-equal #(2 3) (take -2 #(1 2 3)))
(test-equal "bc" (take -2 "abc"))

(test-equal '(1 2 3) (take -99 '(1 2 3)))
(test-equal #(1 2 3) (take -99 #(1 2 3)))
(test-equal "abc" (take -99 "abc"))
(test-end)


(test-begin "drop")
(test-equal '() (drop 1 '()))
(test-equal #() (drop 1 #()))
(test-equal "" (drop 1 ""))

(test-equal '(1 2 3) (drop 0 '(1 2 3)))
(test-equal #(1 2 3) (drop 0 #(1 2 3)))
(test-equal "abc" (drop 0 "abc"))

(test-equal '(2 3) (drop 1 '(1 2 3)))
(test-equal #(2 3) (drop 1 #(1 2 3)))
(test-equal "bc" (drop 1 "abc"))

(test-equal '(3) (drop 2 '(1 2 3)))
(test-equal #(3) (drop 2 #(1 2 3)))
(test-equal "c" (drop 2 "abc"))

(test-equal '() (drop 99 '(1 2 3)))
(test-equal #() (drop 99 #(1 2 3)))
(test-equal "" (drop 99 "abc"))

(test-equal '(1 2) (drop -1 '(1 2 3)))
(test-equal #(1 2) (drop -1 #(1 2 3)))
(test-equal "ab" (drop -1 "abc"))

(test-equal '(1) (drop -2 '(1 2 3)))
(test-equal #(1) (drop -2 #(1 2 3)))
(test-equal "a" (drop -2 "abc"))

(test-equal '() (drop -99 '(1 2 3)))
(test-equal #() (drop -99 #(1 2 3)))
(test-equal "" (drop -99 "abc"))
(test-end)


(test-begin "head")
(test-equal #f (head '()))
(test-equal #f (head #()))
(test-equal #f (head ""))

(test-equal 1 (head '(1 2 3)))
(test-equal 1 (head #(1 2 3)))
(test-equal "a" (head "abc"))
(test-end)


(test-begin "last")
(test-equal #f (last '()))
(test-equal #f (last #()))
(test-equal #f (last ""))

(test-equal 3 (last '(1 2 3)))
(test-equal 3 (last #(1 2 3)))
(test-equal "c" (last "abc"))
(test-end)


(test-begin "tail")
(test-equal '() (tail '()))
(test-equal #() (tail #()))
(test-equal "" (tail ""))

(test-equal '() (tail '(1)))
(test-equal #() (tail #(1)))
(test-equal "" (tail "a"))

(test-equal '(2 3) (tail '(1 2 3)))
(test-equal #(2 3) (tail #(1 2 3)))
(test-equal "bc" (tail "abc"))
(test-end)


(test-begin "init")
(test-equal '() (init '()))
(test-equal #() (init #()))
(test-equal "" (init ""))

(test-equal '() (init '(1)))
(test-equal #() (init #(1)))
(test-equal "" (init "a"))

(test-equal '(1 2) (init '(1 2 3)))
(test-equal #(1 2) (init #(1 2 3)))
(test-equal "ab" (init "abc"))
(test-end)


(test-begin "list-comprehension")
(test-equal '(1 2 3) (for-list (: a 1 4) a))
(test-equal #(1 2 3) (for-vector (: a 1 4) a))
(test-equal "123" (for-string (: a 1 4) (number->string a)))
(test-equal 6 (for-sum (: a 1 4) a))
(test-equal 24 (for-product (: a 2 5) a))
(test-equal 1 (for-min (: a 1 4) a))
(test-equal 3 (for-max (: a 1 4) a))
(test-equal #t (for-any? (: a 1 4) (odd? a)))
(test-equal #f (for-any? (: a 1 4) (> a 3)))
(test-equal #f (for-every? (: a 1 4) (odd? a)))
(test-equal #t (for-every? (: a 1 4) (<= a 3)))
(test-equal 2 (for-first 100 (: a 1 4) (if (even? a)) a))
(test-equal 2 (for-first (: a 1 4) (if (even? a)) a))
(test-equal 100 (for-first 100 (: a 1 4) (if (> a 10)) a))
(test-equal #f (for-first (: a 1 4) (if (> a 10)) a))
(test-equal 3 (for-last 100 (: a 1 4) (if (odd? a)) a))
(test-equal 3 (for-last (: a 1 4) (if (odd? a)) a))
(test-equal 100 (for-last 100 (: a 1 4) (if (> a 10)) a))
(test-equal #f (for-last (: a 1 4) (if (> a 10)) a))
(test-equal '(3 2 1) (for-fold '() (: a 1 4) a cons))
(test-equal 6 (for-fold 0 (: a 1 4) a +))
(test-end)
