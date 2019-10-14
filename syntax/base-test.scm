(define-module (syntax base-test)
  use-module: (syntax base)
  use-module: (srfi srfi-64)) ;; Tests

(test-begin "def")
(def (my-add1 n) (+ 1 n))
(test-equal "simple def" 99 (my-add1 98))

(def (my-first (lst <list>)) (car lst))
(def (my-first (str <string>)) (string-ref str 0))
(test-equal "multimethod-list" 1 (my-first '(1 2 3)))
(test-equal "multimethod-string" #\a (my-first "abc"))

(def (my-plus a b) (+ a b))
(def (my-plus (a <string>) (b <string>)) (string-append a b))
(test-equal "multimethod-top-int" 3 (my-plus 1 2))
(test-equal "multimethod-top-float" 3.0 (my-plus 1.0 2.0))
(test-equal "multimethod-specialized" "xy" (my-plus "x" "y"))

(def (my-guard a)
  (return "less than 5" if (< a 5))
  "greater or equal than 5")
(test-equal "simple-guard-apply" "less than 5" (my-guard 4))
(test-equal "simple-guard-not-apply" "greater or equal than 5" (my-guard 5))

(def (optional-append (a <string>) default: "xyz")
  (string-append a default))
(test-equal "abcxyz" (optional-append "abc"))
(test-equal "abczyx" (optional-append "abc" default: "zyx"))

(def (optional-syntax a <string> b c <number> arg1: "a" arg2: "b")
  (string-append a b (number->string c) arg1 arg2))
(test-equal "aaa12ab" (optional-syntax "aaa" "1" 2))
(test-equal "aaa23cccddd" (optional-syntax "aaa" "2" 3 arg1: "ccc" arg2: "ddd"))

(def (outer-def a <integer>)
  (def (inner-def b)
    (add1 b))
  (inner-def (add1 a)))
(test-equal 3 (outer-def 1))
(test-end)

(test-begin "var")
(var [a 1] (test-equal 1 a))
(var [a (- 2 1) b 2]
  (test-equal 1 a)
  (test-equal 2 b))
(var [x 9])
(test-equal 9 x)
(test-end)

(test-begin "cut")
(let [(x (Λ + <> 2))] (test-equal 3 (x 1)))
(let [(x (Λ + <> <>))] (test-equal 5 (x 2 3)))
(test-end)

(test-begin "add1")
(test-equal 2 (add1 1))
(test-equal 3 (add1 2))
(test-end)

(test-begin "sub1")
(test-equal 2 (sub1 3))
(test-equal 3 (sub1 4))
(test-end)
