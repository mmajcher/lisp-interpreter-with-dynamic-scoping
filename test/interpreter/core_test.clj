(ns interpreter.core-test
  (:require [clojure.test :refer :all]
            [interpreter.core :refer :all]))


(defmacro eval-my-language [& forms]
  "This macro allows me to use my own language inside Clojure.

It turns given forms into strings, joins them and passes to high-level
intepret function."
  (let [forms (clojure.string/join " " (map str forms))
        clean-env (get-clean-env)]
    (interpret-multiple-statements forms clean-env)))


(deftest interpreter-test
  (testing "define"
    (is (= "3"
           (eval-my-language
            (define x 3)
            x
            ))))
  (testing "primitive-proc"
    (is (= "9"
           (eval-my-language
            (* 3 3)
            )))
    (is (= "8"
           (eval-my-language
            (- 10 2)
            )))
    (is (= "20"
           (eval-my-language
            (+ 12 8)
            ))))
  (testing "compound-proc"
    (is (= "9"
           (eval-my-language
            (define square (lambda (x) (* x x)))
            (square 3)
            ))))
  (testing "let"
    (is (= "5"
           (eval-my-language
            (let ((x 5))
              x)
            ))))
  (testing "if"
    (is (= "7"
           (eval-my-language
            (if (> 200 100)
              7
              666)
            )))
    (is (= "7"
           (eval-my-language
            (if (= 5 4)
              666
              7)
            ))))
  (testing "dynamic scoping"
    (is (= "7"
           (eval-my-language
            (define x 666)
            (define get-x (lambda () x))
            (let ((x 7))
              (get-x))
            )))))

