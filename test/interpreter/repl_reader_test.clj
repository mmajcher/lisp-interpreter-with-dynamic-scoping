(ns interpreter.repl-reader-test
  (:require [clojure.test :refer :all]
            [interpreter.repl-reader :refer :all]))


(deftest repl-reader-test
  (testing "multiline-input"
    (is (= '(define x 3)
           (read-string
            (with-in-str "(define \n x \n 3)"
             (get-statement)))))))

