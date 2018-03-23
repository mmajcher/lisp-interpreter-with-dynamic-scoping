(ns interpreter.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!")
  (println "got " (count args) "args")
  (println "those are: " args))

(remove-method print-method clojure.lang.IDeref)

(defn parse [raw]
  (read-string raw))


(defn err [msg]
  (throw (Exception. msg)))

(defn find-variable [env var]
  (cond
    (contains? @env (keyword var)) (get @env (keyword var))
    (contains? @env :next-env) (find-variable (:next-env @env) var)
    :else (throw (Exception. (str "variable " var " not defined")))
    ))


(defn create-frame [next-env]
  (let [env (atom {})]
    (if next-env
      (swap! env assoc :next-env next-env))
    env))

(defn make-procedure [params-list proc-body env]
  {:procedure true
   :compound true
   :params params-list
   :body proc-body
   :env env})

(defn define-variable [name env value]
  (swap! env assoc (keyword name) value))

(def test-global-env
  (atom {:x 3
         :+ {:procedure true :primitive true :proc +}}))

(defn exp-type [exp]
  (cond (list? exp)
        (condp = (first exp)
          'define 'definition
          'lambda 'make-lambda
          'application)
        (symbol? exp) 'variable
        (number? exp) 'number
        :else 'uknown-exp-type))

(declare my-apply)

(defn my-eval [exp env]
  (let [type (exp-type exp)]
    (condp = type

      'variable (find-variable env exp)

      'number exp

      'definition
      (let* [name (second exp)
             body (nth exp 2)
             value (my-eval body env)
             ]
        (define-variable name env value)
        'defined)

      'make-lambda
      (let [args-list (second exp)
            proc-body (nth exp 2)]
        (make-procedure args-list proc-body env))

      'application
      (let* [procedure (my-eval (first exp) env)
             args-exprs (rest exp)
             args-vals (map #(my-eval % env) args-exprs)]
        (my-apply procedure args-vals))

      (err (str "uknown exp type: " exp ", " type)))))


(defn my-apply [procedure args]

  (if (not (contains? procedure :procedure))
    (err ("application: not a procedure: " procedure)))

  (cond
    (contains? procedure :primitive)
    (apply (:proc procedure) args)

    (contains? procedure :compound)
    (let* [proc-params (:params procedure)
           proc-body (:body procedure)
           proc-env (:env procedure)
           apply-env (create-frame proc-env)]
      (if (not (= (count proc-params) (count args)))
        (err "wrong number of arguments: " args " ;; params: " proc-params))
      (doall (map (fn [param arg]
                    (define-variable param apply-env arg))
                  proc-params
                  args))
      (my-eval proc-body apply-env))))



(defn my-eval-wrap [& args]
  "Avoid printing return value.

When my-eval returns a map, it might contain circular references. REPL
will overflow the stack during an attempt to print that map.

E.g.:
(define proc (x) (+ 2 x)) in global-env results in:
{... :proc-body (+ 2 x) ... :proc-env global-env}

REPL will attempt to print contents of global-env. After definition it
contains a reference to proc, which in turns contains a reference to
global-env...
"
  (let [result (apply my-eval args)]
    (if (not (map? result))
      result
      'ok)))

(my-eval-wrap (parse "(+ 2 3)") test-global-env)
(my-eval-wrap (parse "(define x 6)") test-global-env)
(my-eval-wrap (parse "(lambda (x y) (+ x y))") test-global-env)
(my-eval-wrap (parse "(define my-test (+ 3 4))") test-global-env)
(my-eval (parse "(define my-proc (lambda (x y) (+ x y)))") test-global-env)
(my-eval (parse "(my-proc 3 4)") test-global-env)

test-global-env
