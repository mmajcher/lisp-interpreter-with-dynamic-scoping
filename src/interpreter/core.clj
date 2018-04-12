(ns interpreter.core
  (:gen-class)
  (require [interpreter.repl-reader :as repl-reader]))

(declare my-eval-statements-seq-wrap)
(declare global-env)
(declare interpret)

(defn -main
  [& args]

  (println "READY ...")

  (defn should-read-from-file? []
    (> (count args) 0))

  (defn read-from-file-filename []
    (first args))

  (if (should-read-from-file?)
    ;; file
    (let [statements (slurp (read-from-file-filename))]
      (let [result (interpret statements global-env)]
        (println "result: " result)))
    ;; repl
    (while true
      (let [stmt (repl-reader/get-statement)]
        (println "got:" stmt)
        (println "result: " (interpret stmt global-env))))))

(defn err [msg]
  (throw (Exception. msg)))


;; high-level functions (meant to work string -> string)

(defn interpret [some-str env]
  (defn parse [raw-str]
    (read-string raw-str))
  (defn parse-possibly-multiple-statements [raw-str]
    "Interpreted string (possibly sequence of statements) is being
      wrapped in parens, because parse (read-string) only reads one
      form (list of statements in this case)."
    (parse (str \( " " some-str " " \) )))
  (try
    (let [parsed-statements (parse-possibly-multiple-statements some-str)]
      (str (my-eval-statements-seq-wrap parsed-statements env)))
    (catch Exception e
      (let [] (println (.getMessage e))))))

;; variables

(defn find-variable [env var]
  (cond
    (contains? @env (keyword var)) (get @env (keyword var))
    (contains? @env :next-env) (find-variable (:next-env @env) var)
    :else (throw (Exception. (str "variable " var " not defined")))))

(defn define-variable [name env value]
  (swap! env assoc (keyword name) value))

;; procedures

(defn make-procedure [params-list proc-body env]
  {:procedure true
   :compound true
   :params params-list
   :body proc-body
   :env env})

(defn make-primitive-procedure [proc]
  {:procedure true
   :primitive true
   :proc proc})

(defn register-primitive-procedure [proc-symbol env]
  (let [var-name (keyword proc-symbol)
        procedure (make-primitive-procedure (eval proc-symbol))]
    (define-variable var-name env procedure)))

;; frames, environments

(defn create-frame-pointing-to [next-env]
  (let [env (atom {})]
    (if next-env
      (swap! env assoc :next-env next-env))
    env))

(defn get-clean-env []
  (let [env (create-frame-pointing-to nil)]
    (doall (map #(register-primitive-procedure % env) '(+ - * /)))
    (doall (map #(register-primitive-procedure % env) '( = not > < >= <=)))
    env))

(def global-env (get-clean-env))

;; expressions

(defn exp-type [exp]
  (cond (and (list? exp) (empty? exp)) 'empty-list
        (list? exp)
        (condp = (first exp)
          'define 'definition
          'lambda 'make-lambda
          'let 'let
          'unquote 'unquoting
          'quote 'quoting
          'if 'if
          'application)
        (symbol? exp) 'variable
        (number? exp) 'number
        (= (first exp) 'quote) 'quote
        (string? exp) 'string
        :else 'uknown-exp-type))

;; INTERPRETER

(declare my-apply)
(declare my-eval)

(defn my-eval-statements-seq [statements env]
  (last
   (doall (map #(my-eval % env) statements))))

(defn my-eval [exp env]
  (let [type (exp-type exp)]
    (condp = type
      'variable (find-variable env exp)
      'number exp
      'quote (rest exp)

      'quoting (rest exp)

      'unquoting
      (let* [quote-cell (second exp)
             quoted-expr (second quote-cell)]
        (my-eval quoted-expr env))

      'if
      (let [[_ pred-expr true-expr false-expr] exp]
        (if (my-eval pred-expr env)
          (my-eval true-expr env)
          (my-eval false-expr env)))

      'definition
      (let [[_ name expr] exp]
        (define-variable name env (my-eval expr env))
        'defined)

      'make-lambda
      (let [[_ args-list proc-body] exp]
        (make-procedure args-list proc-body env))

      'let
      (let [[_ pairs let-body] exp]
        (let* [params (map first pairs)
               args (map second pairs)
               args-vals (map #(my-eval % env) args)
               let-procedure (make-procedure params let-body env)]
          (my-apply let-procedure args-vals env)))

      'application
      (let [[proc-expr & args-exprs] exp]
        (let [procedure (my-eval proc-expr env)
              args-vals (map #(my-eval % env) args-exprs)]
          (my-apply procedure args-vals env)))

      (err (str "uknown exp type: " exp ", " type)))))


(defn my-apply [procedure args caller-env]
  (if (not (contains? procedure :procedure))
    (err ("application: not a procedure: " procedure)))

  (cond
    (contains? procedure :primitive)
    (apply (:proc procedure) args)

    (contains? procedure :compound)
    (let [{proc-params :params
           proc-body :body
           proc-env :env} procedure
          apply-env (create-frame-pointing-to caller-env)]
      (if (not (= (count proc-params) (count args)))
        (err "wrong number of arguments: " args " ;; params: " proc-params))
      (doall (map (fn [param arg]
                    (define-variable param apply-env arg))
                  proc-params
                  args))
      (my-eval proc-body apply-env))))


;; INTERFACE

(defn my-eval-statements-seq-wrap [statements-seq env]
  "Avoid printing return value when possibly harmful.

When my-eval returns an environment map, it might contain circular
references. REPL will overflow the stack during an attempt to print
that map.

E.g.:
(define proc (lambda (x) (+ 2 x))) in global-env results in:
{... :proc-body (+ 2 x) ... :proc-env global-env}

global-env now has a reference to proc and proc references global-env.
We'd better not try to print that.
"
  (let [result (my-eval-statements-seq statements-seq env)]
    (if (not (map? result))
      result
      'ok)))

