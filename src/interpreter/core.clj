(ns interpreter.core
  (:gen-class))

(declare my-eval-wrap)
(declare global-env)
(declare interpret)

(defn -main
  [& args]

  (println "READY ...")
  (doseq [line (line-seq (java.io.BufferedReader. *in*))]
    (println "got:" line)
    (println "result: " (interpret line global-env))))

(defn parse [raw]
  (read-string raw))

(defn interpret [some-str env]
  (try
    (let [parsed (parse some-str)]
      (str (my-eval-wrap parsed env)))
    (catch Exception e
      (let [] (println (.getMessage e))))))

(defn err [msg]
  (throw (Exception. msg)))


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

(defn my-eval-wrap [& args]
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
  (let [result (apply my-eval args)]
    (if (not (map? result))
      result
      'ok)))

