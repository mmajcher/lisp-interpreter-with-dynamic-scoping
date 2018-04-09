(ns interpreter.repl-reader
  (require [clojure.string :as string]))


(defn get-statement []

  (defn closed-statement? [buffer]
    (defn how-many-occurs [buffer regex]
      (count (re-seq regex buffer)))

    (let [left-parens (how-many-occurs buffer #"\(")
          right-parens (how-many-occurs buffer #"\)")]
      (= right-parens left-parens)))


  (loop [input (read-line) buffer ""]
    (let [new-buffer (string/join " " [buffer input])]
      (if (closed-statement? new-buffer)
        new-buffer
        (recur (read-line) new-buffer)))))

