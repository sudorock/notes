(ns notes.core
  (:require [clojure.string :as str]))

(defn wb-rgx-str
  "Custom word boundary like \\b but includes
   the supplied chars along with \\w.

   Note: \\b is short for (?:(?<!\\w)(?=\\w)|(?<=\\w)(?!\\w))"
  [chars]
  (if (empty? chars)
    "\\b"
    (let [w (str "[" "\\w" (apply str chars) "]")]
      (format "(?:(?<!%s)(?=%s)|(?<=%s)(?!%s))" w w w w))))


#_(defn near-words-rgx
    "Regex to match text that has the input
   words near each other. `leeway` is the
   maximum gap between the words and `alts`
   is a map of words to their alternatives,
   these alternatives will be or'ed."
    [words {:keys [leeway alts]}]
    (let [leeway         (or leeway 0)
          or-with-alts   (partial or-with-alts alts)
          fuzzy-word-gap (format "\\W*(?:\\w+\\W+){0,%s}?" leeway)
          prep-rgx-str   (->> words
                              (map or-with-alts)
                              (map #(str "(" % ")"))
                              (str/join fuzzy-word-gap))
          rgx-str        (str "(?i)" "\\b" prep-rgx-str "\\b")]
      (re-pattern rgx-str)))


(comment
 "socket repl server"
 (clojure.core.server/start-server {:port   9999
                                    :name   "socket-server"
                                    :accept 'clojure.core.server/repl}))
