(ns transformers.generic)

(defn transformer [{:keys [name mapped-name class-bytes]}]
  (if-not (clojure.string/starts-with? name "java.")
    (println "seen class =" name "(" mapped-name ")"))
  class-bytes)
