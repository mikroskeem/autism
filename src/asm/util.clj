(ns asm.util
  (:require [asm]))

(defn inject-method
  ([method at insns] (inject-method method at :before insns))
  ([method at pos insns]
   (let [method-insns (.-instructions method)
         injection-point (if (keyword? at)
                           (condp = at
                             :head (.getFirst method-insns)
                             :tail (.getLast method-insns))
                           at)]
     (condp = pos
       :after (.insert method-insns injection-point insns)
       :before (.insertBefore method-insns injection-point insns)))))

(defmacro insns-> [& body]
  `(doto (asm/new-insnlist)
     ~@body))
