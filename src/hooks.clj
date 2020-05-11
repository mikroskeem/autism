(ns hooks
  (:require [remapper]
            [transformers.gamepack :as gpt]
            [transformer]
            [util]))

(def gamepack-cl (atom {}))

(def gamepack-signature-verifier->get-verified-resource->hooked!
  (memoize (fn [self]
             (println "Hooked into gamepack signature verifier, it's showtime! cl ="
                      (:cl @gamepack-cl))
             (let [cl (class self)
                   field (-> (.getDeclaredField cl "classBytes")
                             (util/set-accessible))
                   hashtable-ref (.get field self)

                   m (do
                       (println "Loading mappings")
                       (remapper/load-mappings "./gamepack.v2"))
                   name-remapper (remapper/create-class-name-mapper m)]
               (swap! gamepack-cl assoc
                      :mappings m
                      :name-remapper name-remapper
                      :classes hashtable-ref
                      :loaded-class-names #{})
               (println "Done!")))))

(def gamepack-class-loader->ctor->hooked!
  (memoize (fn [self]
             (println "Hooked into gamepack classloader, cl =" self)
             (swap! gamepack-cl assoc :cl self))))

(defn gamepack-signature-verifier->get-verified-resource [self resource-name]
  (try
    (if (and (not (clojure.string/starts-with? resource-name "java."))
             (clojure.string/ends-with? resource-name ".class"))
      (let [class-name (subs resource-name ;; subs 6 ".class"
                             0 (- (count resource-name) 6))
            unmapped-name (remapper/unmap-class-name (:name-remapper @gamepack-cl)
                                                     class-name)
            mapped-name (if (= class-name unmapped-name)
                          (remapper/map-class-name (:name-remapper @gamepack-cl)
                                                   class-name)
                          class-name)

            class-bytes (let [hashtable (:classes @gamepack-cl)]
                          (.remove hashtable (str unmapped-name ".class")))]

        (when (some? class-bytes)
          (swap! gamepack-cl update-in [:loaded-class-names] conj mapped-name)
          (#'gpt/transformer {:class-bytes class-bytes
                              :name unmapped-name
                              :mapped-name mapped-name
                              :current-mappings (:mappings @gamepack-cl)}))))
    (catch Exception e
      (.printStackTrace e)
      (System/exit 1)
      nil)))
