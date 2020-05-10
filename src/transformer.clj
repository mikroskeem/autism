(ns transformer
  (:require [transformer]
            [remapper]
            [util :refer [stream->bytes]]))

(def ^:dynamic *current-mappings*)
(def ^:dynamic *current-name-remapper*)

(defn run-through-transformers [transformers ctx]
  (loop [transformers (seq transformers)
         class-bytes (:class-bytes ctx)]
    (if-let [transformer (first transformers)]
      (recur (next transformers) (transformer (assoc ctx :class-bytes class-bytes)))
      class-bytes)))

(defn get-class-resource [cl class-name]
  (.getResourceAsStream cl (str (clojure.string/replace class-name "." "/") ".class"))  )

(defn find-class-resource [cl class-name mappings]
  (let [unmapped (remapper/unmap-class-name mappings class-name)]
    (if-let [stream (get-class-resource cl unmapped)]
      (with-open [s stream]
        {:mapped-lookup (not (= class-name unmapped))
         :name unmapped
         :class-bytes (stream->bytes s)}))))

(defn new-transformer-classloader [parent urls transformers mappings]
  (let [name-remapper (some-> mappings remapper/create-class-name-mapper)
        cl (proxy [clojure.lang.DynamicClassLoader];;java.net.URLClassLoader]
               [;;(into-array java.net.URL urls)
                parent]
             (findClass [class-name]
               (let [{:keys [name mapped-lookup class-bytes]} (find-class-resource this class-name name-remapper)]
                 (if (some? class-bytes)
                   (let [mapped-name (if (some? name-remapper) (remapper/map-class-name name-remapper name) name)
                         ctx {:current-mappings mappings
                              :current-name-remapper name-remapper
                              :name name
                              :mapped-name mapped-name
                              :class-bytes class-bytes}
                         transformed-bytes (try
                                             (run-through-transformers transformers ctx)
                                             (catch Exception e
                                               (println (str "WARN: failed to transform '" name "' ('" mapped-name "')"))
                                               (.printStackTrace e)
                                               class-bytes))]
                     ;; (.defineClass this name transformed-bytes 0 (count transformed-bytes))
                     (.defineClass this class-name transformed-bytes nil))
                   (proxy-super findClass class-name)))))]

    ;; Add URLS
    (doseq [url urls]
      (.addURL cl url))

    cl))
