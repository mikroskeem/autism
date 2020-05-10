(ns remapper
  (:require [clojure.java.io :as io])
  (:import (net.fabricmc.mapping.tree TinyMappingFactory)
           (net.fabricmc.mapping.util AsmRemapperFactory)
           (net.fabricmc.mapping.util ClassMapper)
           (net.fabricmc.mappings.model V2MappingsProvider)
           (org.objectweb.asm.commons ClassRemapper)))

(defn load-mappings [mappings-file]
  (let [f (io/file mappings-file)]
    {:tree
     (with-open [r (-> (io/input-stream f)
                       (java.io.InputStreamReader.)
                       (java.io.BufferedReader.))]
       (TinyMappingFactory/load r))
     :mappings
     (with-open [r (-> (io/input-stream f)
                       (java.io.InputStreamReader.)
                       (java.io.BufferedReader.))]
       (V2MappingsProvider/readTinyMappings r))}))

(defn create-class-name-mapper [mappings]
  (loop [old-to-new {}
         new-to-old {}
         class-entries (seq (-> mappings :mappings .getClassEntries))]
    (if-let [class-entry (first class-entries)]
      (let [f (clojure.string/replace
               (.get class-entry "intermediary") "/" ".")
            t (clojure.string/replace
               (.get class-entry "named") "/" ".")]
        (recur (assoc old-to-new f t)
               (assoc new-to-old t f)
               (next class-entries)))
      {:a (ClassMapper. old-to-new)
       :b (ClassMapper. new-to-old)})))

(defn map-class-name [mapper old]
  (.mapClass (:a mapper) old))

(defn unmap-class-name [mapper new]
  (.mapClass (:b mapper) new))

(defn create-asm-remapping-adapter [cv mappings]
  (ClassRemapper.
   cv
   (.getRemapper
    (-> mappings :tree AsmRemapperFactory.)
    "intermediary" "named")))
