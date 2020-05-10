(ns util
  (:require [clojure.java.io :as io]))

(defn start-thread [name f]
  (-> (Thread. f name)
      .start))

(defn set-accessible [thing]
  (.setAccessible thing true)
  thing)

(defn stream->bytes [is]
  (let [baos (java.io.ByteArrayOutputStream.)]
    (io/copy is baos)
    (.toByteArray baos)))
