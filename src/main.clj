(ns main
  (:require [clojure.java.io :as io]
            [asm]
            [remapper]
            [transformer]
            [transformers.applet-viewer :as avt]
            [transformers.generic :as gt]
            [transformers.remapper :as rt]
            [util :refer [set-accessible start-thread]]))

(def runtime-debug-nrepl-available?
  (try
    (require 'nrepl.server)
    true
    (catch Exception e
      false)))

(def runtime-debug-cider-available?
  (try
    (require 'cider.nrepl)
    true
    (catch Exception e
      false)))

(defn- start-nrepl [port]
  (println "nrepl:" runtime-debug-nrepl-available?
           "cider:" runtime-debug-cider-available?)
  (if (and runtime-debug-nrepl-available? runtime-debug-cider-available?)
    (let [start-server-fn (ns-resolve 'nrepl.server 'start-server)
          cider-nrepl-handler-fn (ns-resolve 'cider.nrepl 'cider-nrepl-handler)]
      (println "Setting up NREPL on port :7888")
      (start-server-fn :init-ns 'zentria.emperor.core
                       :handler cider-nrepl-handler-fn
                       :port port))
    (println "Not starting NREPL because dependencies aren't present")))

(defn -main [& args]
  ;; Set up system properties
  (System/setProperty "user.home" "cache")
  (System/setProperty "sun.awt.noerasebackground" "true")
  (System/setProperty "com.jagex.configuri" "jagex-jav://oldschool.runescape.com/jav_config.ws")

  ;; Preload some classes
  ;; https://github.com/runelite/runelite/blob/master/runelite-client/src/main/java/net/runelite/client/ClassPreloader.java
  (java.time.ZoneId/of "Europe/London") ;; Timezone database
  (-> java.time.format.DateTimeFormatter/BASIC_ISO_DATE .toString) ;; Slow 20 constructors
  
  ;; Load Jagex applet viewer into replacement system classloader
  (let [av-url (-> (java.io.File. "jagexappletviewer.jar")
                   .toURI
                   .toURL)
        applet-mappings (remapper/load-mappings "./av_mappings.v2")
        sys-cl (ClassLoader/getSystemClassLoader)
        new-cl (transformer/new-transformer-classloader sys-cl
                                                        [av-url] [#'rt/transformer
                                                                  #'avt/transformer]
                                                        applet-mappings)
        scl-field (try (-> (.getDeclaredField java.lang.ClassLoader "scl")
                           set-accessible)
                       (catch Exception e
                         (.printStackTrace e)
                         nil))]
    (.addURL new-cl av-url)
    (when (some? scl-field)
      (println "Replacing system classloader")
      (.set scl-field nil new-cl)
      #_(start-thread
         "SCL watcher"
         (fn []
           (loop []
             (println "scl =" (.get scl-field nil) "new-cl =" new-cl)
             (Thread/sleep 2500)
             (recur))))
      )

    ;; Set up nrepl
    (start-nrepl 7888)

    ;; Invoke entry point
    (let [current-dir (-> (java.io.File. ".")
                          .getAbsoluteFile
                          .getParentFile
                          .getName)
          cl (.loadClass (ClassLoader/getSystemClassLoader) "jagexappletviewer")
          main-method (.getMethod cl "main" (into-array Class [(class (into-array String '()))]))
          final-args (into-array String (if (empty? args)
                                          [current-dir]
                                          args))]
      (.invoke main-method
               nil 
               (into-array Object
                           [final-args])))))
