(ns gamepack-hooks
  (:require [asm]
            [transformer]
            [util]))

(defn gamepack-chat->add-message [integer sender-name
                                  message-contents string5]
  (let [message-info
        {:type (if (empty? sender-name)
                 :system
                 :user)
         :sender (if (empty? sender-name) "SYSTEM" sender-name)
         :message message-contents}]
    (println "msg =" message-info ", string5 =" string5 ", integer =" integer))


  #_(try
      (when-let [m (some->> (Class/forName "eb" false
                                           (-> @@#'hooks/gamepack-cl :cl)
                                           #_(-> (Thread/currentThread)
                                                 (.getContextClassLoader)))
                            (.getDeclaredMethods)
                            (filter (fn [m]
                                      (println m)
                                      (= (.getName m) "m")))
                            first
                            util/set-accessible)]
        
        (println "method =" m)
        #_(let [args (into-array Object [(int 30) "" "->> test" (int 55555)])]
            (.invoke m nil args)))
      (catch Exception e
        (.printStackTrace e)))
  )
