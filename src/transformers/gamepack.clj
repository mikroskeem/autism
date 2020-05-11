(ns transformers.gamepack
  (:require [asm]
            [clojure.java.io :as io]
            [gamepack-hooks]
            [remapper]))

(defn transformer [{:keys [class-bytes mapped-name current-mappings]}]
  (condp = mapped-name
    "net.runescape.oldschool.ChatChannel"
    (let [cn (asm/new-class-node)
          _ (-> (asm/new-class-reader class-bytes)
                (asm/class-reader->visitor
                 (remapper/create-asm-remapping-adapter cn current-mappings true)))
          methods (.-methods cn)]

      (when-let [add-message (first (filter #(= (.-name %) "addChatMessage") methods))]
        (.insert (.-instructions add-message)
                 (doto (asm/new-insnlist)
                   (asm/install-fn-call #'gamepack-hooks/gamepack-chat->add-message
                                        :pop-result true
                                        :override-arg-count 4
                                        ;; Parameters shift by one for static methods
                                        :pre-call-insns [(asm/load-var 0 :int)
                                                         (asm/box-primitive :int)
                                                         (asm/load-var 1)
                                                         (asm/load-var 2)
                                                         (asm/load-var 3)]))))

      (asm/class-visitor->bytes
       (asm/class-node-accept
        cn #(remapper/create-asm-remapping-adapter
             % current-mappings false))))

    ;; else
    class-bytes))
