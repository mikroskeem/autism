(ns transformers.gamepack
  (:require [asm]
            [asm.util]
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
        (asm.util/inject-method
         add-message :head
         (asm.util/insns->
          (asm/install-fn-call #'gamepack-hooks/gamepack-chat->add-message
                               :method-desc (.-desc add-message)
                               :pop-result true
                               :static-context (asm/is-static? (.-access add-message))
                               :load-params 4))))

      (asm/class-visitor->bytes
       (asm/class-node-accept
        cn #(remapper/create-asm-remapping-adapter
             % current-mappings false))))

    ;; else
    class-bytes))
