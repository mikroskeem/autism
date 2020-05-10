(ns transformers.remapper
  (:require [asm]
            [remapper]))

(defn transformer [{:keys [class-bytes current-mappings]}]
  (let [cn (asm/new-class-node)
        _ (-> (asm/new-class-reader class-bytes)
              (asm/class-reader->visitor
               (remapper/create-asm-remapping-adapter cn current-mappings)))]
    (asm/class-visitor->bytes cn)))
