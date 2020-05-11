(ns transformers.applet-viewer
  (:require [asm]
            [hooks]))

(defn transformer [{:keys [name mapped-name class-bytes current-mappings]}]
  (if-let [transformed
           (cond
             ;; The entry point
             (= mapped-name "jagexappletviewer")
             (let [cn (-> (asm/new-class-reader class-bytes)
                          (asm/class-reader->visitor (asm/new-class-node)))
                   methods (.-methods cn)]

               ;; Don't try to load dumb shit on OSX
               (when-let [cl-init (some-> (first (filter #(= "<clinit>" (.-name %)) methods)) .-instructions)]
                 (loop [iter (.iterator cl-init)
                        insn (if (.hasNext iter) (.next iter))]
                   (when (some? insn)

                     ;; No dumb stuff please
                     (if (and (instance? org.objectweb.asm.tree.LdcInsnNode insn) (= "mac" (.-cst insn)))
                       (.set cl-init insn (asm/ldc "#######")))

                     (recur iter (if (.hasNext iter) (.next iter))))))

               (asm/class-visitor->bytes cn))

             (= mapped-name "app.GamepackClassLoader")
             (let [cn (-> (asm/new-class-reader class-bytes)
                          (asm/class-reader->visitor (asm/new-class-node)))
                   methods (.-methods cn)]

               (when-let [ctor (first (filter #(= (.-name %1) "<init>") methods))]
                 (.insert (.-instructions ctor)
                          (first (filter
                                  #(and (instance? org.objectweb.asm.tree.MethodInsnNode %1)
                                        (= (.getOpcode %1) org.objectweb.asm.Opcodes/INVOKESPECIAL)
                                        (= (.-owner %1) "java/lang/ClassLoader")
                                        (= (.-name %1) "<init>"))
                                  (.-instructions ctor)))
                          (doto (asm/new-insnlist)
                            (asm/install-fn-call #'hooks/gamepack-class-loader->ctor->hooked!
                                                 :load-this true
                                                 :pop-result true)
                            )))


               (asm/class-visitor->bytes cn))

             (= mapped-name "app.GamepackSignatureVerifier")
             (let [cn (-> (asm/new-class-reader class-bytes)
                          (asm/class-reader->visitor (asm/new-class-node)))
                   methods (.-methods cn)]

               (when-let [get-verified-resource (first (filter #(= (.-name %1) "getVerifiedResource") methods))]
                 (.insert (.-instructions get-verified-resource)
                          (doto (asm/new-insnlist)
                            (asm/install-fn-call #'hooks/gamepack-signature-verifier->get-verified-resource->hooked!
                                                 :load-this true
                                                 :pop-result true)
                            (asm/install-fn-call #'hooks/gamepack-signature-verifier->get-verified-resource
                                                 :load-this true
                                                 :override-arg-count 1
                                                 :pre-call-insns [(asm/load-var 2) ;; (ZLjava/lang/String;), we care only about resource name
                                                                  ])
                            (.add (org.objectweb.asm.tree.TypeInsnNode.
                                   org.objectweb.asm.Opcodes/CHECKCAST
                                   "[B"))
                            (.add (org.objectweb.asm.tree.InsnNode.
                                   org.objectweb.asm.Opcodes/ARETURN)))))

               (asm/class-visitor->bytes cn))

             (= mapped-name "nativeadvert.browsercontrol")
             (let [cn (-> (asm/new-class-reader class-bytes)
                          (asm/class-reader->visitor (asm/new-class-node)))
                   methods (.-methods cn)]

               ;; Defuse this class
               (when-let [create (first (filter #(= (.-name %1) "create") methods))]
                 (doto (.-instructions create)
                   ;; In reverse order - insert prepends.
                   (.insert (org.objectweb.asm.tree.InsnNode.
                             org.objectweb.asm.Opcodes/IRETURN))
                   (.insert (org.objectweb.asm.tree.InsnNode.
                             org.objectweb.asm.Opcodes/ICONST_0))))


               (asm/class-visitor->bytes cn))

             :else
             nil)]
    (do
      (println "launcher transformed =" name)
      transformed)
    class-bytes))
