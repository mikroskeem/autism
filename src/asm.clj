(ns asm
  (:import (org.objectweb.asm
            ClassReader
            ClassVisitor
            ClassWriter
            Opcodes)
           (org.objectweb.asm.tree
            ClassNode
            InsnList
            InsnNode
            LdcInsnNode
            MethodNode
            MethodInsnNode
            TypeInsnNode
            VarInsnNode)))

(defn new-class-reader [bytes]
  (ClassReader. bytes))

(defn class-reader->visitor
  ([reader visitor] (class-reader->visitor reader visitor nil))
  ([reader visitor flags]
   (.accept reader visitor
            (if (some? flags)
              (if (>= (count flags) 2)
                (apply bit-or flags)
                (first flags))
              0))
   visitor))

(defn class-visitor->bytes
  ([^ClassVisitor cv] (class-visitor->bytes cv [ClassWriter/COMPUTE_MAXS ClassWriter/COMPUTE_FRAMES]))
  ([^ClassVisitor cv flags]
   (let [orred-flags  (if (some? flags)
                        (if (>= (count flags) 2)
                          (apply bit-or flags)
                          (first flags))
                        0)
         cw ;;(ClassWriter.)
         (proxy [ClassWriter] [orred-flags]
           (getCommonSuperClass [type1 type2]
             "java/lang/Object"))
         ]
     (.accept cv cw)
     (.toByteArray cw))))

(defn new-class-node []
  (ClassNode.))

(defn class-node-accept [^ClassNode cn visitor-fn]
  (let [new-cn (new-class-node)]
    (.accept new-cn (visitor-fn cn))
    new-cn))

(defmacro new-insnlist []
  `(InsnList.))

(defn new-method [access name desc]
  (MethodNode. access name desc nil nil))

(defn ldc [value]
  (LdcInsnNode. value))

(defn load-var [n]
  (VarInsnNode. Opcodes/ALOAD n))

(defn throw-unsupported-operation-exception []
  (throw (UnsupportedOperationException.)))

(defn return-null [insn-list]
  (doto insn-list
    (.add (org.objectweb.asm.tree.InsnNode.
           Opcodes/ACONST_NULL))
    (.add (org.objectweb.asm.tree.InsnNode.
           Opcodes/ARETURN))))

(defn install-fn-call [insn-list fn & {:keys [load-this load-params
                                              pop-result override-arg-count
                                              pre-call-insns]
                                       :or {load-this false
                                            load-params 0
                                            pop-result false
                                            override-arg-count nil
                                            pre-call-insns nil}}]
  
  ;; Insert Clojure.var(String)
  (let [{:keys [ns name]} (meta fn)
        nsname (-> ns ns-name str)
        funcname (-> name str)]
    (doto insn-list
      (.add (ldc nsname))
      (.add (ldc funcname))
      (.add (MethodInsnNode.
             Opcodes/INVOKESTATIC
             "clojure/java/api/Clojure"
             "var"
             "(Ljava/lang/Object;Ljava/lang/Object;)Lclojure/lang/IFn;"
             false))))

  (if load-this
    (.add insn-list (load-var 0)))

  (if (< 0 load-params)
    (doseq [i (range load-params)]
      ;; TODO: inspect method signature and add boxing instructions
      (.add insn-list (load-var (+ i 1)))))

  (if (and (some? pre-call-insns) (not (empty? pre-call-insns)))
    (doseq [insn pre-call-insns]
      (.add insn-list insn)))
  
  ;; Call the func
  (let [arg-count 
        (+ (if (some? override-arg-count) override-arg-count 0)
           (if load-this 1 0)
           (if (< 0 load-params) load-params 0))]
    (.add insn-list (MethodInsnNode.
                     Opcodes/INVOKEINTERFACE
                     "clojure/lang/IFn"
                     "invoke"
                     (str "("
                          (clojure.string/join
                           (repeat arg-count "Ljava/lang/Object;"))
                          ")Ljava/lang/Object;")
                     true)))

  ;; Pop result
  (if pop-result
    (.add insn-list (InsnNode. Opcodes/POP))))
