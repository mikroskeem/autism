(ns asm
  (:import (org.objectweb.asm
            ClassReader
            ClassVisitor
            ClassWriter
            Opcodes
            Type)
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

(def primitives {Type/BOOLEAN_TYPE :bool
                 Type/BYTE_TYPE :byte
                 Type/CHAR_TYPE :char
                 Type/DOUBLE_TYPE :double
                 Type/FLOAT_TYPE :float
                 Type/INT_TYPE :int
                 Type/LONG_TYPE :long
                 Type/SHORT_TYPE :short})

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
    (.accept cn (visitor-fn new-cn))
    new-cn))

(defmacro new-insnlist []
  `(InsnList.))

(defn new-method [access name desc]
  (MethodNode. access name desc nil nil))

(defn ldc [value]
  (LdcInsnNode. value))

(defn is-static? [access]
  (-> access
      (bit-and Opcodes/ACC_STATIC)
      (= 0)
      (not)))

(defn invoke-method
  ([type owner name sig]
   (invoke-method type owner name sig (= type :intf)))
  ([type owner name sig intf]
   (MethodInsnNode.
    (condp = type
      :virt Opcodes/INVOKEVIRTUAL
      :static Opcodes/INVOKESTATIC
      :intf Opcodes/INVOKEINTERFACE
      :special Opcodes/INVOKESPECIAL
      (throw (ex-info "Unknown method invocation opcode" {})))
    owner name sig intf)))

(defn load-var
  ([n] (load-var n :object))
  ([n type]
   (VarInsnNode. (condp = type
                   :bool Opcodes/ILOAD
                   :byte Opcodes/ILOAD
                   :char Opcodes/ILOAD
                   :double Opcodes/DLOAD
                   :float Opcodes/FLOAD
                   :int Opcodes/ILOAD
                   :long Opcodes/LLOAD
                   :short Opcodes/ILOAD
                   :object Opcodes/ALOAD)
                 n)))

(defn box-primitive [type]
  (let [[owner sig] (condp = type
                      :bool ["java/lang/Boolean" "Z"]
                      :byte ["java/lang/Byte" "B"]
                      :char ["java/lang/Char" "C"]
                      :double ["java/lang/Double" "D"]
                      :float ["java/lang/Float" "F"]
                      :int ["java/lang/Integer" "I"]
                      :long ["java/lang/Long" "J"]
                      :short ["java/lang/Short" "S"])]
    (invoke-method :static owner "valueOf" (str "(" sig ")L" owner ";"))))

(defn return
  ([] (return :object))
  ([type]
   (InsnNode. (condp = type
                :object Opcodes/ARETURN
                :int Opcodes/IRETURN
                :bool Opcodes/IRETURN))))

(defn check-cast [type]
  (TypeInsnNode. Opcodes/CHECKCAST type))

(defn throw-unsupported-operation-exception []
  (throw (UnsupportedOperationException.)))

(defn return-null
  ([] (-> (new-insnlist) return-null))
  ([insn-list]
   (doto insn-list
     (.add (InsnNode. Opcodes/ACONST_NULL))
     (.add (InsnNode. Opcodes/ARETURN)))))

(defn install-fn-call [insn-list fn & {:keys [load-this load-params
                                              pop-result override-arg-count
                                              pre-call-insns
                                              method-desc
                                              static-context]
                                       :or {load-this false
                                            load-params 0
                                            pop-result false
                                            override-arg-count nil
                                            pre-call-insns nil
                                            method-desc nil
                                            static-context false}}]

  ;; Insert Clojure.var(String)
  (let [{:keys [ns name]} (meta fn)
        nsname (-> ns ns-name str)
        funcname (-> name str)]
    (doto insn-list
      (.add (ldc nsname))
      (.add (ldc funcname))
      (.add (invoke-method :static
                           "clojure/java/api/Clojure" "var"
                           "(Ljava/lang/Object;Ljava/lang/Object;)Lclojure/lang/IFn;")))

    (if load-this
      (if static-context
        (throw (ex-info "Cannot load `this` in static context" {}))
        (.add insn-list (load-var 0))))

    (when (< 0 load-params)
      (let [parsed-desc (if method-desc (Type/getArgumentTypes method-desc) '())]
        (doseq [i (range load-params)]
          (let [param-type (if-let [type (get parsed-desc i)]
                             (get primitives type :object)
                             (if method-desc
                               (throw (ex-info "Parameter index out of bounds" {}))
                               :object))]
            (.add insn-list (load-var (+ (if static-context
                                           0 1)
                                         i)
                                      param-type))
            (if-not (= param-type :object)
              (.add insn-list (box-primitive param-type)))))))

    (if (and (some? pre-call-insns) (not (empty? pre-call-insns)))
      (doseq [insn pre-call-insns]
        (.add insn-list insn)))

    ;; Call the func
    (let [arg-count
          (+ (if (some? override-arg-count) override-arg-count 0)
             (if load-this 1 0)
             (if (< 0 load-params) load-params 0))]
      (.add insn-list
            (invoke-method :intf
                           "clojure/lang/IFn"
                           "invoke"
                           (str "("
                                (clojure.string/join
                                 (repeat arg-count "Ljava/lang/Object;"))
                                ")Ljava/lang/Object;"))))

    ;; Pop result
    (if pop-result
      (.add insn-list (InsnNode. Opcodes/POP)))))
