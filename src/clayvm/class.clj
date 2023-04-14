(ns clayvm.class
  (:require [clojure.java.io :as io])
  (:import [java.io DataInputStream]))

;; ClassFile {
;;     u4             magic;
;;     u2             minor_version;
;;     u2             major_version;
;;     u2             constant_pool_count;
;;     cp_info        constant_pool[constant_pool_count-1];
;;     u2             access_flags;
;;     u2             this_class;
;;     u2             super_class;
;;     u2             interfaces_count;
;;     u2             interfaces[interfaces_count];
;;     u2             fields_count;
;;     field_info     fields[fields_count];
;;     u2             methods_count;
;;     method_info    methods[methods_count];
;;     u2             attributes_count;
;;     attribute_info attributes[attributes_count];
;; }

(defn data-stream [class-file-path]
  (-> (io/resource class-file-path)
      io/input-stream
      DataInputStream.))

(defn read-type-desc [dis t]
  (case t
    :u4 (.readInt dis)
    :u2 (.readUnsignedShort dis)
    :u1 (.readUnsignedByte dis)))

(defn read-bytes [dis desc]
  (reduce (fn [r [field-type field-name]]
            (assoc r field-name (read-type-desc dis field-type)))
   {}
   desc))

(def cp-info-tag
  {7 :CONSTANT_Class
   9 :CONSTANT_Fieldref
   10 :CONSTANT_Methodref
   11 :CONSTANT_InterfaceMethodref
   8 :CONSTANT_String
   3 :CONSTANT_Integer
   4 :CONSTANT_Float
   5 :CONSTANT_Long
   6 :CONSTANT_Double
   12 :CONSTANT_NameAndType
   1 :CONSTANT_Utf8
   15 :CONSTANT_MethodHandle
   16 :CONSTANT_MethodType
   18 :CONSTANT_InvokeDynamic})

(defn read-cp-tag [dis]
  (-> (read-type-desc dis :u1)
      cp-info-tag))

(defn read-cp-method-interface-or-field-ref [dis]
  (read-bytes dis [[:u2 :class-index]
                   [:u2 :name-and-type-index]]))

(defn read-cp-constant-class [dis]
  (read-bytes dis [[:u2 :name-index]]))

(defn read-cp-constant-name-and-type [dis]
  (read-bytes dis [[:u2 :name-index]
                   [:u2 :descriptor-index]]))

(defn read-cp-constant-utf8 [dis]
  (let [{:keys [length] :as res} (read-bytes dis [[:u2 :length]])
        bytes (byte-array length)]
    (.read dis bytes)
    (assoc res :bytes (String. bytes))))

(defn read-cp-contant-string [dis]
  {:string-index (read-type-desc dis :u2)})

(defn read-cp-info [dis]
  (let [tag (read-cp-tag dis)
        cp-info (case tag
                  :CONSTANT_Class (read-cp-constant-class dis)
                  :CONSTANT_Fieldref (read-cp-method-interface-or-field-ref dis)
                  :CONSTANT_Methodref (read-cp-method-interface-or-field-ref dis)
                  :CONSTANT_InterfaceMethodref (read-cp-method-interface-or-field-ref dis)
                  :CONSTANT_String (read-cp-contant-string dis)
                  :CONSTANT_Integer (throw (ex-info "Tag not imlemented" {:tag :CONSTANT_Integer}))
                  :CONSTANT_Float (throw (ex-info "Tag not imlemented" {:tag :CONSTANT_Float}))
                  :CONSTANT_Long (throw (ex-info "Tag not imlemented" {:tag :CONSTANT_Long}))
                  :CONSTANT_Double (throw (ex-info "Tag not imlemented" {:tag :CONSTANT_Double}))
                  :CONSTANT_NameAndType (read-cp-constant-name-and-type dis)
                  :CONSTANT_Utf8 (read-cp-constant-utf8 dis)
                  :CONSTANT_MethodHandle (throw (ex-info "Tag not imlemented" {:tag :CONSTANT_MethodHandle}))
                  :CONSTANT_MethodType (throw (ex-info "Tag not imlemented" {:tag :CONSTANT_MethodType}))
                  :CONSTANT_InvokeDynamic (throw (ex-info "Tag not imlemented" {:tag :CONSTANT_InvokeDynamic})))]
    (assoc cp-info
           :tag tag)))

(defn flags-u2->class-flags-set [u2]
  (let [fset? (fn [bits]
                (not (zero? (bit-and u2 bits))))]
    (cond-> #{}
      (fset? 0x0001) (conj :ACC_PUBLIC)
      (fset? 0x0010) (conj :ACC_FINAL)
      (fset? 0x0020) (conj :ACC_SUPER)
      (fset? 0x0200) (conj :ACC_INTERFACE)
      (fset? 0x0400) (conj :ACC_ABSTRACT)
      (fset? 0x1000) (conj :ACC_SYNTHETIC)
      (fset? 0x2000) (conj :ACC_ANNOTATION)
      (fset? 0x4000) (conj :ACC_ENUM))))

(defn flags-u2->methods-flags-set [u2]
  (let [fset? (fn [bits]
                (not (zero? (bit-and u2 bits))))]
    (cond-> #{}
      (fset? 0x0001) (conj :ACC_PUBLIC)
      (fset? 0x0002) (conj :ACC_PRIVATE)
      (fset? 0x0004) (conj :ACC_PROTECTED)
      (fset? 0x0008) (conj :ACC_STATIC)
      (fset? 0x0010) (conj :ACC_FINAL)
      (fset? 0x0020) (conj :ACC_SYNCHRONIZED)
      (fset? 0x0040) (conj :ACC_BRIDGE)
      (fset? 0x0080) (conj :ACC_VARARGS)
      (fset? 0x0100) (conj :ACC_NATIVE)
      (fset? 0x0400) (conj :ACC_ABSTRACT)
      (fset? 0x0800) (conj :ACC_STRICT)
      (fset? 0x1000) (conj :ACC_SYNTHETIC))))

(defn read-interfaces-indices [dis interfaces-count]
  (loop [i interfaces-count
         indices []]
    (if (zero? i)
      indices
      (recur (dec i) (read-type-desc dis :u2)))))

(defn read-unsigned-bytes [dis cnt]
  (loop [i 0
         bs []]
    (if (< i cnt)
      (recur (inc i) (conj bs (.readUnsignedByte dis)))
      bs)))

(defn parse-code-attribute [attr-bytes]
  (let [dis (DataInputStream. (io/input-stream attr-bytes))
        max-stack (read-type-desc dis :u2)
        max-locals (read-type-desc dis :u2)
        code-length (read-type-desc dis :u4)
        code-bytes (read-unsigned-bytes dis code-length)]
    {:attr/type :Code
     :code-bytes code-bytes}))

(defn parse-attribute [attr-name attr-bytes]
  (case attr-name
    "Code" (parse-code-attribute attr-bytes)
     "ConstantValue" {:attr/type :ConstantValue}
     "StackMapTable" {:attr/type :StackMapTable}
     "Exceptions" {:attr/type :Exceptions}
     "InnerClasses" {:attr/type :InnerClasses}
     "EnclosingMethod" {:attr/type :EnclosingMethod}
     "Synthetic" {:attr/type :Synthetic}
     "Signature" {:attr/type :Signature}
     "SourceFile" {:attr/type :SourceFile}
     "SourceDebugExtension" {:attr/type :SourceDebugExtension}
     "LineNumberTable" {:attr/type :LineNumberTable}
     "LocalVariableTable" {:attr/type :LocalVariableTable}
     "LocalVariableTypeTable" {:attr/type :LocalVariableTypeTable}
     "Deprecated" {:attr/type :Deprecated}
     "RuntimeVisibleAnnotations" {:attr/type :RuntimeVisibleAnnotations}
     "RuntimeInvisibleAnnotations" {:attr/type :RuntimeInvisibleAnnotations}
     "RuntimeVisibleParameterAnnotations" {:attr/type :RuntimeVisibleParameterAnnotations}
     "RuntimeInvisibleParameterAnnotations" {:attr/type :RuntimeInvisibleParameterAnnotations}
     "AnnotationDefault" {:attr/type :AnnotationDefault}
     "BootstrapMethods" {:attr/type :BootstrapMethods}
    (throw (ex-info "Unkown attribute" {:attr-name attr-name
                                        :attr-bytes attr-bytes}))))

(defn read-attribute [dis cons-pool-table]
  (let [attr-info (read-bytes dis [[:u2 :attribute-name-index]
                                   [:u4 :attribute-length]])
        attr-name (-> attr-info
                      :attribute-name-index
                      cons-pool-table
                      :bytes)
        attr-bytes (byte-array (:attribute-length attr-info))]
    (.read dis attr-bytes)
    (parse-attribute attr-name attr-bytes)))

(defn read-attributes [dis cnt cons-pool-table]
  (loop [i cnt
         attrs []]
    (if (zero? i)
      attrs
      (recur (dec i) (conj attrs (read-attribute dis cons-pool-table))))))

(defn read-field-or-method-info [dis cons-pool-table]
  (let [field-info (read-bytes dis [[:u2 :access-flags]
                                    [:u2 :name-index]
                                    [:u2 :descriptor-index]
                                    [:u2 :attributes-count]])
        attributes (read-attributes dis (:attributes-count field-info) cons-pool-table)]

    (-> field-info
        (update :access-flags flags-u2->methods-flags-set)
        (update :name-index cons-pool-table)
        (update :descriptor-index cons-pool-table)
        (assoc :attributes attributes))))

(defn read-fields-or-methods [dis cnt cons-pool-table]
  (loop [i cnt
         xs []]
    (if (zero? i)
      xs
      (recur (dec i)
             (conj xs (read-field-or-method-info dis cons-pool-table))))))

(defn read-constant-pool-table [dis cnt]
  (loop [i (dec cnt)
         table [:no-zero]] ;; hacky but all the indexes on the pool table start at 1, so lets shift everything
    (if (zero? i)
      table
      (recur (dec i)
             (conj table (read-cp-info dis))))))

(defn parse-class [class-file-path]
  (let [dis (data-stream class-file-path)
        magic-str (format "%x" (read-type-desc dis :u4))]

    (if-not (= "cafebabe" magic-str)

      (throw (ex-info "Doesn't look like a class file." {:file class-file-path :magic magic-str}))

      (let [minor-version (read-type-desc dis :u2)
            major-version (read-type-desc dis :u2)
            constant-pool-count (read-type-desc dis :u2)
            cons-pool-table (read-constant-pool-table dis constant-pool-count)
            class-flags (-> (read-type-desc dis :u2)
                            flags-u2->class-flags-set)
            {:keys [this-class super-class interfaces-count]} (read-bytes dis
                                                                          [[:u2 :this-class]
                                                                           [:u2 :super-class]
                                                                           [:u2 :interfaces-count]])
            interfaces-indices (read-interfaces-indices dis interfaces-count)
            fields-count (read-type-desc dis :u2)
            fields (read-fields-or-methods dis fields-count cons-pool-table)
            methods-count (read-type-desc dis :u2)
            methods (read-fields-or-methods dis methods-count cons-pool-table)
            attributes-count (read-type-desc dis :u2)
            attributes (read-attributes dis attributes-count cons-pool-table)]

        {:class-version [major-version minor-version]
         :class-flags class-flags
         :cons-pool-table cons-pool-table
         :this-class (get cons-pool-table this-class)
         :super-class (when-not (zero? super-class)
                        (get cons-pool-table super-class))
         :interfaces-indices interfaces-indices
         :fields fields
         :methods methods
         :class-attributes attributes}

        ))))

(comment

  (parse-class "Hello.class")


  )
