(ns clayvm.disassembler)

(defn join-bytes [b0 b1]
  (bit-or (bit-shift-left b0 8)
          b1))

(defn next-instruction [[b0 b1 b2 & _]]
  (when b0
    (case b0
      (0x3,0x4,0x5,0x6,0x7,0x8) {:op :iconst, :i (- b0 3), :size 1}
      0x12                      {:op :ldc :idx b1 :size 2}
      (0x2a,0x2b,0x2c,0x2d)     {:op :aload :n (- b0 42) :size 1}
      0xb8                      {:op :invokestatic :idx (join-bytes b1 b2) :size 3}
      0xb1                      {:op :return :size 1}
      0xb2                      {:op :getstatic :idx (join-bytes b1 b2) :size 3}
      0xb6                      {:op :invokevirtual :idx (join-bytes b1 b2) :size 3}
      0xb7                      {:op :invokespecial :idx (join-bytes b1 b2) :size 3}
      (throw (ex-info "Unknown instruction" {:instruction-byte b0 :byte-hex (format "0x%x" b0)})))))

(defn disassemble-bytes [bytes]
  (loop [bs bytes
         ops []]
    (if-not (seq bs)
      ops
      (let [{:keys [size] :as op} (next-instruction bs)]
        (recur (subvec bs size) (conj ops op))))))

(comment
  (next-instruction [8 184 0 7 177])

  (disassemble-bytes [8 184 0 7 177])
  (disassemble-bytes [42 183 0 1 177])
  (disassemble-bytes [178 0 7 18 13 182 0 15 177])

  )
