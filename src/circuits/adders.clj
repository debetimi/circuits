(require '[circuits.core :refer :all ])

(defn half-adder
  "Half adder circuit"
  [a b s c uid & {:keys [del] :or {del 0}}]
  (println del)
  (let [uid-xor (str uid "xor")
        uid-and (str uid "and")]
    (xor-gate a b s uid-xor :del del)
    (and-gate a b c uid-and :del del)))

(def a (wire))
(def b (wire))
(def s (wire))
(def c (wire))

(half-adder a b s c "ha1")

(do
  (sig! a 0)
  (sig! b 0) nil)