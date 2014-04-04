(ns circuits.adders
  (:require [circuits.core :refer :all ] :reload-all ))

(defn half-adder
  "Half adder circuit"
  [a b s c uid & {:keys [del] :or {del 0}}]
  (let [uid-xor (str uid "xor")
        uid-and (str uid "and")]
    (xor-gate a b s uid-xor :del del)
    (and-gate a b c uid-and :del del)))

(defn full-adder
  "Full adder circuit"
  [a b c0 s c1 uid & {:keys [del] :or {del 0}}]
  (let [ha1s (wire)
        ha1c (wire)
        ha2c (wire)
        del2 (/ del 2)
        uid-ha1 (str uid "ha1")
        uid-ha2 (str uid "ha2")
        uid-or (str uid "or")]
    (half-adder c0 a ha1s ha1c uid-ha1 :del del2)
    (half-adder ha1s b s ha2c uid-ha2 :del del2)
    (or-gate ha1c ha2c c1 uid-or :del del)))



(defn -main []
  (def a (wire))
  (def b (wire))
  (def c0 (wire))
  (def s (wire))
  (def c1 (wire))

  (full-adder a b c0 s c1 "a1" :del 2)

  (probe c1 "c1")
  (probe s "s")
  (probe a "a")
  (probe b "b")

  (do
    (println "starting....")
    (sig! a 0)
    (sig! b 0)
    (future
      (Thread/sleep 4000)
      (println "switch 1")
      (sig! a 1)
      (sig! b 0)
      (Thread/sleep 4000)
      (println "switch 2")
      (sig! a 0)
      (sig! b 1)
      (Thread/sleep 4000)
      (println "switch 3")
      (sig! a 1)
      (sig! b 1) nil) nil))





