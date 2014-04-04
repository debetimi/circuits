(ns circuits.core)

(defn sig!
  "sets v on a wire input"
  [a v]
  (reset! (:i a) v))

(defn probe
  "place a probe on output of net and reports when its value has changed"
  [net uid & {:keys [del] :or {del 1}}]
  (let [o (:o net)
        delta (* del 1000)
        watch
        (fn [k a old new]
          (future
            (Thread/sleep delta)
            (condp
              not= @o
                   old (print (str uid ":" @o "\n")))))]
    (add-watch o :watch watch)))

(defn invert
  [x]
  (case x
    0 1
    0))

(defn inverter
  "This is a logical inverter
  b is set to !a after a delay"
  [a b uid & {:keys [del] :or {del 0}}]
  (let [i1 (:o a)
        o1 (:i b)
        delta (* del 1000)
        do-invert!
        (fn [k a old new]
          (future
            (Thread/sleep delta)
            (reset! o1 (invert @i1)) nil))]
    (add-watch i1 (keyword uid) do-invert!)
    (reset! o1 (invert @i1))
    {:d delta}))

(defn wire
  "This is a wire. It takes an optional delay
  keyword parameter and outputs a map with :i, and :o
  which are atoms and :d which represents the delay
  in milliseconds. The default delay is 0. When i changes value,
  o follows in d number of seconds"
  [& {:keys [del] :or {del 0}}]
  (let [in (atom 0)
        out (atom 0)
        delta (* del 1000)
        ;;Transition is a watch function on the input
        ;;when the value changes it sets the output the value of the
        ;;input after delay seconds
        transition!
        (fn [k a old new]
          (future
            (Thread/sleep delta)
            (reset! out new) nil))]
    (add-watch in :fn transition!)
    {:i in, :o out :d delta}))

(defn gate
  "creates a logic gate, takes in
  bit function and text to display when
  logic gate transitions"
  [func]
  ;;a, b are input wires
  ;;c is output wire
  ;;uid is unique id required per element
  ;;so we can attach unique watchers to nets
  (fn [a b c uid & {:keys [del] :or {del 1}}]
    (let [i1 (:o a)
          i2 (:o b)
          o (:i c)
          delta (* del 1000)
          update!
          (fn [k a old new]
            (future
              (Thread/sleep delta)
              (reset! o (func @i1 @i2)) nil))]
      (add-watch i1 (keyword uid) update!)
      (add-watch i2 (keyword uid) update!)
      (reset! o (func @i1 @i2))
      {:d delta})))

(defn bit-nand
  [a b]
  (invert (bit-and a b)))

(def and-gate
  (gate bit-and))

(def or-gate
  (gate bit-or))

(def xor-gate
  (gate bit-xor))

(def nand-gate
  (gate bit-nand))

