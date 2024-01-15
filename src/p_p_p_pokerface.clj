(ns p-p-p-pokerface)

(def replacements {\A 14 \K 13 \Q 12 \J 11 \T 10});; => #'p-p-p-pokerface/replacements
(defn rank [card]
  (let [[fst _] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (replacements fst))));; => #'p-p-p-pokerface/rank
(rank "2H");; => 2
(rank "4S");; => 4
(rank "TS");; => 10
(rank "JS");; => 11
(rank "QS");; => 12
(rank "KS");; => 13
(rank "AS");; => 14

(defn suit [card]
  (let [[_ snd] card]
    (str snd)));; => #'p-p-p-pokerface/suit
(suit "QH");; => "H"
(suit "2H");; => "H"
(suit "2D");; => "D"
(suit "2C");; => "C"
(suit "3S");; => "S"
(comment ;the lein midje doesn't load with these definitions 
  (def high-seven                   ["2H" "3S" "4C" "5C" "7D"])
  (def pair-hand                    ["2H" "2S" "4C" "5C" "7D"])
  (def two-pairs-hand               ["2H" "2S" "4C" "4D" "7D"])
  (def three-of-a-kind-hand         ["2H" "2S" "2C" "4D" "7D"])
  (def four-of-a-kind-hand          ["2H" "2S" "2C" "2D" "7D"])
  (def straight-hand                ["2H" "3S" "6C" "5D" "4D"])
  (def low-ace-straight-hand        ["2H" "3S" "4C" "5D" "AD"])
  (def high-ace-straight-hand       ["TH" "AS" "QC" "KD" "JD"])
  (def flush-hand                   ["2H" "4H" "5H" "9H" "7H"])
  (def full-house-hand              ["2H" "5D" "2D" "2C" "5S"])
  (def straight-flush-hand          ["2H" "3H" "6H" "5H" "4H"])
  (def low-ace-straight-flush-hand  ["2D" "3D" "4D" "5D" "AD"])
  (def high-ace-straight-flush-hand ["TS" "AS" "QS" "KS" "JS"]))

(defn pair? [hand]
  (= (sort (vals (frequencies (map rank hand)))) [1 1 1 2]))
(pair? ["2H" "2S" "4C" "5C" "7D"])
(pair? ["2H" "2S" "2C" "4D" "7D"])

(defn three-of-a-kind? [hand]
  (= (sort (vals (frequencies (map rank hand)))) [1 1 3]))

(defn four-of-a-kind? [hand]
  (= (sort (vals (frequencies (map rank hand)))) [1 4]))

(defn flush? [hand]
  (= 5 (apply max (vals (frequencies (map suit hand))))))
(flush?  ["2H" "3S" "4C" "5C" "7D"])
(flush? ["2H" "4H" "5H" "9H" "7H"])

(defn full-house? [hand]
  (= [2 3] (sort (vals (frequencies (map rank hand))))))
(full-house? ["2H" "5D" "2D" "2C" "5S"])
(full-house?  ["2H" "3S" "4C" "5C" "7D"])
(full-house? ["2H" "4H" "5H" "9H" "7H"])

(defn two-pairs? [hand]
  (= [1 2 2] (sort (vals (frequencies (map rank hand))))))
(two-pairs? ["2H" "5D" "2D" "2C" "5S"])
(two-pairs?  ["2H" "3S" "2C" "5C" "5D"])

(defn straight? [hand]
  (let [ordered (sort (map rank hand))
        smallest (first ordered)
        biggest (apply max ordered)]
    (if (and (= 14 biggest) (= 2 smallest))
      (= ordered [2 3 4 5 14])
      (= ordered (range smallest (+ smallest 5)))
      )
    ))
(straight? ["7H" "3S" "6C" "5D" "4D"])
(straight?  ["2H" "3S" "2C" "5C" "5D"])

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))
(straight-flush? ["2H" "3H" "6H" "5H" "4H"])
(straight-flush? ["2D" "3D" "4D" "5D" "AD"])

(defn value [hand]
  (cond
    (straight-flush? hand) 8
    (four-of-a-kind? hand) 7
    (full-house? hand) 6
    (flush? hand) 5
    (straight? hand) 4
    (three-of-a-kind? hand) 3
    (two-pairs? hand) 2
    (pair? hand) 1
    :else 0
    ))
(value ["2D" "3D" "4D" "5D" "AD"])
