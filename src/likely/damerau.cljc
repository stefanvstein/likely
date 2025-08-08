
(ns likely.damerau
  (:import [likely DamerauLevenshtein]))
(comment
  ;;; En cljs vänlig variant med persistenta strukturer
  #_(defn damerau-levenshtein
      "returns the number of keystrokes between two strings"
      [s1 s2]
      (let [len1 (count s1)
            len2 (count s2)
            ;; Skapa en tom matrix
            distance (vec (for [i (range (inc len1))]
                            (vec (repeat (inc len2) 0))))
            ;; Initiera första raden och kolumnen
            distance (reduce (fn [d i] (assoc-in d [i 0] i)) distance (range (inc len1)))
            distance (reduce (fn [d j] (assoc-in d [0 j] j)) distance (range (inc len2)))]
        ;; Beräkna avståndet
        (loop [i 1
               dist distance]
          (if (> i len1)
            ;; Returnera nedersta högra cellen
            (get-in dist [len1 len2])
            (let [dist (loop [j 1
                              d dist]
                         (if (> j len2)
                           d
                           (let [cost (if (= (nth s1 (dec i)) (nth s2 (dec j))) 0 1)
                                 del (inc (get-in d [(dec i) j]))
                                 ins (inc (get-in d [i (dec j)]))
                                 sub (+ cost (get-in d [(dec i) (dec j)]))
                                 trans (if (and (> i 1) (> j 1)
                                                (= (nth s1 (dec i)) (nth s2 (- j 2)))
                                                (= (nth s1 (- i 2)) (nth s2 (dec j))))
                                         (inc (get-in d [(- i 2) (- j 2)]))
                                         ##Inf)]
                             (recur (inc j) (assoc-in d [i j] (min del ins sub trans))))))]
              (recur (inc i) dist))))))

  #_(defn damerau-levenshtein
    "Returns the number of keystrokes between two files"
    [^CharSequence s1 ^CharSequence s2]
    (let [len1 (count s1)
          len2 (count s2)
          prev-row (int-array (inc len2))
          curr-row (int-array (inc len2))
          prev-prev-row (int-array (inc len2))]
    ;; initiera första raden
      (dotimes [j (inc len2)]
        (aset-int prev-row j j))
    ;; iterera genom s1
      (dotimes [i len1]
        (aset-int curr-row 0 (inc i))
        (dotimes [j len2]
          (let [cost (if (= (.charAt s1 i) (.charAt s2 j)) 0 1)
                del (inc (aget prev-row (inc j)))
                ins (inc (aget curr-row j))
                sub (+ cost (aget prev-row j))
                trans (if (and (> i 0) (> j 0)
                               (= (.charAt s1 i) (.charAt s2 (dec j)))
                               (= (.charAt s1 (dec i)) (.charAt s2 j)))
                        (inc (aget prev-prev-row (dec j)))
                        Integer/MAX_VALUE)]
            (aset-int curr-row (inc j) (min del ins sub trans))))
      ;; flytta rader
        (System/arraycopy prev-row 0 prev-prev-row 0 (inc len2))
        (System/arraycopy curr-row 0 prev-row 0 (inc len2)))
      (aget curr-row len2)))
  
;;;En array intensiv clojure varant
  #_(defn damerau-java-style [^String s1 ^String s2]
      (let [len1 (count s1)
            len2 (count s2)
            dist (make-array Integer/TYPE (inc len1) (inc len2))]

        ;; init
        (dotimes [i (inc len1)]
          (aset-int dist i 0 i))
        (dotimes [j (inc len2)]
          (aset-int dist 0 j j))

        ;; fill
        (dotimes [i len1]
          (dotimes [j len2]
            (let [cost (if (= (.charAt s1 i) (.charAt s2 j)) 0 1)
                  del (inc (aget dist i (inc j)))
                  ins (inc (aget dist (inc i) j))
                  sub (+ cost (aget dist i j))
                  base (min del ins sub)]
              (aset-int dist (inc i) (inc j)
                        (if (and (>= i 1) (>= j 1)
                                 (= (.charAt s1 i) (.charAt s2 (dec j)))
                                 (= (.charAt s1 (dec i)) (.charAt s2 j)))
                          (min base (inc (aget dist (- i 1) (- j 1))))
                          base)))))

        (aget dist len1 len2)))

)

;; Java varianten. Java ser ut att vara överlägsen på denna algorithm. Kanske blir det annorlunda om vi optimerar med en enda array osv.
;; Vore kul att slippa java
;; Denna tar även en max längd och ger null vid långt. optimerat för klienten
(defn damerau-levenshtein [s1 s2 maxi]
  (let [r (DamerauLevenshtein/distanceOf s1 s2 maxi)]
    (when-not (= r -1) r)))



(comment
  (= 1 (damerau-levenshtein nil "olel" 10)))

(defn- substr
  "A friendlier, non barking, subs "[s i l]
  (when s
    (if (> (+ i l) (count s))
      (if (> i (count s))
        ""
        (subs s i))
      (subs s i l))))

(comment
  (= "ll" (subs "olle" 1 3))
  (subs "olle" 1 6)
  (substr "olle" 1 6))

(defn damerau-prefix
  [src target distance]
  (when (and src target)
    (damerau-levenshtein (substr src 0 distance)
                         (substr target 0 distance)
                         distance)))
(defn damerau
  "The number of keystrokes between two strings, nil if more than max"

  [src target max]
  (damerau-levenshtein src target max))

(comment
  (damerau-prefix "olleman" "olla" 4)
  (damerau-prefix "olelman" "olla" 4)
  (damerau "olleman" "olla" 4)
  (damerau "olelman" "olla" 10)
  (damerau "olelma" "olla" 10)
  (damerau "olela" "olla" 10)
  (damerau "olale" "ollaen" 2))
