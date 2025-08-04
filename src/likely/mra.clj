(ns likely.mra
  (:require [clojure.math.combinatorics :refer [selections]]
            [clojure.string :as str]
            [likely.strings :refer [split-str]]
            [likely.normalize :refer [normalize-accents]]))


(defn- get-codex-letters [pword]
  (let [last-3 (min 3 (- (count pword) 3))]
    (apply str (concat (take 3 pword)
                       (take-last last-3 pword)))))

(defn distinct-consecutive
  "Drop consecutive duplicates in sequence"
  [sequence] (map first (partition-by identity sequence)))

(defn eat
  "Drop the first 2 letters of a [string]."
  [string]
  (if (< 2 (count string))
    (subs string 2)
    string))

(defn drop-non-leading-vowel [word]
  (if (< 2 (count word))
    (apply str
           (subs word 0 2)
           (str/replace (eat word) #"[AEIYOU]" ""))
    word))

(defn clean-non-alphabetical
  "Drop every non alphabetical character in [word]."
  [word]
  (if-not (= word (str/replace word #"[^A-Z]" ""))
    (str/replace (normalize-accents word) #"[^A-Z]" "")
    word))

(defn- prep-word [word]
  (-> (str/upper-case word)
      (clean-non-alphabetical)))

(defn- simplify [word]
  (-> word
      (str/replace "ZZ" "TS")
      (str/replace "PH" "F")
      (str/replace \E \I)
      (str/replace \O \A)
      (str/replace \U \A)
      (str/replace \Y \I)
      (str/replace \X \S)
      (str/replace \W \V)
      (str/replace \Z \S)
      (str/replace \J \I)
      (str/replace \Q \K)
      (str/replace "H" "")))

(comment
  (str/replace "HAHAH" "H" "" ))



(defn expand-combinations [word from tos]
  (if (str/includes? word from)
    (let [splited (split-str word from)
          ends-with-C (str/ends-with? word from)
          no-c (+ (dec (count splited))
                  (if ends-with-C
                    1 0))
          KS-combinations (selections tos no-c)]

      (for [c KS-combinations]
        (apply str (if ends-with-C
                     (interleave splited c)
                     (concat (interleave splited c) (last splited))))))
    [word]))
      
(comment
  (= ["JAKL" "JASL"] (expand-combinations "JACL" "C"  ["K" "S"]))
  (= ["JALK" "JALS"] (expand-combinations "JALC" "C"  ["K" "S"]));
  )
          
      

(defn mra-codex
  "Compute the MRA codex for a [word]."
  [word]

  (let [results (-> (prep-word word)
                    (drop-non-leading-vowel)
                    (simplify)
                    (distinct-consecutive)
                    (get-codex-letters)
                    (expand-combinations "C"  ["K" "S"]))]
    (->> results
         (map distinct-consecutive)
         (map #(apply str %)))))

(comment
  (mra-codex "ollemanzzaco")
  (apply str '(\A \L \M \T \S))
  (map #(apply str %) '((\A \L \M \T \S \K) (\A \L \M \T \S))))
