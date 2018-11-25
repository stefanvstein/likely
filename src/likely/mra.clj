(ns likely.mra
  (:require [clojure.math.combinatorics :refer [selections]]))

(defn- normalize-accents [s]
  (let [t (java.text.Normalizer/normalize s java.text.Normalizer$Form/NFD)]
    (.replaceAll (.matcher #"\p{InCombiningDiacriticalMarks}+" t) "")))

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
           (clojure.string/replace (eat word) #"[AEIYOU]" ""))
    word))

(defn clean-non-alphabetical
  "Drop every non alphabetical character in [word]."
  [word]
  (if-not (= word (clojure.string/replace word #"[^A-Z]" ""))
    (clojure.string/replace (normalize-accents word) #"[^A-Z]" "")
    word))

(defn- prep-word [word]
  (-> (clojure.string/upper-case word)
      (clean-non-alphabetical)))

(defn- simplify [word]
  (-> word
      (clojure.string/replace #"ZZ" "TS")
      (clojure.string/replace \E \I)
      (clojure.string/replace \O \A)
      (clojure.string/replace \U \A)
      (clojure.string/replace \Y \I)
      (clojure.string/replace \X \S)
      (clojure.string/replace \W \V)
      (clojure.string/replace \Z \S)
      (clojure.string/replace \J \I)
      (clojure.string/replace \Q \K)
      (clojure.string/replace "H" "")))

(defn expand-combinations [word from from-pattern tos]
  (if (clojure.string/includes? word from)
    (let [splited (clojure.string/split word from-pattern)
          ends-with-C (clojure.string/ends-with? word from)
          no-c (+ (dec (count splited))
                  (if ends-with-C
                    1 0))
          KS-combinations (selections tos no-c)]

      (for [c KS-combinations]
        (apply str (if ends-with-C
                     (interleave splited c)
                     (concat (interleave splited c) (last splited))))))
    [word]))
      
      
          
      

(defn mra-codex
  "Compute the MRA codex for a [word]."
  [word]
  
  (-> (prep-word word)
      (drop-non-leading-vowel)
      (simplify)
      (distinct-consecutive)
      (get-codex-letters)
      (expand-combinations "C" #"C" ["K" "S"])))
