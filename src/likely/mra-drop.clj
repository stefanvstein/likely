(ns likely.mraplusvoweldrop
  (:require [clojure.string :as str]))
{:pat "Æ", :out ["Ä"]}
   {:pat "Ø", :out ["Ö"]}
(def vowels      #{\A \E \I \O \U \Y \Å \Ä \Ö \Æ \Ø})
(def weak-vowels #{\A \E \O})

;; svenska + engelska suffix som inte ska “ätas upp” om de startar efter vokal #2
(def protected-suffixes
  ["ING" "ION" "TION" "SION" "IOUS" "IOUSLY" "IAL" "IC" "ISH" "ISM" "IST"
   "ABLE" "IBLE" "MENT" "NESS" "LESS" "FUL" "WARD" "WARDS" "LY" "ED" "ES" "ER" "EST" "OUS"
   ;; svenska
   "NING" "NINGEN" "NINGAR" "LIG" "LIGA" "LIGT" "HET" "HETEN" "HETER" "HETS"
   "ADE" "ANDE" "AR" "ARE" "ARNA" "ER" "EN" "ET" "ES" "OR" "ORS" "SKA" "SKT" "ELSE"])

;; skydda svenska sj/tj-kluster så vi inte sabbar dem
(def sv-protect-re (re-pattern "(SKJ|STJ|SCH|SJ|KJ|TJ)"))

(defn- starts-with-any? [^String s coll]
  (some #(str/starts-with? s %) coll))

(defn expand-drop-second-vowel
  "Ger #{BAS ALT?} där ALT = strängen med 2:a vokalen (A/E/O) borttagen
   om (i) ordet har ≥3 vokaler, (ii) v2 sitter mellan konsonanter,
   (iii) och direkt EFTER v2 börjar INTE ett skyddat suffix.
   Gäller både svenska och engelska. Skyddar sj/tj-kluster."
  [word]
  (let [s (str/upper-case word)
        n (.length s)]
    (if (or (< n 3) (re-find sv-protect-re s))
      #{s}
      (let [v-idxs (keep-indexed (fn [i ch] (when (vowels ch) i)) s)]
        (if (< (count v-idxs) 3)
          #{s}
          (let [v2i  (nth v-idxs 1)
                ch   (.charAt s v2i)
                prev (when (pos? v2i) (.charAt s (dec v2i)))
                next (when (< (inc v2i) n) (.charAt s (inc v2i)))
                ;; suffix ska börja direkt EFTER v2
                tail-after-v2 (subs s (inc v2i))
                consonant?    (fn [ c]
                                (let [u (Character/toUpperCase c)]
                                  (and (Character/isLetter u) (not (vowels u)))))
                droppable?
                (and (weak-vowels ch)
                     prev (consonant? prev)
                     next (consonant? next)
                     (not (starts-with-any? tail-after-v2 protected-suffixes)))]
            (if droppable?
              #{s (str (subs s 0 v2i) (subs s (inc v2i)))}
              #{s})))))))
(comment
  (expand-drop-second-vowel "chocolate")
(expand-drop-second-vowel "rasefullan"))
