(ns likely.strings
  (:require [clojure.string :as str]))

(defn starts-with? [ substr s]
  (when (and s substr)
    (str/starts-with? s substr)))

(defn includes? [s substr]
  (when (and s substr)
    (str/includes? s substr)))

(def empty-local-text-set
  #?(:clj
     (sorted-set-by (java.text.Collator/getInstance))
     :cljs
     (sorted-set-by
      (let [collator (js/Intl.Collator.)]
        (fn [a b] (.compare collator a b))))))

(defn text-set
  "A set sorted on text in local language, containing elements of col"
  [col]
  (when col (into empty-local-text-set col)))

(def empty-local-text-map
  #?(:clj (sorted-map-by
               (let [collator (java.text.Collator/getInstance)]
                 #(.compare collator %1 %2)))
     :cljs (sorted-map-by
               (let [collator (js/Intl.Collator.)]
                 (fn [a b] (.compare collator a b))))))

(defn text-map
  "Returns a sorted map where keys are compared using local language collator"
  [m]
  (when m
    (into empty-local-text-map m)))

(comment
  (text-map {"allan" 1 "cesar" :A "berit" [1 2]})
  (text-set ["c" "A" "f"]))
(defn split-spaces [text]
  (when text
    (str/split text #"\s+")))

(defn split-str
  "Splits string s on substring sep without regex.
   Removes trailing empty if separator was at end."
  [s sep]
  (when (and s sep)
    (let [seplen (count sep)]
      #?(:clj
         (loop [start 0 parts []]
           (let [idx (.indexOf ^String s sep start)]
             (if (neg? idx)
               (let [tail (.substring s start)]
                 (if (and (empty? tail)
                          (.endsWith ^String s sep))
                   parts
                   (conj parts tail)))
               (recur (+ idx seplen)
                      (conj parts (.substring s start idx))))))

         :cljs
         (loop [start 0 parts []]
           (let [idx (.indexOf s sep start)]
             (if (= idx -1)
               (let [tail (.substring s start)]
                 (if (and (empty? tail)
                          (.endsWith s sep))
                   parts
                   (conj parts tail)))
               (recur (+ idx seplen)
                      (conj parts (.substring s start idx))))))))))

(comment

  (split-str "AdamBertilCesar" "r")
  (split-str nil "r")
  (vec (.split "AdamBertilCesar" "Bertil")))
