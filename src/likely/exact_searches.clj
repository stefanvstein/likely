(ns likely.exact-searches
  (:require [clojure.string :as str]
            [likely.strings :as strings])
  )

;; används på andra ställen och ska förmodligen inte vara här. Kanske en konfig till sökning i search
(defn not-too-long-pred [input-len]
  (cond
    (> 2 input-len) (fn [x] (> 4 (count x)))
    (> 3 input-len) (fn [x] (> 5 (count x)))
    (> 4 input-len) (fn [x] (> 6 (count x)))
    (> 5 input-len) (fn [x] (> 8 (count x)))
    :else identity))


(defn- starts-with [string words]
  (let [length (count string)]
    (->> words
         (filter (not-too-long-pred length))
         (filter #(strings/starts-with? string %))
         (not-empty))))

(defn find-starts-with [string words]
  (when (not-empty string)
    (if (map? words)
      (->> (starts-with string (keys words))
           (select-keys words)
           (not-empty)
           strings/text-map)
      (-> (starts-with string words)
          strings/text-set))))

(comment
  (find-starts-with "oll" {"olle" {2 3}}))

(defn find-exact [string words]
  (if (map? words)
    (when-let [r (words string)]
      (assoc strings/empty-local-text-map string r))

    (when ((set words) string)
      (conj strings/empty-local-text-set string))))

(comment
  (= {"adam" 1} (find-exact "adam" {"adam" 1}))
  (= #{"adam"} (find-exact "adam" ["berit" "adam"]))
  (nil? (find-exact "adam" ["beril" "cesar"])))


(defn- contains [string words min-search-word-len min-match-word-len]
  (let [length (count string)]
    (when (>= length min-search-word-len)
      (->> words
           (filter #(let [l (count %)]
                      (and (>= l min-match-word-len)
                           (> l length))))
           (filter #(strings/includes? % string))
           (not-empty)))))

(defn find-contains
;;; Ta bort denna arity när den inte refereras
  ([string words]
   (find-contains string words 5 7))

  ([string words min-search-word-len min-match-word-len]
   {:pre [(some? min-match-word-len)
          (some? min-search-word-len)]}
   (if (map? words)
     (->> (contains string (keys words) min-search-word-len min-match-word-len)
          (select-keys words)
          (not-empty)
          (strings/text-map))
     (-> (contains string words min-search-word-len min-match-word-len)
         strings/text-set))))

(comment
  (find-contains "ollepolle" {"ollepollepo" 1 "gollepollepo" 2 "gollepopo" 2})
  (find-exact "olle" {"olle" 3 "polle" 4})
  
  (find-starts-with "olle" ["olleman"])
  (find-starts-with "olle" {"olleman" 3 "ollefi" 4 "adamski" 5}))
