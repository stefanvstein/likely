(ns likely.distance-searches
  (:require [likely.damerau :refer [damerau]]
            [likely.exact-searches :refer [not-too-long-pred]]
            [likely.strings :refer [text-set text-map]]))



(defn- distance-find [string n words]
  (let [length (count string)
        close-enough #(let [diff (-> (count %)
                                     (- length)
                                     Math/abs)]
                        (>= n diff))
        distance-filter #(>= n (damerau string %))]
    (->> words
         (filter close-enough)
         (filter distance-filter)
         not-empty)))

(defn find-with-distance
  "Find word of string in words, where string i max n keystrokes from word. Words is either a seq of words of map where keys are words. Returns a set or of findings, or the map where keys are the findings"
  [string n words]
  (if (map? words)
    (->> (keys words)
         (distance-find string n)
         (select-keys words)
         (not-empty)
         text-map)
    (-> (distance-find string n words)
        (text-set))))

(defn- not-shorter-than-pred [len]
  (fn [x] (< len (count x))))

(defn- start-with-candidates [input-len xs]
  (->> xs
       (filter (not-shorter-than-pred input-len))
       (filter (not-too-long-pred input-len))))

(defn- start-distance-find [q n names]
  (let [qlen (count q)]
      (->> (start-with-candidates qlen names)
           (filter #(= n (damerau q % qlen)))
           (not-empty))))

(defn find-starts-with-distance
  "Find words that start with string where string i max n keystrokes from the start of the word. Words is either a seq of words of map where keys are words. Returns a set or of findings, or the map where keys are the findings"
  [string n words]
  (if (map? words)
    (->> (start-distance-find string n (keys words))
         (select-keys words)
         not-empty
         text-map)
    (-> (start-distance-find string n words)
        text-set)))


