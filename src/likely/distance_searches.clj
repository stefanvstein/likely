(ns likely.distance-searches
  (:require [likely.damerau :refer [damerau]]
            [likely.exact-searches :refer [not-too-long-pred]]))

(defn find-with-distance [q n names]
  (let [qlen (count q)
        fun  (fn [x] (when (>= n
                               (Math/abs (- (.length ^String x)
                                            qlen)))
                       (= n (damerau q x))))]
    (not-empty (filter fun (keys names)))))

(defn not-shorter-than-pred [len]
  (fn [x] (< len (count x))))

(defn start-with-candidates [input-len xs]
  (->> xs
       (filter (not-shorter-than-pred input-len))
       (filter (not-too-long-pred input-len))))
       
(defn find-starts-with-distance [q n names]
  (let [qlen (count q)]
    (->> (start-with-candidates qlen (keys names))    
         (filter #(= n (damerau q % qlen)))
         (not-empty))))

