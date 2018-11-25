(ns likely.fonetic-searches
  (:require [likely.mra :refer [mra-codex]]
            [likely.distance-searches :refer [find-with-distance]]
            [likely.exact-searches :refer [not-too-long-pred starts-with-f]]))

(defn size-filter [i]
  (fn [x]
    (let [l (count x)] 
      (cond
        (< i 3) (< l 5)
        (< i 4) (< l 7)
        (< i 6) (< l 12)
        (< i 10) (< l 17)
        :else true))))

(defn find-fonetic [q mra]
  (->> (mra-codex q)
       (map #(->> (first (mra-codex %))
                  (get mra)
                  (filter (size-filter (count %)))))
       (apply concat)
       (not-empty)))

(defn find-fonetic-distance [q n mra]
  (->> (mra-codex q)
       (map #(->> (find-with-distance % n mra)
                  (map (partial get mra))
                  (filter (size-filter (count q)))
                  (apply concat)))
       (apply concat)
       (not-empty)))

(defn find-starts-with-fonetic [q mra]
  (->> (mra-codex q)
       (map #(->> (keys mra)
                  (filter (starts-with-f %))
                  (map (partial get mra))
                  (apply concat)
                  (filter (not-too-long-pred (count q)))))
       (apply concat)
       (not-empty)))
