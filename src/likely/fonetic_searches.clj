(ns likely.fonetic-searches
  (:require [likely.mraplus :refer [mra-plus english-rules]]
            [likely.strings :as strings]
            [likely.distance-searches :refer [find-with-distance]]
            [likely.exact-searches :refer [not-too-long-pred ]]))

(defn size-filter [i]
  (fn [x]
    (let [l (count x)] 
      (cond
        (< i 3) (< l 5)
        (< i 4) (< l 7)
        (< i 6) (< l 12)
        (< i 10) (< l 17)
        :else true))))

(defn mra-codex [q]
  (mra-plus q [english-rules]))
;;Hitta referenser istället för nyklar med hjälp av mra. MRA träffar i sig är inte intressant
(defn find-fonetic
  ([q mra]
   (->> (mra-codex q)
        (mapcat #(->>
                  (mra %)
                  (filter (size-filter (count %)))))
        (not-empty)))
  ([q mra ref]
   (if (map? ref)
     (->> (find-fonetic q mra)
          (select-keys ref)
          (not-empty)
          (strings/text-map))
     (->> (find-fonetic q mra)
          (filter (set ref))
          (not-empty)
          (strings/text-set)))))

(comment
  (let [olleman (first (mra-codex "olleman"))
        alleman (first  (mra-codex "alleman"))
        galen (first (mra-codex "galen"))]
    (assert (= alleman olleman))
    (find-fonetic "ollemon" {olleman ["olleman" "alleman"] galen ["galen"]})
    #_(find-fonetic "olleman"
                    {olleman ["olleman" "alleman"]
                     galen ["galen"]}
                    {"olleman" #{1 2}
                     "alleman" #{3}})))


(defn find-fonetic-distance
  ([q n mra]
   (->> (mra-codex q)
        (map #(->> (find-with-distance % n mra)
                   (vals)
                   (reduce into [])
                   (filter (size-filter (count q)))
                   ))
        (apply concat)
        (not-empty)))
  ([q n mra ref]
   (if (map? ref)
     (->> (find-fonetic-distance q n mra)
          (select-keys ref)
          (not-empty))
     (->> (find-fonetic-distance q n mra)
          (filter (set ref))
          (not-empty)))
   ))

(comment
  (let [olleman (first (mra-codex "olleman"))
        alleman (first  (mra-codex "alleman"))
        galen (first (mra-codex "galen"))]
    (assert (= alleman olleman))
    (find-fonetic-distance  "polleman" 1 {olleman ["olleman" "alleman"]
                                          galen  ["olleman"]})
    (find-fonetic "olleman"
                  {olleman ["olleman" "alleman"]
                   galen ["galen"]}
                  {"olleman" #{1 2}
                   "alleman" #{3}})
    (find-fonetic "olleman"
                  {olleman ["olleman" "alleman"]
                   galen ["galen"]}
                  [
                   "alleman"])))

(defn find-starts-with-fonetic
  ([q mra]
   (->> (mra-codex q)
        (map (fn [codex]
               (->> (keys mra)
                    (filter  #(strings/starts-with? codex %))
                    (map (partial get mra))
                    (reduce into [])
                    (filter (not-too-long-pred (count q))))))
        (reduce into [])
        (not-empty)))
  ([q mra ref]
   (if (map? ref)
     (->> (find-starts-with-fonetic q mra)
          (select-keys ref)
          (not-empty))
     (->> (find-starts-with-fonetic q mra)
          (filter (set ref))
          (not-empty)))))

(comment
  (let [olleman (first (mra-codex "olleman"))
        alleman (first  (mra-codex "alleman"))
        galen (first (mra-codex "galen"))]
    (assert (= alleman olleman))
    (mra-codex "ollem")
    (find-starts-with-fonetic "ollem" {olleman ["olleman" "alleman"]
                                        galen  ["olleman"]})
    (find-starts-with-fonetic "ollem"
                  {olleman ["olleman" "alleman"]
                   galen ["galen"]}
                  {"olleman" #{1 2}
                   "alleman" #{3}})
    (find-starts-with-fonetic "olleman"
                  {olleman ["olleman" "alleman"]
                   galen ["galen"]}
                  [
                   "alleman"])))
