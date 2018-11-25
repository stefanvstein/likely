(ns likely.exact-searches)

(defn not-too-long-pred [input-len]
  (cond
    (> 2 input-len) (fn [x] (> 4 (count x)))
    (> 3 input-len) (fn [x] (> 5 (count x)))
    (> 4 input-len) (fn [x] (> 6 (count x)))
    (> 5 input-len) (fn [x] (> 8 (count x)))
    :otherwise identity))

(defn starts-with? [ substr s]
  (when (and s substr)
    (clojure.string/starts-with? s substr)))

(defn starts-with-f [substr]
  (partial starts-with? substr))

(defn find-starts-with [q names]
  (let [qlen (count q)]
    (->> (keys names)
         (filter (not-too-long-pred qlen))
         (filter (starts-with-f q))
         (not-empty))))

(defn find-exact [q names]
  (when (contains? names q)
    #{q}))
    
(defn includes? [s substr]
  (when (and s substr)
    (clojure.string/includes? s substr)))

(def ^{:private true :const true} min-contains-len 7)
(def ^{:private true :const true} min-contains-q-len 5)

(defn find-contains [q names]
  (let [qlen (count q)]
    (if (>= qlen min-contains-q-len)
      (->> (keys names)
           (filter #(let [l (count %)]
                      (and (>= l min-contains-len)
                           (> l qlen))))
           (filter #(includes? % q))
           (not-empty))
      nil)))

