(ns likely.quicksearch
  (:require [likely.fonetic-searches :as fonetic]
            [likely.distance-searches :as distance]
            [likely.exact-searches :as exact]))


(defn text-set
  "A set sorted on text in local language, containing elements of col"
  [col]
  (when col
    (into (sorted-set-by (java.text.Collator/getInstance))
          col)))

(defn exact-search [names]
  [[(fn find-ex [q ] (exact/find-exact q names)) :exact-match]])

(defn basic-searches [mra names]
  [[(fn find-distance [q ] (text-set (distance/find-with-distance q 1 names)) ) :basic-distance]
   [(fn find-fon [q ] (text-set (fonetic/find-fonetic q mra)) ) :basic-fonetic]])

(defn start-only-exact [names]
  [[(fn find-starts [q ] (text-set (exact/find-starts-with q names))) :start-only]])

(defn contains-searches [names]
  [[#(text-set (exact/find-contains % names)) :contains]])

(defn start-searches [mra names]
  [[#(text-set (exact/find-starts-with % names)) :start-with]   
   [#(text-set (distance/find-starts-with-distance % 1 names)) :start-distance]
   [#(text-set (fonetic/find-fonetic-distance % 1 mra)) :fonetic-distance]
   [#(text-set (fonetic/find-starts-with-fonetic % mra)) :start-fonetic]])



(defn sort-by-lookup [data comp table field]
  (sort-by #(table (field %)) comp data))

(defn sort-by-points [funs points]
  (sort-by-lookup funs > points second))

(defn tiny-searches [names points]
  (-> (concat (exact-search names)
              (start-only-exact names))
      (sort-by-points points)))

(defn mid-searches [mra names points]
  (-> (concat (exact-search names)
              (basic-searches mra names)
              (start-only-exact names))
      (sort-by-points points)))

(defn longer-searches [mra names points]
  (-> (concat (exact-search names)
              (basic-searches mra names)
              (contains-searches names)
              (start-searches mra names))
      (sort-by-points points)))

(defn simple-search-functions [mra names points q-len ]
  (cond (> 3 q-len) (tiny-searches names points)
        (> 4 q-len) (mid-searches mra names points)
        :else (longer-searches mra names points)))

(defn without [st elems]
  (-> (apply disj st elems)
      (not-empty)))

(defn search-run
  ([q search-funs]
   (search-run q search-funs #{}))
  ([q search-funs previously-found]
   (let [[[fun weight] & remaining] search-funs
         found (when-let [items (fun q)]
                 (without items previously-found))
         used (into previously-found found)
         value {:algo weight :q q :match found}]
     (cond
       (and found remaining) (cons value (lazy-seq (search-run q remaining used)))
       remaining             (recur q remaining used)
       found                 (list value)))))

(defn point [point-table data]
  (assoc data :points (or (point-table (:algo data))
                          0)))

(defn split [s r]
  (when (and s r)
    (clojure.string/split s r)))

(defn split-spaces [text]
  (split text #"\s+"))


(defn weighted-search [q funs]
  (->> (funs (count q))
       (search-run q))
                            )
(defn weighted-searches [qs funs] 
  (mapcat #(weighted-search % funs) qs))

(defn expand-per-value-as [maps value as]
  (let [as-many-matches #(map (partial assoc % as)
                              (value %))]
    (reduce #(apply conj %1 (as-many-matches %2))
            #{}
            maps)))

(defn strings [s]
  (-> (if (string? s)
        (split-spaces s)
        s)
      (set)))

(defn comp-by-keys [& ks]
        (if (empty? ks)
          #(if (and (empty? %1) (empty? %2))
             0 1)
          (let [collator (java.text.Collator/getInstance)]
            (partial (fn com [[current & remaining] a b ]
                         (if current
                           (let [av (current a)
                                 bv (current b)
                                 value  (- 0 (cond
                                               (string? av) (.compare collator av bv)
                                               :otherwise (compare av bv)))]
                             (if (= 0 value)
                               (recur remaining a b )
                               value))
                           0))
                       ks))))

(defn as-sorted-per [data & ks]
  (into (sorted-set-by (apply comp-by-keys ks)) data))

#_( public api)

(defn ids-of-ref [data name]
  [name (-> ((:ref data) name)
            (vec))])

(defn with-ids [names data]
  (map (partial ids-of-ref data) names))

(def function-points
  {:exact-match 100000
   :basic-distance 10
   :basic-fonetic 6
   :start-only 1
   :start-with 200
   :contains 150
   :start-distance 3
   :fonetic-distance 1
   :start-fonetic 0})

(use 'clojure.pprint)

(defn search
  ([q fonetic names points]
   (let [search-funs (partial simple-search-functions fonetic names points)
         expand-each-match-as-value #(expand-per-value-as % :match :value )
         sorted-per-point-and-value #(as-sorted-per % :points :value)]
     (->> search-funs
          (weighted-searches (strings q))
          (map (partial point points))
          (expand-each-match-as-value)
          (sorted-per-point-and-value)
          (map :value))))
  ([q data]
   (search q (:mra data) (:ref data) function-points)))

