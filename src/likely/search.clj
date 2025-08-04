(ns likely.search
   (:require [likely.fonetic-searches :as fonetic]
            [likely.distance-searches :as distance]
            [likely.exact-searches :as exact]
            [likely.strings :as strings]
            [clojure.string :as str]))


(defn words [s]
  (-> (when (string? s)
        (strings/split-spaces s))
      (set)
      ;;Ska det ske nån filtrering här? Förmodligen inte
      ))



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


(defn tiny-searches [word]
  [{:algo :exact :query word :points 1000 }
   {:algo :exact-start :query word :points 200}])

(defn mid-searches [word]
  [{:algo :exact :query word :points 1000}
   {:algo :exact-start :query word :points 200}
   {:algo :distance :length 1 :query word :points 10}
   {:algo :fonetic :query word :points 6}])

(defn longer-searches [word]
  [{:algo :exact :query word :points 1000}
   {:algo :exact-start :query word :points 200}
   {:algo :distance :length 1 :query word :points 10}
   {:algo :fonetic :query word :points 10}
   {:algo :start-distance :query word :length 1 :points 3}
   {:algo :fonetic-distance :query word :length 1 :points 1}
   {:algo :start-with-fonetic :query word :points 0}
   {:algo :contains :query word :min-word-length 5 :min-match-word-length 7 :points 150}
{:algo :contains :query word :min-word-length 4 :min-match-word-length 5 :points 100}])

(defn searches-for-word [word]
  (let [length (count word)]
    (cond
      (> 3 length) (tiny-searches word)
      (> 4 length) (mid-searches word)
      :else (longer-searches word))))

(defn searches [question]
  (let [search-words (words question)]
    (mapcat searches-for-word search-words)))

#_(
  (searches  "ya gola"))

(defn search-with-spec [{:keys [algo query length min-word-length min-match-word-length] :as _search-spec}
                        {:keys [ref mra] :as _data}]
   (condp = algo
       :exact (exact/find-exact query ref)
       :exact-start  (exact/find-starts-with query ref)
       :distance (distance/find-with-distance query length ref)
       :fonetic (fonetic/find-fonetic query mra ref)
       :start-distance (distance/find-starts-with-distance query length ref)
       :fonetic-distance (fonetic/find-fonetic-distance query length mra ref)
       :start-with-fonetic (fonetic/find-starts-with-fonetic query mra ref)
       :contains (exact/find-contains query ref min-word-length min-match-word-length)
       nil)
  )

(defn update-when-greater-points
  "update value of k in m when v is greater than value of k, or k does not exist"
  [m search-result search-spec]
  (update m (key search-result) #(if (and  % (> (:points % 0) (:points search-spec 0))) % (assoc search-spec :refs (val search-result) :word (key search-result)))))

(comment
  (update-when-greater-points {:b {}} :b {}))

(defn perform-search [data already-found-with-points search-spec]
  (let [findings (search-with-spec search-spec data)]
    (reduce #(update-when-greater-points %1 %2 search-spec)
            already-found-with-points
            findings)))

(comment
  (perform-search testdata
                  {}
                  {:algo :fonetic
                   
                   :query "adam"
                                      
                                      }))

(defn perform-searches
  "returns map of word and points, where points is the highest point scored for a search"
  [search-specs data]
  (reduce (partial perform-search data) {} search-specs))

(defn as-value-and-points [xs]
  (-> (map #(array-map :value (key %) :points (:points (val %) 0)) xs)
      (as-sorted-per :points :value)))


(defn words-with-common-group [words group-lookup]
  (let [lookup-to-pairs (fn [word] (map #(vector word %) (group-lookup word)))
        word-group-pairs (mapcat lookup-to-pairs words)

        words-per-group (reduce
                         (fn [a [word group]]
                           (update a group #(if % (conj % word) #{word})))
                         {}
                         word-group-pairs)
        more-than-one-common (filter #(> (count %) 1) (vals words-per-group))]
    (into #{} (mapcat identity more-than-one-common))))

(defn update-extra-for-common [m points]
  (let [extras (words-with-common-group (keys m) (fn [k] (:refs (m k))))]
    (println "Extras:" extras)
    (reduce (fn [a v]
              (update a v #(assoc % :points (+ points (:points % 0)))))
            m
            extras)))
(defn- do-search [question data]
  (let [s (searches question)]
    (perform-searches s data)))

(defn search [question data]
  (let [
        r (do-search question data)
        
        ;; Hmm give extra points fort those that have same ref
        
        with-extra (update-extra-for-common r 1)
        _ (clojure.pprint/pprint with-extra)
        a (as-value-and-points with-extra)
        ]
    (map :value a)
    ))


(comment
  (search "tilson morgan" testdata)
  (search "bertil" testdata)
  (require '[likely.mra :as mra])
  (mra/mra-codex "bertil")
  (mra/mra-codex "bertol")
  (search "bertol" testdata)
  (search "adim" testdata)
                                        ;**************
  (def testresult
    {"nilsonn" {:algo :start-distance, :query "tilson", :length 1, :points 3},
     "nilsson"  {:algo :fonetic-distance, :query "tilson", :length 1, :points 1},
     "nilssson" {:algo :fonetic-distance, :query "tilson", :length 1, :points 1},
     "bertilson" {:algo :contains,
                  :query "tilson",
                  :min-word-length 5,
                  :min-match-word-length 7,
                  :points 150},
     "morgan" {:algo :exact, :query "morgan", :points 1000},
     "marianne" {:algo :fonetic-distance, :query "morgan", :length 1, :points 1}})

  (def testdata
    {:ref
     {"daam"       #{3},
      "nilsson"    #{5 4},
      "adam"       #{1 6},
      "nilsonn"    #{6},
      "aadm"       #{2},
      "morgan"     #{4},
      "marianne"   #{5},
      "nilssson"   #{3},
      "bertilson"  #{2},
      "bertilsson" #{1},
      "bedrup"     #{4}},
     :mra
     {"ADM"    #{"adam" "aadm"},
      "BIRLSN" #{"bertilson" "bertilsson"},
      "DAM"    #{"daam"},
      "NILSN"  #{"nilsson" "nilsonn" "nilssson"},
      "MARGN"  #{"morgan"},
      "BIDRP"  #{"bedrup"},
      "MARN"   #{"marianne"}}})

  (words-with-common-group
   (keys testresult)
   (:ref testdata))

                                    ;;;----
  )
