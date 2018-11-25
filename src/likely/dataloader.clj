(ns likely.dataloader
  (:require [likely.mra :refer [mra-codex]])
  (:import [java.text BreakIterator]))

(def nonLetterOrDigits #"[^\p{javaUpperCase}\p{javaLowerCase}\p{Digit}]")

(defn split [s r]
  (when (and s r)
    (clojure.string/split s r)))

(defn split-spaces [text]
  (split text #"\s+"))

(defn names-in [data column-names]
  "Given a map, return a sequence of space separated values matching column-names "
  (->> (map #(get data %) column-names)
       (filter some?)
       (map split-spaces)
       (flatten)
       (map #(clojure.string/replace % nonLetterOrDigits ""))
       (map #(clojure.string/lower-case %))
       (map not-empty)
       (filter some?)))

(defn and-pred [& fns]
  (fn [& args]
    (every? #(apply % args) fns)))

(defn with-names-of-current-key [result current-key names]
  (reduce (fn [result name]
            (update-in result
                       [name]
                       #(if %
                          (conj % current-key)
                          #{current-key})))
          result
          names))

(defn with-mra-from-names [result names]
  (reduce (fn [a v]
            (reduce (fn [acc mra]
                      (update-in acc [mra] #(if %
                                              (conj % v)
                                              #{v})))
                    a (mra-codex v)))
          result names))

(defn with-key-of-names [result current-key names]
  (assoc result current-key names)) 

(defn create-data-from-resultset [data-set key-name column-names include-preds info-from-data]
  "creates searchable data from data-set, a seq of maps. 
   key-map is the key (column) defining the key
   column-names defines the columns were intersting names are found
   include-preds is a series of predicates defining what words to include
   info-from-data is a function returning the full info of a data-set entry"

  (reduce
   (fn [result data]
     (let [names (names-in data column-names)
           filtered-names (if include-preds
                            (filter (apply and-pred include-preds) names)
                            names)
           current-key (get data key-name)]
       (-> result
           (update-in [:ref] #(with-names-of-current-key % current-key filtered-names))
           (update-in [:mra] #(with-mra-from-names % filtered-names))
           (update-in [:id] #(with-key-of-names % current-key (info-from-data data))))))
   
   {}
   data-set))

(defn sentences [s]
  (let [bi (BreakIterator/getSentenceInstance)
        f (fn [start res]
            (let [end (.next bi)]
              (if (= end BreakIterator/DONE)
                res
                (recur end (conj res (.trim (.substring s start end)))))))]
    (.setText bi s)
    (f (.first bi) [])))

(defn create-data-from-sentences [text include-preds]
  (let [as-sentence-map (fn [x i] {:text x :key i})]
    (create-data-from-resultset (vec (doall  (map as-sentence-map (sentences text)
                                                  (range ))))
                                :key
                                [:text]
                                include-preds
                                :text)))
