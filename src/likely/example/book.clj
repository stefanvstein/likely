(ns likely.example.book
  (:require [hickory.core :as hickory]
            [clojure.string :as string]
            [clojure.edn :as edn]
            [likely.strings :as strings]
            [clojure.string :as str]
            [likely.mra :as mra]
            [likely.search :as search]
            clojure.pprint)

  (:import [java.nio.file Files Paths LinkOption]
           [java.util.stream Collectors]))

(defmacro time
  [msg expr]
  `(let [start# (. System (currentTimeMillis))
         ret# ~expr]
     (println (str ~msg " in : "  (- (. System (currentTimeMillis)) start#) " msecs"))
     ret#))

(defonce book* (atom nil))

(defn find-tag [hick tag]
  (first (filter #(= (:tag %) tag)
                 (tree-seq map? :content hick))))
(defn title [hick]
  (let [head-tag (find-tag hick :head)

        title-tag (find-tag head-tag :title)]

    (first (:content title-tag))))

(defn body-node [hick]
  (find-tag hick :body))

(defn is-empty-paragraph? [node]
  (and (map? node)
       (= (:tag node) :p)
       (or (nil? (:content node))
           (empty? (:content node)))))

(defn flatten-text [node]
  (cond
    (string? node) node
    (map? node) (->> (:content node)
                     (map flatten-text)
                     (apply str))
    (sequential? node) (->> node (map flatten-text) (apply str))
    :else ""))

(defn replaceline-with-space [s]
  (string/replace s #"\n" " ")
)


(defn extract-paragraphs [hick]
  (let [title (title hick)
        segments (:content (body-node hick))
        cleaned (remove #(and (map? %) (= (:tag %) :h2)) segments)
        paragraph-groups (partition-by is-empty-paragraph? cleaned)
        text-groups (remove #(is-empty-paragraph? (first %)) paragraph-groups)]

    (->> text-groups
         (map #(->> (map flatten-text %)
                    (apply str)
                    clojure.string/trim
                    replaceline-with-space)
              )
         (remove clojure.string/blank?)
         (map #(do {:chapter title :text %2 :paragraph %1}) (range))
         vec)))

(defn path [dir]
  (Paths/get dir (make-array String 0)))

(defn stream-to-list [s]
  (.collect s (Collectors/toList)))

(defn regular-file? [path]
  (Files/isRegularFile path (make-array LinkOption 0)))

(defn list-xhtml-files [dir]
  (let [path (path dir)
        files (stream-to-list (Files/list path))]
    (->> files
         (filter #(and (regular-file? %)
                       (.endsWith (str %) ".xhtml")))
         (map str)
         (sort)
         vec)))

(def fancy-apostrophes "['’ʻʾʿ＇]")

(def non-letter-digit-or-apostrophes
  (re-pattern (str "[^" fancy-apostrophes "\\p{L}\\p{Nd}]")))

(def non-letter-or-digits
  (re-pattern "[^\\p{L}\\p{Nd}]"))

(defn tokenize [str skips]
  (->> (strings/split-spaces str)
       (map #(str/replace % non-letter-or-digits ""))
       (map str/lower-case)
       (filter (complement skips))
       (filter (complement empty?))))


(defn- update-tokens [m entry]
  (reduce (fn [m x]
            (update m x #(if % (conj % entry) #{entry})))
          m
          (:tokens entry)))

(defn- make-ref-map [entries skips]
  (->> entries
       
       (map #(assoc % :tokens (tokenize (:text %) skips)))
       (reduce update-tokens {})))

(defn- mra-words [words]
  (reduce (fn [a v]
            (reduce (fn [acc mra]
                      (update-in acc [mra] #(if %
                                              (conj % v)
                                              #{v})))
                    a (mra/mra-codex v)))
          {} words))

(defn- parse-book []
  (->> (list-xhtml-files "book")
       (mapcat #(->> (slurp %)
                     hickory/parse
                     hickory/as-hickory
                     extract-paragraphs
                     ))
       vec))


(defn- store-parsed-book [events]
  (let [a (with-out-str (clojure.pprint/pprint events))]
    (spit "book.edn" a)))

(defn load-skips []
  (if (regular-file? (path "book/skip.edn"))
    (set (edn/read-string (slurp "book/skip.edn")))
    #{}))

(defn- load-book []
  (println "Loading book")
  (let [paragraphs (parse-book)
        skips (load-skips)
        ref (time "make ref" (make-ref-map paragraphs skips))
        mra (time "make mra" (mra-words (keys ref)))]
    (println "Done")
    {:ref ref :mra mra }))

(defn- book []
  (or @book*
      (reset! book* (load-book))))

(defn- reload-book []
  (reset! book* (load-book))
  nil)

(defn search [q]
  (println "Searching")
  (let [r (search/search (book) q)]
    (println "Done")
    r))

(defn refs [word]
  ((:ref (book)) word))



(defn deduped-paragraphs-sorted-by-matches
  "Sorterar och deduplicerar paragraf-mappar utifrån en lista query-words och nycklar att särskilja på"
  [dedup-keys query-words paragraphs ]
  (let [query-set (set query-words)
        deduped (->> paragraphs
                     (reduce (fn [m p]
                               (let [k (select-keys p dedup-keys)]
                                 (if (contains? m k)
                                   m
                                   (assoc m k p))))
                             {})
                     vals)]
    (sort-by
     (fn [p]
       (let [tokens (set (:tokens p))]
         (- (count (clojure.set/intersection tokens query-set)))))
     deduped)))



(comment
  (deduped-paragraphs-sorted-by-matches
   [:book :chapter :paragraph]
   ["zaphod" "trillian"]
   [{:chapter "1" :paragraph 1 :tokens ["zaphod" "ford" "trillian"]}
    {:chapter "1" :paragraph 2 :tokens ["marvin" "robot"]}
    {:chapter "1" :paragraph 3 :tokens ["zaphod" "marvin"]}])
  (->> (sort-by #(- 0 (count (val %))) (:ref (book)))
       (map (fn [[a b]] [a (count b)]))
       (take 100))
  (refs "zaphod")
  (store-parsed-book (parse-book))
  (reload-book)
  (spit "dump.edn" (with-out-str (clojure.pprint/pprint (book))))
  (search "")
  (map :chapter (refs "arthur"))
  (mra/mra-codex "planet’s")
  (let [a (time "slurp book" (edn/read-string (slurp "book.edn")))]
    nil)
  (let [a (time "parse book" (parse-book))] nil)
  (let [a  (load-book)] nil)
  ((set (keys (:ref (book)))) ""))
