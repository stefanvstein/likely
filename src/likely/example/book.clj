(ns likely.example.book
  (:require [hickory.core :as hickory]
            [clojure.string :as string]
            [clojure.edn :as edn]
            [clojure.set :as set]
            [likely.strings :as strings]
            [likely.mra :as mra]
            [likely.search :as search]
            [likely.example.debug :as debug]
            [clojure.java.io :as io]
            clojure.pprint)
  (:import [java.nio.file Files Paths LinkOption StandardCopyOption OpenOption]
           [java.io File]
           [java.util.stream Collectors]
           [java.util.zip ZipInputStream ZipEntry]))

(defmacro clock
  [msg  expr ]
  `(let [start# (. System (currentTimeMillis))
         ret# ~expr]
     (debug/debug! (str ~msg " in : "  (- (. System (currentTimeMillis)) start#) " msecs"))
     ret#))



(defn- unzip! [^java.nio.file.Path zip-path ^java.nio.file.Path target-dir]
  (with-open [zis (ZipInputStream. (Files/newInputStream zip-path
                                                         (make-array OpenOption 0)))]
    (loop []
      (when-let [^ZipEntry e (.getNextEntry zis)]
        (let [out (-> target-dir (.resolve (.getName e)))]
          (if (.isDirectory e)
            (Files/createDirectories out (make-array java.nio.file.attribute.FileAttribute 0))
            (do
              (Files/createDirectories (.getParent out)
                                       (make-array java.nio.file.attribute.FileAttribute 0))
              (Files/copy zis out (make-array java.nio.file.CopyOption 0))))
          (.closeEntry zis)
          (recur))))))

(defn ensure-books-dir!
  "Packar upp resources/books.zip till systemets temp/mydemo-books
   om det inte redan finns. Returnerar Path till katalogen."
  []
  (let [tmp-root (Paths/get (System/getProperty "java.io.tmpdir")
                            (into-array String ["clever-searching-books"]))

        zip-tmp  (Paths/get (System/getProperty "java.io.tmpdir")
                            (into-array String ["books.zip"]))]
    (debug/debug! "book directory:" (str tmp-root))
    (when (not (Files/exists tmp-root
                             (make-array LinkOption 0)))
      (debug/debug! "There are no books. Extracting from binary..")
      (Files/createDirectories tmp-root (make-array java.nio.file.attribute.FileAttribute 0))
      (with-open [in (io/input-stream (io/resource "books.zip"))]
        (Files/copy in zip-tmp (into-array java.nio.file.CopyOption
                                           [StandardCopyOption/REPLACE_EXISTING])))
      (unzip! zip-tmp tmp-root))
    tmp-root))


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

(defn clean-text [s]
  (-> s
      (string/replace #"\s+" " ")
      string/trim))



(defn extract-paragraphs [hick]
  (let [title (title hick)
        body (body-node hick)
        ;; Hitta alla p-taggar i hela body-trädet
        all-paragraphs (->> (tree-seq map? :content body)
                            (filter #(= (:tag %) :p))
                            (filter #(not (empty? (:content %)))))]
    (->> all-paragraphs
         (map #(->> (flatten-text %)
                    clean-text))
         (remove string/blank?)
         (map-indexed (fn [i txt]
                        {:chapter title
                         :paragraph i
                         :text txt}))
         vec)))


(defn path [dir & steps]
  (Paths/get dir (into-array String steps)))


(defn stream-to-list [s]
  (.collect s (Collectors/toList)))

(defn regular-file? [path]
  (Files/isRegularFile path (make-array LinkOption 0)))

(defn list-xhtml-files [dir]
  (let [path (path dir)
        files (stream-to-list (Files/list path))]
    (->> files
         (filter #(and (regular-file? %)
                       (or (.endsWith (str %) ".xhtml")
                           (.endsWith (str %) ".html"))))
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
       (map #(string/replace % non-letter-or-digits ""))
       (map string/lower-case)
       (filter (complement skips))
       (filter (complement empty?))))


(defn- update-tokens [m entry]
  (reduce (fn [m x]
            (update m x #(if % (conj % entry) #{entry})))
          m
          (:tokens entry)))

(defn- make-ref-map [entries skips name]
  (->> entries
       (map #(assoc %
                    :tokens (tokenize (:text %) skips)
                    :book name))
       (reduce update-tokens {})))

(defn- mra-words [words]
  (reduce (fn [a v]
            (reduce (fn [acc mra]
                      (update-in acc [mra] #(if %
                                              (conj % v)
                                              #{v})))
                    a (mra/mra-codex v)))
          {} words))

(defn- parse-file [file]
  (->> (slurp file)
       hickory/parse
       hickory/as-hickory
       extract-paragraphs))


(defn- parse-book [directory]
  (->> (list-xhtml-files directory)
       (mapcat parse-file)
       vec))


(defn- store-parsed-book [events]
  (let [a (with-out-str (clojure.pprint/pprint events))]
    (spit "book.edn" a)))

(defn load-skips [book]
  (let [file (path book "skip.edn")]
    (if (regular-file? file)
      (set (edn/read-string (slurp (str file))))
      #{})))

(defn- load-book
  [bookdir book]
  (let [_ (debug/debug! (str "Reading " book))
        paragraphs (parse-book (str (File. bookdir book)))
        skips (load-skips book)
        ref (clock "make references" (make-ref-map paragraphs skips book))
        mra (clock "make phonetics" (mra-words (keys ref)))]

    {:ref ref :mra mra}))


(defn merge-books [books]
  (reduce (fn [a book]
            (-> a
                (update :ref #(merge-with into % (:ref book)))
                (update :mra #(merge-with into % (:mra book)))))
          {}
          books))

(defn list-dirs [temp-dir]
  (->> (.listFiles temp-dir)
       (filter #(.isDirectory %))
       (map #(.getName %))))

(defn load-books
  []
  (debug/debug! "Loading books!!")
  (let [books-dir (.toFile (ensure-books-dir!))]
    (->> (list-dirs books-dir)
         (map #(load-book books-dir %))
         (merge-books))))

(comment
  (count  (parse-book "In Freedoms Cause")))

(defn book []
  (or @book*
      (reset! book* (load-books))))

(defn- reload-book []
  (reset! book* (load-books))
  nil)

(defn search [q]
  (println "Searching" q)
  (let [r (search/search (book) q)]
    (println "Done")
    r))

(defn refs
  ([word]
   ((:ref (book)) word)))



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
         (- (count (set/intersection tokens query-set)))))
     deduped)))

(defn sort-paragraphs-on-words [words paragraphs]
  (deduped-paragraphs-sorted-by-matches 
   [:book :chapter :paragraph]
   words
   paragraphs))

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
  (reset! book* nil)
                                        ;(store-parsed-book (parse-book))
  (reload-book)
  (spit "dump.edn" (with-out-str (clojure.pprint/pprint (book))))
  (search "")
  (map :chapter (refs "arthur"))
  (mra/mra-codex "planet’s")
  (let [a (clock "slurp book" (edn/read-string (slurp "book.edn")))]
    nil)
  ;(let [a (time "parse book" (parse-book))] nil)
  
  ((set (keys (:ref (book)))) ""))
