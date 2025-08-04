(ns likely.normalize
  #?(:cljs (:require [clojure.string :as str])))

(defn normalize-accents [s]
  #?(:clj
     (try
       ;; JVM: använd Normalizer om det finns
       (let [normalized (java.text.Normalizer/normalize s java.text.Normalizer$Form/NFD)]
         (.replaceAll normalized "\\p{InCombiningDiacriticalMarks}+" ""))
       (catch Exception _
         ;; Babashka fallback: gör ingenting
         s))

     :cljs
     ;; Browser: använd JS normalize
     (-> s
         (.normalize "NFD")
         (str/replace #"[\\u0300-\\u036f]" ""))))
