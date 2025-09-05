(ns likely.mraplus
  "A substitution based fonetic encoder optimized for scandinavian countries, 
   knowing swedish and english, while trying to respect norwegian and danish" 
  (:require
   [likely.normalize :as normalize]
   [likely.mraplusvoweldrop :refer [expand-drop-second-vowel]]))

(defn- normalize-vowels
  "Slå ihop I/E/Y→I, Å→O. Lämna övriga."
   [^String s]
  (let [xf (fn [^Character ch]
             (case ch
               \Y \U
               \Å \O
               \Ä \E
               \Æ \E
               \Ø \O
               \Ö \O
               ch))]
    (apply str (map xf s))))

(defn- normalize-consonants [s]
  (let [xf (fn [^Character ch]
             (case ch
               \B \P
               \D \T
               \G \K
               \C \X
               \J \X
               \M \N
               \Z \S
               \W nil
               \V nil
               \R nil
               \H nil
               \Q nil
               ch))]
    (apply str (map xf s))))

(defn take-codex-letters
  "Ta upp till head-count första + upp till tail-count sista tecken, utan överlapp."
  ^String [^String s head-count tail-count]
  (let [n (count s)]
    (if (<= n head-count)
      s
      (let [head (subs s 0 head-count)
            tail (subs s (max head-count (- n tail-count)) n)] ; starta svansen efter huvudet
        (str head tail)))))


(defn starts-with-at? [^String s i ^String pat]
  (let [m (count pat)]
    (and (<= (+ i m) (count s))
         (.regionMatches s i pat 0 m))))

(defn first-hit
  [^String s i rules]
  (some (fn [{:keys [pat out at-start? at-end?]}]
          (let [start? (= i 0)
                end? (= (+ i (count pat)) (count s))]
            (when (and (if (some? at-start?)
                         (= at-start? start?) true)
                       (if (some? at-end?)
                         (= end? at-end?) true)
                       (starts-with-at? s i pat))
              {:len (count pat)
               :translations (cond (string? out) [out]
                                   :else out)})))
        rules))

(defn extend-each-production [xs extensions]
  (reduce (fn [out prod]
            (reduce (fn [o r]
                      (let [ext-prod (str prod r)]
                        (conj o ext-prod)))
                    out
                    extensions))
          [] xs))

(defn expand-by-rules
  [^String s rules]
  (let [total-len (count s)]
    (loop [i 0, acc [""]]
      (if (= i total-len)
        acc
        (if-let [{:keys [len translations]} (first-hit s i rules)]
          (recur (+ i len)
                 (extend-each-production acc translations))
          (recur (inc i)
                 (mapv #(str % (.charAt s i)) acc)))))))

(comment
  (let [rules [{:pat "ab" :out ["1" "2"] :at-end false :at-start? true}
               {:pat "c" :out ["4"] :at-end? false :at-start? false}]]
    (expand-by-rules "abc" rules)))

(defn dedupe-consecutive [s]
  (->> s (partition-by identity) (map first) (apply str)))


(comment
  (dedupe-consecutive "hallandllal"))

(defn keep-n-of
  "Behåll högst n förekomster av tecken ur xs i s. Övriga tecken behålls alltid.
   xs kan vara en sträng eller en sekvens av tecken."
  [n xs ^String s]
  (let [S   (set xs) ;; funkar för både sträng och seq
        n   (long (max 0 n))
        len (count s)
        sb  (StringBuilder. len)]
    (loop [i 0, left n]
      (if (= i len)
        (.toString sb)
        (let [ch (.charAt s i)]
          (if (and (contains? S ch) (neg? (dec left)))
            ;; ch är i xs men vi har slut på kvoten -> hoppa över
            (recur (unchecked-inc i) left)
            ;; annars behåll tecknet, och om det ingår i xs drar vi ner kvoten
            (do (.append sb ch)
                (recur (unchecked-inc i)
                       (if (contains? S ch) (dec left) left)))))))))

(defn mra-plus-of-string [word rulesets  {:keys [vowels head tail norm? norm-vowels?] :or {vowels 2 head 3 tail 3 norm? true norm-vowels? true}}]
  (->> (mapcat #(expand-by-rules word %) rulesets) ; union över språk efter expansion
       (map #(keep-n-of vowels "AEIOUYÅÄÖÆØ" %))
       (map dedupe-consecutive)
       (map #(take-codex-letters % head tail))
       (map #(if norm? (normalize-consonants %) %))
       (map #(if norm-vowels? (normalize-vowels %) %))
       (map dedupe-consecutive)))

(defn mra-plus
  "Phonetics with expansions, keeping first 2 vowels, shortened to first and last 3"
  [word rulesets & settings]
  (let [s (normalize/normalize word "ÅÄÖÆØ")]
    (->> (expand-drop-second-vowel s)
         (mapcat #(mra-plus-of-string % rulesets settings))
         (set)
         )))

(defn phonetic-of-string [rulesets s]
  (->> rulesets
       (mapcat #(expand-by-rules s %)) ; union över språk efter expansion
       (map dedupe-consecutive)))

(defn phonetic [word rulesets]
  (let [s (normalize/normalize word "ÅÄÖÆØ")]
    (->> (expand-drop-second-vowel s)
         (mapcat #(phonetic-of-string rulesets %))
         (set))))

(comment
  (into #{} [1 2 3]))


#_(comment
    A ≈ IPA [a], [ɑ] (kort/långt a, sv. mat)
    E ≈ IPA [e], [ɛ] (kort/långt e, kort ä)
    I ≈ IPA [i] (sv. vit)
    O ≈ IPA [o], [ɔ] (sv. sol, boll)
    U ≈ IPA [ʉ] (sv. hus) och ibland [u] (eng. boot)
    Y ≈ IPA [y] (sv. ny)
    Å ≈ IPA [oː], [ɔː] (sv. båt)
    Ä ≈ IPA [æ], [ɛː] (sv. är, eng. cat ~lång ä)
    Ö ≈ IPA [ø], [œ] (sv. röd)

    B, D, F, G, K, L, M, N, P, R, S, T, V  – direkt svensk mappning.

    C = tje-ljud ([ɕ], [t͡ɕ], [ɧ], [ʃ], [x])
    X = sch-ljud (engelska sh i skotska loch eller svenska skepp)
    J = tonande tje-ljud som i engelska dodge)

(def english-rules
  [{:pat "TCH", :out  "C"}
   {:pat "DGE", :at-end? true :out  "J"}
   {:pat "ISL", :out  "AIL" :at-start? true}
   {:pat "AIL" :out "EIL"}
   {:pat "ALE" :out "EIL"}
   {:pat "EIGH", :out  "EI"}
   {:pat "AY", :out ["EI"]}
   {:pat "LAUGH" :out ["LAF"]}
   {:pat "AUGH", :out ["Å"]}
   {:pat "THROUGH" :out ["TRU"]}
   {:pat "PLOUGH" :out ["PLAU"]}
   {:pat "ROUGHT" :at-end? true :out ["RÅT"]}
   {:pat "OUGHT" :at-end? true :out ["ÅT"]}
   {:pat "TOUGH" :at-end? true :out ["TAF"]}
   {:pat "ENOUGH" :at-end? true :out ["ENAF"]}
   {:pat "QUEUE" :out  "KU"}
   {:pat "OUGH" :at-end? true :out ["O"]}
   {:pat "URE" :out ["Ö" "ER"] :at-end? true}
   {:pat "OUGH", :out ["O" "OF" "Å" "U"]}
   {:pat "YACHT", :out "IÅT"}
   {:pat "GH", :out ["F" ""]}
   {:pat "GLE", :out ["GLE" "GEL"]}
   {:pat "IPT", :out ["IPT" "IT"]}
   {:pat "PLY" :at-end? true :out ["PLAI"]}
   {:pat "JULY" :at-end? true :out ["IULAI"]}
   {:pat "LY" :at-end? true :out ["LI"]}
   ;; tysta initiala kluster
   {:pat "KN", :at-start? true :out ["N"]}
   {:pat "GN", :at-start? true :out ["N"]}
   {:pat "WR", :at-start? true :out ["R"]}
   {:pat "PS", :at-start? true :out ["S"]}
   {:pat "WH", :at-start? true :out ["V"]}
   {:pat "X" :at-start? true :out ["S"]}

   {:pat "PN", :out ["N"]} ;;?
   {:pat "PT", :out ["T"]} ;;?
   {:pat "KE", :out ["K" "KE"]} ;;?
   {:pat "MB", :at-end? true :out ["M"]}
   {:pat "NG" :out ["N" "NG"]}
   {:pat "TH", :out ["T" "D"]}
;; slutlig tystnad
   {:pat "NEW" :out "NU"}
   {:pat "DEW" :out "DU"}
   {:pat "TEW" :out "TU"}
   {:pat "LEW" :out "LU"}

   {:pat "EW" :out "IU"}
   {:pat "XES" :at-end? true :out "SIS"}
   {:pat "SES" :at-end? true :out "SIS"}
   {:pat "ZES" :at-end? true :out "SIS"}
   {:pat "SHES" :at-end? true :out "CIS"}
   {:pat "CHES" :at-end? true :out "CIS"}
   {:pat "ES" :at-end? true :out "S"}
   ;;sj/tj ljud
   {:pat "SCH", :out ["SK" "C"]}
   {:pat "CH", :out ["K" "C"]}
   {:pat "SH", :out ["C"]}
 ;; SC före frontvokal (science, scene)
   {:pat "SCE", :out ["SE"] :at-start? true}
   {:pat "SCI", :out ["SI"] :at-start? true}
   {:pat "SCY", :out ["SI"] :at-start? true}
;; PH/CK/QU
   {:pat "PH", :out "F"}
   {:pat "CK", :out "K"}
   {:pat "QU", :out ["KV" "K" "K"]}

   ;; mjuka C/G före frontvokaler
   {:pat "CE", :out  "SE"}
   {:pat "CI", :out  "SI"}
   {:pat "CY", :out  "SI"}
   {:pat "GE", :at-start? false :out ["GE" "J" "IE" "E"]}
   {:pat "GE", :out ["GE" "IE" "E"]}
   {:pat "GI", :at-start? false :out ["I" "J" "GI"]}
   {:pat "GI", :out ["I" "GI"]}

   {:pat "GY", :at-start? false :out ["I" "J"]}
   {:pat "GY", :out "I"}
   ;; -tion/-sion
   {:pat "TION", :at-end? true :out ["CON" "TION"]}
   {:pat "SION", :at-end? true :out ["CON"]}
   {:pat "TS" :at-start? true :out "S"}
   ;; vokalgrupper – nu strikt svenska vokaler
   {:pat "AI", :out ["Ä" "ÄI" "E"]}
   {:pat "AL" :out ["EL" "AL"]}
   {:pat "AY", :out ["Ä" "AI" "E"]}
   {:pat "IA", :out ["AI" "IA"]}
   {:pat "IG", :out  "AI"}
   {:pat "EA", :out ["E" "I"]}
   {:pat "EE", :out  "I"}
   {:pat "IE", :out ["E" "I"]}
   {:pat "EI", :out ["E" "I"]}
   {:pat "OO", :out ["O" "U"]}
   {:pat "OU", :out ["O" "AU"]}
   {:pat "OW", :out ["OV" "Å"]}
   {:pat "AU", :out ["Å" "O" "A"]}
   {:pat "AI" :out ["EI" "AI"]}
   {:pat "AY" :out "EY"}
   {:pat "AO" :out "EIO"}
   {:pat "AI" :out "EI"}
   {:pat "AR" :out "AR"}
   {:pat "AL" :out "AL"}
   {:pat "Y", :out  "I"}
   {:pat "UE" :out ["U" "UE"]}
   {:pat "U" :out ["U" "A"]}
                                        ; eng. loch
   {:pat "OCH", :at-end? true :out ["OX"]}
;;Bach
   {:pat "ACH", :at-end? true :out ["AX"]}
;single
   {:pat "H", :out [""]}
   {:pat "C", :out ["K"]}
   {:pat "Q", :out ["K"]}
   {:pat "W", :out ["V"]}
   {:pat "J", :out ["I"]}
   {:pat "X", :out ["KS"]}
;;nordiska grafem
   {:pat "Æ", :out "Ä"}
   {:pat "Ø", :out "Ö"}
   {:pat "EY", :out ["EI" "I"]}
   {:pat "I", :out ["AI" "I"] #_#_:at-start? true}
   {:pat "EE" :out "I"}
   {:pat "E" :at-end? true, :out [""]}

   {:pat "ZZ", :out ["TS" "S"]}
   {:pat "Z", :out "S"}
   {:pat "R" :at-end? true :out ""}])

(def swedish-rules
  [;; sje före främre vokaler och kluster
   {:pat "GARAGE" :out "GARAC"}
   {:pat "KONSERT" :out "KONSÄR"}
   {:pat "SKJ", :out ["X"]}
   {:pat "STJ", :out ["X"]}
   {:pat "SCH", :out ["X"]}
   {:pat "SJ", :out ["X"]}

   {:pat "SKÄ", :out ["XÄ"]}
   {:pat "SKÖ", :out ["XÖ"]}
   {:pat "SKE", :out ["XE"]}
   {:pat "SKI", :out ["XI" "SKI"]}
   {:pat "SKY", :out ["XY" "SKY"]}
   {:pat "SKÆ" :out ["XÄ"]}
   {:pat "SKØ" :out ["XÖ"]}

   ;; retroflex-kluster (läggs tidigt)
   {:pat "RS", :out ["RS" "C"]}
   {:pat "RD", :out ["D" "RD"]}
   {:pat "RT", :out ["T" "RT"]}
   {:pat "RN", :out ["N" "RN"]}
   {:pat "RL", :out ["L" "RL"]}

   ;; mjukgöring av G före frontvokaler (och umlaut)
   {:pat "GÄ", :out "IÄ"}
   {:pat "GÖ", :out "IÖ"}
   {:pat "GE", :out "IE"}
   {:pat "GI", :out "I"}
   {:pat "GY", :out "IY"}
   {:pat "LGJ", :out "LI"}
   {:pat "GJ" :at-start? true :out "I"}
   ;; tje-ljud
   {:pat "KÄ", :out "CÄ"}
   {:pat "KÖ", :out "CÖ"}
   {:pat "KE", :out "CE"}
   {:pat "KI", :out "CI"}
   {:pat "KY", :out "CY"}

   {:pat "TJ", :out ["C"] :at-start? true}
   {:pat "KJ", :out ["C"] :at-start? true}

   ;; CH i svenska/lånord

   {:pat "CH"  :out ["X" "K"]}
   {:pat "PH", :out ["F" "P"]}

   ;; “tion/sion” i lånord
   {:pat "TION" :out ["XON" "TION"] :at-end? true}
   {:pat "SION" :out ["XON"] :at-end? true}

   ;; tysta/halvtysta J-kluster i början
   {:pat "DJ", :out ["I"] :at-start? true}
   {:pat "GJ", :out ["I"] :at-start? true}
   {:pat "HJ", :out ["I"] :at-start? true}
   {:pat "LJ", :out ["I"] :at-start? true}

   ;; nasaler och annat

   {:pat "NG" :out ["N" "NG"]}

   ;; fallback och enkla kollapser
   {:pat "CK", :out ["K"]}
   {:pat "CI" :out "SI"}
   {:pat "CE" :out "SE"}
   {:pat "CY" :out "SY"}
   {:pat "CÄ" :out "SÄ"}
   {:pat "CÖ" :out "SÖ"}
   {:pat "CA" :out "KA"}
   {:pat "CO" :out "KO"}
   {:pat "CU" :out "KU"}
   {:pat "CÅ" :out "KÅ"}
   {:pat "C", :out ["K" "S"]}
   {:pat "Q", :out ["K"]}
   {:pat "W", :out ["V"]}
   {:pat "X", :at-start? true :out ["S"]} ;;xylofon xerces
   {:pat "X", :out ["KS"]}
   {:pat "ZZ", :out ["TS" "S"]}
   {:pat "Z", :out ["S"]}
   {:pat "PS", :out ["PS" "S"]}
   {:pat "J", :out ["I"]}
   {:pat "TS" :out ["TS" "S"]} ;;detsamma->desamma

   {:pat "AU", :out ["A"]}
   {:pat "G", :out ["G" "I"] :at-start? true} ; För aggresiv?
   {:pat "KO", :out "KO"}
   {:pat "GO", :out "GO"}
   {:pat "LO", :out "LO"}
   
   {:pat "O", :out ["O" "Å"]}

   ;; --- Bokstäver / digrafer (DK/NO) ---
   {:pat "ÄU", :out ["EU" "ÄU"]}
   {:pat "AA", :out ["Å" "A"]}
   {:pat "AE", :out "Ä"}
   {:pat "Æ", :out "Ä"}
   {:pat "Ø", :out "Ö"}
   {:pat "E" :at-end? true :out ["E", ""]}

   ;; --- SK + främre vokaler inkl. Æ/Ø -> sj-ljud (DK också) ---

   ;; --- "Mjukt d" 
   {:pat "AD" :out ["A" "AD"] :at-end? true}
   {:pat "ED" :out ["E" "ED"] :at-end? true}
   {:pat  "OD" :out ["O" "OD"] :at-end? true}
   {:pat "ÅD" :out ["Å" "ÅD"] :at-end? true}
   {:pat "R" :at-end? true :out ["" "R"]}
   {:pat "H", :out [""]}]               ;Är H tyst
  )


(comment
  
  

 )
