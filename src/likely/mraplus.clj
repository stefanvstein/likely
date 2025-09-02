(ns likely.mraplus
  (:require
   [likely.normalize :as normalize]))



(defn take-codex-letters
  "Ta upp till head-count första + upp till tail-count sista tecken, utan överlapp."
  ^String [^String s head-count tail-count]
  (let [n (count s)]
    (if (<= n head-count)
      s
      (let [head (subs s 0 head-count)
            tail (subs s (max head-count (- n tail-count)) n)]  ; starta svansen efter huvudet
        (str head tail)))))




(defn ^boolean starts-with-at? [^String s ^long i ^String pat]
  (let [m (count pat)]
    (and (<= (+ i m) (count s))
         (.regionMatches s i pat 0 m))))

(defn first-hit-all
  "Första regeln som matchar vid i i ett ruleset. Returnerar {:len L :repls [...]}, annars nil."
  [^String s ^long i rules]
  (some (fn [[pat repls]]
          (when (starts-with-at? s i pat)
            {:len (count pat) :repls (or (distinct repls) "")})) ; behåll ordning, ta bort dubbletter
        rules))

(defn dedupe-consecutive [s]
  (->> s (partition-by identity) (map first) (apply str)))

(defn expand-per-ruleset-all
  "Ett flöde per språk: första träffen per position, men alla ersättningar för den träffen."
  [^String s rules]
  (let [n (count s)]
    (loop [i 0, acc ["" ]]
      (if (= i n)
        acc
        (if-let [{:keys [len repls]} (first-hit-all s i rules)]
          ;; kartesisk produkt: alla prefix × alla repls, gå fram 'len'
          (recur (+ i len)
                 (persistent!
                   (reduce (fn [out p]
                             (reduce (fn [o r] (conj! o (str p r))) out repls))
                           (transient []) acc)))
          ;; ingen träff: kopiera tecknet och gå 1
          (recur (inc i)
                 (mapv #(str % (.charAt s i)) acc)))))))

(defn keep-n-of
  "Behåll högst n förekomster av tecken ur xs i s. Övriga tecken behålls alltid.
   xs kan vara en sträng eller en sekvens av tecken."
  [n xs s]
  (let [S   (set xs)                       ;; funkar för både sträng och seq
        n   (long (max 0 n))
        ^String s s
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




(defn mra-plus
  "Phonetics with expansions, keeping first 2 vowels, shortened to first and last 3"
  [word rulesets]
  (let [s (normalize/normalize word "åäö")]
    (->> rulesets
         (mapcat #(expand-per-ruleset-all s %)) ; union över språk efter expansion

         (map #(keep-n-of 2 "AEIOUYÅÄÖ" %))
         (map dedupe-consecutive)
         (map #(take-codex-letters % 3 3))

         set)))

(defn phonetic [word rulesets]
  (let [s (normalize/normalize word "åäö")]
    (->> rulesets
         (mapcat #(expand-per-ruleset-all s %)) ; union över språk efter expansion
         (map dedupe-consecutive)
         )))

#_(comment 
    A ≈ IPA [a], [ɑ] (kort/långt a, sv. mat)
    E ≈ IPA [e], [ɛ] (kort/långt e, kort ä)
    I ≈ IPA [i] (sv. vit)
    O ≈ IPA [o], [ɔ] (sv. sol, boll)
    U ≈ IPA [ʉ] (sv. hus) och ibland [u] (eng. boot)
    Y ≈ IPA [y] (sv. ny)
    Å ≈ IPA [oː], [ɔː] (sv. båt)
    Ä ≈ IPA [æ], [ɛː] (sv. är, eng. cat ~ lång ä)
    Ö ≈ IPA [ø], [œ] (sv. röd)

    B, D, F, G, K, L, M, N, P, R, S, T, V, Y – direkt svensk mappning.

    C = tje-ljud ([ɕ], [t͡ɕ], [ɧ], [ʃ], [x] )
    X = sch-ljud (, engelska sh i skotska loch eller svenska skepp)
    J = tonande tje-ljud som i engelska dodge)

(def english-rules
  [;; längsta match först
   ["TCH"  ["C"]]                         ; CHG: tje-ljud
   ["DGE"  ["J"]]
   ["ISL" ["AIL"]]
   ["EIGH" ["EI"] ]
   ["AY" ["EI"]]
   ["AUGH" ["O" "OF" "Å"]]                ; CHG: å in som alt
   ["OUGH" ["O" "OF" "Å" "U"]]                ; CHG: samma logik
   ["GH"   ["F" ""]]                      ; ibland stumt
   ["GLE" ["GLE" "GEL"]]
   ["IPT" ["IPT" "IT"]]
   ;; tysta initiala kluster
   ["KN"   ["N" "KN"]]
   ["GN"   ["N" "GN"]]
   ["WR"   ["R"]]
   ["PS"   ["S"]]
   ["PN"   ["N"]]
   ["PT"   ["T"]]
   ["KE" ["K" "KE"]] 
   ["MB"   ["M" "MB"]]

   ;; engelska 'th', 'wh'
   ["TH"   ["T" "D"]]                     ; CHG
   ["WH"   ["V"]]                         ; CHG: landar i sv. V

;; sj/tj-ljud
   ["SCH"  ["X" "SK" "C"]]
   ["CH"   ["C" "K"]]                 ; CHG: C primär
   ["SH"   ["C"]]

   ;; SC före frontvokal (science, scene)
   ["SCE"  ["SE"]]                        ; NEW
   ["SCI"  ["SI"]]                        ; NEW
   ["SCY"  ["SI"]]                        ; NEW

   ;; PH/CK/QU
   ["PH"   ["F"]]
   ["CK"   ["K"]]
   ["QU"   ["KV" "K"]]

   ;; mjuka C/G före frontvokaler
   ["CE"   ["SE"]]
   ["CI"   ["SI"]]
   ["CY"   ["SI"]]
   ["GE"   ["IE" "E"]]
   ["GI"   ["I" "GI"]]
   ["GY"   ["I" "GI"]]                   ; CHG: Y → I

   ;; -tion/-sion
   ["TION" ["CON"]]                ; CHG: C eller X
   ["SION" ["CON"]]

   ;; vokalgrupper – nu strikt svenska vokaler

   ["AI"   ["Ä" "ÄI" "E"]]                     ; CHG
   ["AY"   ["Ä" "AI" "E"]]                     ; CHG
   ["IA"   ["AI" "IA"]]
   ["IG"   ["AI"]]
   ["EA"   ["E" "I"]]
   ["EE"   ["I"]]
   ["IE"   ["E" "I"]]
   ["EI"   ["E" "I"]]
   ["OO"   ["O" "U"]]
   ["OU"   ["O" "U"]]
   ["OW"   ["OV" "Å"]]
   ["AU"   ["Å" "O" "A"]]                     ; NEW
   ["Y"    ["I"]]                         ; CHG
   ["OCH" ["OX"]]    ; eng. loch
   ["ACH" ["AX" "AC"]]    ; eng./tyska lån: Bach, nach
   ;; singelbokstäver
   ["H"   [""]]
   ["C"   ["K"]]                          ; fallback
   ["Q"   ["K"]]
   
   ["W"   ["V"]]
   ["J" ["I"]]
   ["X" ["KS"]]
   ;; nordiska grafem i engelska namn
   ["Æ"   ["Ä"]]
   ["Ø"   ["Ö"]]
   ["EY" ["EI" "I"]]
   ["I" ["I" "AI"]]
   
   ["E" ["" "E"]]
   ;; Z
   ["ZZ"  ["TS" "S"]]
   ["Z"   ["S"]]])


(def swedish-rules
  [;; sje före främre vokaler och kluster
   ["SKÄ" ["XÄ"]]
   ["SKÖ" ["XÖ"]]
   ["SKE" ["XE"]]
   ["SKI" ["XI" "SKI"]]
   ["SKY" ["XY" "SKY"]]
   ["SKJ" ["X"]]
   ["STJ" ["X" "C"]]
   ["SCH" ["X"]]
   ["SJ"  ["X"]]

   ;; retroflex-kluster (läggs tidigt)
   ["RS"  ["RS" "C"]]
   ["RD"  ["D" "RD"]]
   ["RT"  ["T" "RT"]]
   ["RN"  ["N" "RN"]]
   ["RL"  ["L" "RL"]]

   ;; mjukgöring av G före frontvokaler (och umlaut)
   ["GÄ"  ["IÄ" "GÄ"]]
   ["GÖ"  ["IÖ" "GÖ"]]
   ["GE"  ["IE" "GE"]]
   ["GI"  ["I" "GI"]]
   ["GY"  ["IY" "GY"]]

   ;; tje-ljud
   ["KÄ"  ["CÄ"]]
   ["KÖ"  ["CÖ" "KÖ"]]
   ["KE"  ["CE" "KE"]]
   ["KI"  ["CI" "KI"]]
   ["KY"  ["CY"]]
   ["TJ"  ["C"]]
   ["KJ"  ["C"]]

;; CH i svenska/lånord
   ["CH"  ["X" "K" "C"]]

   ;; “tion/sion” i lånord
   ["TION" ["XON"]]
   ["SION" ["XON"]]

   ;; tysta/halvtysta J-kluster i början
   ["DJ"  ["I"]]
   ["GJ"  ["I"]]
   ["HJ"  ["I"]]
   ["LJ"  ["I"]]

   ;; nasaler och annat
   ["NG"  ["N" "NG"]]

   ;; fallback och enkla kollapser
   ["SK"  ["SK"]]
   ["CK"  ["K"]]
   ["C"   ["K" "S"]]
   ["QU"  ["KV" "K"]]
   ["W"   ["V"]]
   ["X"   ["KS"]]
   ["ZZ"  ["TS" "S"]]
   ["Z"   ["S"]]
   ["PH"  ["F" "P"]]
   ["PS" ["PS" "S"]]
   ["H"   [""]]
   ["J"   ["I"]]
   ["US" ["AS" "US"]]
   ["RE" ["RE" "R"]]
   ["AU" ["A"]]
   ["G" ["G" "I"]]
   ["O" ["O" "Å"]]

   ;; --- Bokstäver / digrafer (DK/NO) ---
   ["ÄU" ["EU" "ÄU"]]
   ["AA" ["Å" "A"]]
   ["AE" ["Ä"]]
   ["Æ"  ["Ä"]]
   ["Ø"  ["Ö"]]

   ;; --- SK + främre vokaler inkl. Æ/Ø -> sj-ljud (DK också) ---
   ["SKÆ" ["XÄ"]]
   ["SKØ" ["XÖ"]]

   ;; --- "Mjukt d" 

   ["AD" ["A" "AD"]]
   ["ED" ["E" "ED"]]
   ["OD" ["O" "OD"]]
   ["ÅD" ["Å" "ÅD"]]])




(comment
  (def ord ["djur" "färd" "karta" "barn" "första" "whisky" "fax" "taxfree" "gäng" "köra" "skön" "göra"
            "sjuk" "sjö" "skepp" "sked" "skjorta" "skjutsa" "skjuta" "skjuts" "stjärna" "stjäl"
            "tjej" "tjock" "tjur" "tjata" "kyrka" "kjol" "kära" "kärlek" "köpa" "köra"
            "champinjon" "choklad" "champagne" "charter" "chaufför" "chef" "schema" "schampo" "schack" "schäfer"
            "ljus" "hjul" "hjärna" "hjärta" "hjälp" "djur" "ljuga" "ljud" "göra" "gärna"
            "barn" "färd" "karta" "pärla" "första" "storslagen" "fjärde" "ord" "bord" "vård"
            "pizza" "fuzz" "jazz" "zebra" "zon" "zero" "whisky" "whist" "quorn" "quiz"
            "fysik" "fabrik" "fenomen" "filosofi" "fantasi" "fara" "vacker" "vinter" "väder" "väg"
            "ångest" "äventyr" "ärlig" "ärende" "öga" "öppna" "över" "önska" "år" "åska"
            "blomma" "blad" "båt" "bok" "bänk" "byxor" "boll" "banan" "band" "berg"
            "sommar" "vinter" "höst" "vår" "sol" "måne" "stjärnor" "moln" "regn" "snö"
            "språk" "skriva" "skratta" "skrik" "skydd" "skydda" "skyll" "skyddsrum" "skyddsväst" "skymning"
            "förklara" "förstå" "försvinna" "förlora" "förlåta" "försvara" "försöka" "förvåna" "förvänta" "förbjuda"])
  (def words ["rain" "said" "giant" "bridge" "dodge" "cheap" "ship" "loch" "Bach" "though" "through" "laugh"
              "show" "mission" "station" "vision" "you" "yellow" "jam" "jungle" "John" "jeans" "chair"
              "chop" "choir" "psychology" "gnome" "knight" "receipt" "pneumonia" "tsunami" "again"
              "chop" "sheep" "shop" "shower" "shame" "shore" "shrimp" "shrink" "shrub"
              "tough" "enough" "caught" "daughter"
              "nation" "decision" "precision" "explosion" "illusion" "fusion" "confusion"
              "your" "yours" "yesterday" "yes" "youth" "young" "yolk" "yawn"
              "January" "June" "July" "joke" "joy" "justice"
              "knife" "knee" "knew" "know" "knowledge" "gnat"
              "sword" "island" "hour" "honest" "honor" "heir" "eight" "weight"
              "seize" "weird" "leisure" "measure" "treasure" "pleasure" "azure" "genre" "machine" "chef"
              "school" "scheme" "schedule" "chaos" "character" "chemist" "chorus" "echo" "architect" "anchor"
              "photo" "phone" "phase" "physical" "elephant" "graph" "sphere" "alphabet" "nephew" "Stephen"
              "fizz" "buzz" "amazing" "zero" "zebra" "whale" "whisper" "what" "when" "money"])
  (map (fn [x] [x (phonetic x [swedish-rules])])  ord)
  (map (fn [x] [x (phonetic x [english-rules])])  words)
  (map (fn [x] [x (phonetic x [swedish-rules english-rules])]) (concat ord words))
  )
