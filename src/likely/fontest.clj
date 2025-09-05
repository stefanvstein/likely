(ns likely.fontest
  (:require [likely.mraplus :refer [mra-plus phonetic swedish-rules english-rules]]
            [likely.mraorig :as mra]
            [likely.mra :as demo]
            [likely.fonetic-searches :as fonetic]
            [clojure.string :as str])
  (:import [org.apache.commons.codec.language DoubleMetaphone]))
(def dmf (DoubleMetaphone.))

(defn double-metaphone [s]
  (let [d1 (.doubleMetaphone dmf s false)
        d2 (.doubleMetaphone dmf s true)]
    (if (= d1 d2)
      [d1] [d1 d2])))

(def eng-ord
  ["camera" "every" "family" "vegetable" "camera" "whale" "wail" "veil" "vale" "while" "white" "wine" "wolf" "woman" "world"
   "church" "christ" "chef" "chaos" "chord" "chorus" "champagne" "chocolate" "charming" "cheese"
   "giant" "ginger" "gym" "giraffe" "guide" "gulf" "girl" "george" "joy" "john"
   "nation" "station" "action" "vision" "decision" "mission" "fusion" "occasion" "passion" "session"
   "smith" "smyth" "schmidt" "schneider" "jones" "johnson" "jackson" "charles" "charlotte" "geoff"
   "queue" "xylophone" "physics" "psychology" "yacht" "honour" "knight" "gnome" "wrack" "pneumonia"
   "pizza" "fuzz" "jazz" "zebra" "zone" "zero" "quiz" "queen" "quick" "question"
   "factory" "phenomenon" "philosophy" "fantasy" "famous" "fascinate" "fashion" "figure" "family" "future"
   "language" "letter" "listen" "library" "life" "love" "lord" "lady" "long" "light"
   "summer" "winter" "autumn" "spring" "sun" "moon" "stars" "cloud" "rain" "snow"])


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
          "förklara" "förstå" "försvinna" "förlora" "förlåta" "försvara" "försöka" "förvåna" "förvänta" "förbjuda" "brokolligratäng" "stridsvagnsförare"])
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
              "fizz" "buzz" "amazing" "zero" "zebra" "whale" "whisper" "what" "when" "money" "queue" "question" "unique"])
(def exempel [])
(comment
  (phonetic "gärna" [swedish-rules])
  (time (map (fn [x] [x {:dm (double-metaphone x) :demo (demo/mra-codex x)
                         :mra (mra/mra-encode x)
                         :english (phonetic x [english-rules])
                         :swedish (phonetic x [swedish-rules])
                         :e-mraplus (mra-plus x [english-rules])
                         :s-mraplus (mra-plus x [swedish-rules])}])
             (set (concat ord words eng-ord))))
  (map (fn [x] [x {:dm (double-metaphone x) :mra (mra/mra-encode x) ::mraplus (mra-plus x [english-rules])}])  ord)
  (map (fn [x] [x (mra-plus x [swedish-rules])]) ord)
  (map (fn [x] [x (phonetic x [english-rules])]) eng-ord)
  (->> (concat ord words eng-ord)
       (set)

       (mapv #(let [en (str/join ", " (phonetic % [english-rules]))
                    se (str/join ", " (phonetic % [swedish-rules]))]
                (str % " -> en: ("  en ")  se: (" se ")\n")))
       println))
