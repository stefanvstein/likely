(ns likely.dataloader-test
    (:require [clojure.test :refer :all])
    (:require [likely.dataloader :refer :all]))

(deftest data-from-resultset
  (is (= {:ref {"olle" #{1}, "polle" #{1}, "rultan" #{2}},
          :mra {"AL" #{"olle"}, "PAL" #{"polle"}, "RALTN" #{"rultan"}},
          :id {1 "Olle Polle", 2 "Rultan Tultan"}}
         (create-data-from-resultset [{:name "Olle Polle" :key 1}
                                      {:name "Rultan Tultan" :key 2}]                                 
                                     :key
                                     [:name]
                                     [(partial not= "tultan")]
                                     :name))))
(deftest data-from-sentences
  (is (= {:ref {"olle" #{0}, "polle" #{0}, "rultan" #{1}},
          :mra {"AL" #{"olle"}, "PAL" #{"polle"}, "RALTN" #{"rultan"}},
          :id {0 "Olle Polle.", 1 "Rultan Tultan."}}
         (create-data-from-sentences "Olle Polle. Rultan Tultan."                                 
                                     [(partial not= "tultan")]))))


