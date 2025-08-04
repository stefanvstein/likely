(ns likely.distance-searches-tests
  (:require [clojure.test :refer [deftest is]]
            [likely.distance-searches :as distance]
            ))
(def map-of-data
  {"daam" "value",
   "nilsson" #{5},
   "adam" #{1 6},
   "nilsonn" #{6},
   "aadm" #{2 "Often a set"},
   "morgan" #{4},
   "marianne" #{5},
   "nilssson" #{3},
   "bertilson" #{2},
   "bertilsson" #{1},
   "bedrup" #{4}})

(deftest find-with-distance
  (is (=  {"adam" #{1 6}
           "daam" "value"
           "aadm" #{2 "Often a set"}}
          (distance/find-with-distance "adam" 1 map-of-data)))
  (is (nil? (distance/find-with-distance "wawaw" 1 {})))
  (is (= #{"odam" "adam" "adamo"}
         (distance/find-with-distance "adam" 1 ["adam" "niklas" "odam" "adamo"]))))

(deftest starts-with-distance
  (is (= {"bertilson" #{2}
          "bertilsson" #{1}}
         (distance/find-starts-with-distance "beruils" 1  map-of-data)))
  (is (= {"bertilson" #{2}
          "bertilsson" #{1}}
         (distance/find-starts-with-distance "ebrtils" 1 map-of-data)))
  (is (= #{"bertilson" 
          "bertilsson"}
         (distance/find-starts-with-distance "ebrtils" 1 ["marianne" 
                                                          "nilssson"
                                                          "bertilson"
                                                          "bertilsson"
                                                          "bedrup" ]))))
