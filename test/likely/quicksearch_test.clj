(ns likely.quicksearch-test
  (:require [clojure.test :refer [deftest is]]
            [likely.quicksearch :as q]
            [likely.testdata :as testdata]))



(deftest search
  (is (= ["morgan"  "bertilsson" "nilssson" "nilsson"  "marianne"]
         (q/search "tilss morgan" testdata/m))))

