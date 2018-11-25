(ns likely.distance-searches-tests
  (:require [clojure.test :refer [deftest is]]
            [likely.distance-searches :as distance]
            [likely.testdata :as testdata]))

(deftest find-with-distance
  (is (= (set ["daam" "aadm"] )
         (set (distance/find-with-distance "adam" 1 (:ref testdata/m)))))
  (is (nil? (distance/find-with-distance "wawaw" 1 {}))))

(deftest starts-with-distance
  (is (= '("bertilson" "bertilsson")
         (distance/find-starts-with-distance "beruils" 1 (:ref testdata/m))))
  (is (= '("bertilson" "bertilsson")
         (distance/find-starts-with-distance "ebrtils" 1 (:ref testdata/m)))))
