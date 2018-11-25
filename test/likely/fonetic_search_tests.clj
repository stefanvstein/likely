(ns likely.fonetic-search-tests
    (:require [clojure.test :refer [deftest is]]
            [likely.fonetic-searches :as fonetic]
            [likely.testdata :as testdata]))

(deftest find-fonetic
  (is (= (list "marianne")
         (fonetic/find-fonetic "marion" (:mra testdata/m)))))

(deftest starts-with-fonetic
  (is (= ["morgan"] (fonetic/find-starts-with-fonetic "murg" (:mra testdata/m)))))

(deftest fonetic-distance 
  (is (= ["morgan"] (fonetic/find-fonetic-distance "nurgn" 1 (:mra testdata/m)))))

