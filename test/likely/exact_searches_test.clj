(ns likely.exact-searches-test
  (:require [clojure.test :refer  [deftest is]]
            [likely.testdata :as testdata]
            [likely.exact-searches :as exact]))

(deftest find-exact
  (is (= (set ["morgan"]) 
         (exact/find-exact "morgan" (:ref testdata/m )))))

(deftest find-starts-with
  (is (= ["morgan"]
         (exact/find-starts-with "morg" (:ref testdata/m))))
  (is (nil? (exact/find-starts-with "mor" (:ref testdata/m)))))

(deftest find-contains
  (is (= (set ["bertilsson", "nilsson"])
         (set  (exact/find-contains "ilsson" (:ref testdata/m)))))
  (is (nil?
       (exact/find-contains "sson" (:ref testdata/m)))))

(deftest not-too-long
  (is ((exact/not-too-long-pred 3) "1234"))
  (is ((exact/not-too-long-pred 3) "12345"))
  (is (not ((exact/not-too-long-pred 3) "123456")))
  (is ((exact/not-too-long-pred 4) "12345"))
  (is ((exact/not-too-long-pred 4) "1234567"))
  (is (not ((exact/not-too-long-pred 4) "12345678")))
  (is ((exact/not-too-long-pred 5) "12345"))
  (is ((exact/not-too-long-pred 5) "12345678901234567890")))
