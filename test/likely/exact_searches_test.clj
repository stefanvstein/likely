(ns likely.exact-searches-test
  (:require [clojure.test :refer  [deftest is]]
            [likely.exact-searches :as exact]))

(def refdata {"daam" #{3},
	 "nilsson" #{5},
	 "adam" #{1 6},
	 "nilsonn" #{6},
	 "aadm" #{2},
	 "morgan" #{4},
	 "marianne" #{5},
	 "nilssson" #{3},
	 "bertilson" #{2},
	 "bertilsson" #{1},
	 "bedrup" #{4}})

(deftest find-exact
  (is (=  {"morgan" #{4}}
          (exact/find-exact "morgan" refdata))))

(deftest find-starts-with
  (is (=  {"morgan" #{4}}
          (exact/find-starts-with "morg" refdata)))
  (is (nil? (exact/find-starts-with "mor" refdata))))

(deftest find-contains
  (is (=  {"bertilsson" #{1} "nilsson" #{5}}
          (exact/find-contains "ilsson" refdata)))
  (is (nil?
       (exact/find-contains "sson" refdata))))

(deftest not-too-long
  (is ((exact/not-too-long-pred 3) "1234"))
  (is ((exact/not-too-long-pred 3) "12345"))
  (is (not ((exact/not-too-long-pred 3) "123456")))
  (is ((exact/not-too-long-pred 4) "12345"))
  (is ((exact/not-too-long-pred 4) "1234567"))
  (is (not ((exact/not-too-long-pred 4) "12345678")))
  (is ((exact/not-too-long-pred 5) "12345"))
  (is ((exact/not-too-long-pred 5) "12345678901234567890")))
