(ns likely.damerau-test
  (:require [clojure.test :refer :all])
  (:require [likely.damerau :refer :all]))

(deftest damerau-test
  (is (= 1 (damerau "olle" "ole")))
  (is (= 2 (damerau "olle" "oe")))
  (is (= 1 (damerau "olle" "lole")))
  (is (= 2 (damerau "olle" "lolel"))))
