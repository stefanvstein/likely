(ns likely.testdata
  (:require [clojure.test :refer  [deftest is]]
            [likely.dataloader :as dl]))

(def data [{:key 1, :a "Adam" :b "Bertilsson"}
           {:key 2, :a "Aadm" :b "Bertilson"}
           {:key 3, :a "Daam" :b "Nilssson"}
           {:key 4, :a "Morgan" :b "Bedrup ab"}
           {:key 5, :a "Marianne" :b "Nilsson"}
           {:key 6, :a "Adam" :b "Nilsonn"}])

(def m (dl/create-data-from-resultset data
                                      :key
                                      [:a :b]
                                      [(complement #{"ab" "as" "gmbh"})]
                                      #(vector (:a %) (:b %))))

(deftest testdata
  (is (=
       {:ref	  
	{"daam" #{3},
	 "nilsson" #{5},
	 "adam" #{1 6},
	 "nilsonn" #{6},
	 "aadm" #{2},
	 "morgan" #{4},
	 "marianne" #{5},
	 "nilssson" #{3},
	 "bertilson" #{2},
	 "bertilsson" #{1},
	 "bedrup" #{4}},
	:mra
	{"ADM" #{"adam" "aadm"},
	 "BIRLSN" #{"bertilson" "bertilsson"},
	 "DAM" #{"daam"},
	 "NILSN" #{"nilsson" "nilsonn" "nilssson"},
	 "MARGN" #{"morgan"},
	 "BIDRP" #{"bedrup"},
	 "MARN" #{"marianne"}},
	:id
	{1 ["Adam" "Bertilsson"],
	 2 ["Aadm" "Bertilson"],
	 3 ["Daam" "Nilssson"],
	 4 ["Morgan" "Bedrup ab"],
	 5 ["Marianne" "Nilsson"],
	 6 ["Adam" "Nilsonn"]}}
       m)))

