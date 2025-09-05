(defproject likely "0.1.0-SNAPSHOT"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/math.combinatorics "0.1.4"]
                 [org.clj-commons/hickory "0.7.7"]
                 [org.clojure/core.async "1.6.681"]
                 [commons-codec/commons-codec "1.19.0"]
]
  :java-source-paths ["java"]
  :main likely.example.main)
