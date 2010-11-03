(defproject clj-diff "1.0.0-SNAPSHOT"
  :description "Sequential diff in Clojure."
  :dependencies [[org.clojure/clojure "1.2.0"]]
  ;; All dev dependencies are for performance testing.
  :dev-dependencies [[incanter "1.2.3"]
                     [org.clojars.brenton/google-diff-match-patch "0.1"]])
