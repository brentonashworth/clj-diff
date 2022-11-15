(defproject tech.droit/clj-diff "2.0.0-alpha2"
  :description "Sequential diff in Clojure."
  :url "http://github.com/droitfintech/clj-diff"
  :license {:name "Eclipse Public License - v 1.0"
            :url "http://www.eclipse.org/legal/epl-v10.html"
            :distribution :repo
            :comments "same as Clojure"}

  :dependencies [[org.clojure/clojure "1.8.0"]
                 [bsless/clj-fast "0.0.11"]
                 [org.clojure/core.rrb-vector "0.1.2"]]
  :source-paths ["src/clj"]
  :plugins [[lein-marginalia "0.9.1"]])
