(defproject tech.droit/clj-diff "1.0.0"
  :description "Sequential diff in Clojure."
  :url "http://github.com/droitfintech/clj-diff"
  :license {:name "Eclipse Public License - v 1.0"
            :url "http://www.eclipse.org/legal/epl-v10.html"
            :distribution :repo
            :comments "same as Clojure"}

  :source-paths ["src/clj"]
  :java-source-paths ["src/jvm"]
  :plugins [[lein-marginalia "0.9.1"]]
  :profiles {:dev {:dependencies [[org.clojure/clojure "1.7.0"]]}})
