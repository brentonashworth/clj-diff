(defproject clj-diff "1.1.0-SNAPSHOT"
  :description "Sequential diff in Clojure/Script."
  :url "http://github.com/rymndhng/clj-diff"
  :source-paths ["target/classes" "src/clj"]
  :test-paths ["target/test-classes"]
  :auto-clean false
  :java-source-paths ["src/jvm"]
  :jar-exclusions [#"\.swp|\.swo|\.DS_Store|\.class"]

  :dependencies [[org.clojure/clojure "1.7.0-RC1"]]

  :profiles {:dev {:dependencies [[org.clojure/clojurescript "0.0-3308"]
                                  [marginalia "0.8.0"]]
                   :plugins [[lein-difftest "2.0.0"]
                             [com.cemerick/clojurescript.test "0.3.3"]
                             [lein-cljsbuild "1.0.6"]]
                   :aliases {"cleantest" ["do" "clean," "once," "test," "cljsbuild" "test"]
                             "jar" ["do" "clean," "once," "jar"]
                             "deploy" ["do" "clean," "cljx" "deploy" "clojars"]}}}

  :cljsbuild {:builds
              {:test {:source-paths ["target/classes" "target/test-classes"]
                      :compiler {:output-to "target/testable.js"
                                 :optimizations :simple
                                 :pretty-print true}}}
              :test-commands {"test" ["node" :node-runner "target/testable.js"]}}
  )
