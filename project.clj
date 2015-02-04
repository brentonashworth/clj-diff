(defproject clj-diff "1.0.0-SNAPSHOT"
  :description "Sequential diff in Clojure."
  :url "http://github.com/brentonashworth/clj-diff"
  :source-paths ["src/cljx" "src/clj"]
  :test-paths ["target/test-classes"]
  :auto-clean false
  :java-source-paths ["src/jvm"]
  :jar-exclusions [#"\.cljx|\.swp|\.swo|\.DS_Store|\.class"]

  :dependencies [[org.clojure/clojure "1.6.0"]]

  :profiles {:dev {:dependencies [[org.clojure/clojurescript "0.0-2760"]
                                  [marginalia "0.8.0"]]
                   :plugins [[com.keminglabs/cljx "0.5.0"]
                             [lein-difftest "2.0.0"]
                             [com.cemerick/clojurescript.test "0.3.3"]
                             [lein-cljsbuild "1.0.4"]]
                   :aliases {"cleantest" ["do" "clean," "cljx" "once," "test," "cljsbuild" "test"]
                             "jar" ["do" "clean," "cljx" "once," "jar"]
                             "deploy" ["do" "clean," "cljx" "once," "deploy" "clojars"]}}}

  :cljsbuild {:builds
              {:test {:source-paths ["target/classes" "target/test-classes"]
                      :compiler {:output-to "target/testable.js"
                                :optimizations :simple
                                :pretty-print true}}}
              :test-commands {"test" ["node" :node-runner "target/testable.js"]}}

  :cljx {:builds [{:source-paths ["src/cljx"]
                   :output-path "target/classes"
                   :rules :clj}

                  {:source-paths ["src/cljx"]
                   :output-path "target/classes"
                   :rules :cljs}

                  {:source-paths ["test/cljx"]
                   :output-path "target/test-classes"
                   :rules :clj}

                  {:source-paths ["test/cljx"]
                   :output-path "target/test-classes"
                   :rules :cljs}]}
  )
