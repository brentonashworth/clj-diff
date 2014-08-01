(defproject clj-diff "1.0.0-SNAPSHOT"
  :description "Sequential diff in Clojure."
  :url "http://github.com/brentonashworth/clj-diff"
  :source-paths ["src/clj" "src/cljs"]
  :java-source-path "src/jvm"
  :java-fork "true"
  :java-debug "true"
  :hooks [leiningen.hooks.javac
          leiningen.hooks.difftest]
  :dev-dependencies [[org.clojure/clojure "1.6.0"]
                     [lein-javac "1.2.1-SNAPSHOT"]
                     [marginalia "0.5.0"]
                     [lein-difftest "1.3.2-SNAPSHOT"]])
