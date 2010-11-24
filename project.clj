(defproject clj-diff "1.0.0-SNAPSHOT"
  :description "Sequential diff in Clojure."
  :url "http://github.com/brentonashworth/clj-diff"
  :source-path "src/clj"
  :java-source-path "src/jvm"
  :java-fork "true"
  :java-debug "true"
  :hooks [leiningen.hooks.javac]
  :dev-dependencies [[org.clojure/clojure "1.2.0"]
                     [lein-javac "1.2.1-SNAPSHOT"]])
