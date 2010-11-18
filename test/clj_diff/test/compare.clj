(ns clj-diff.test.compare
  (:use [clj-diff [performance :only [random-string
                                      random-between
                                      fraser-distance
                                      fraser-diff
                                      mutate
                                      edit-distance]]])
  (:use [clojure.test])
  (:require [clj-diff [core :as core]]
            [clj-diff [myers :as myers]]
            [clj-diff [miller :as miller]])
  (:import name.fraser.neil.plaintext.diff_match_patch))

(deftest fraser-distance-test
  (is (= (fraser-distance (fraser-diff "aba" "aca"))
         2))
  (is (= (fraser-distance (fraser-diff "abcabba" "cbabac"))
         5))
  (is (= (fraser-distance (fraser-diff "nBP8GaFHVls2dI8h9aK1FWdRgevf43"
                                       "925BCPcYhT5hs8L9T3K2T5C7U3Lz5v"))
         46)))

(defn random-diff->patch-test [f]
  (dotimes [_ 100]
    (let [a (random-string (random-between 100 1000))
          mutations (random-between 10 (quot (count a) 2))
          groups (random-between 1 (quot mutations 2))
          b (mutate a mutations groups)
          diff (try (f a b)
                    (catch Exception e (do (println "----------")
                                           (println e)
                                           (println "a:" (str a))
                                           (println "b:" (str b)))))
          patched (apply str (core/patch a diff))]
      (when (not (= patched b))
        (do (println "----------")
            (println "a:" (str a))
            (println "b:" (str b))
            (println "diff:" diff)
            (println "patched:" patched)))
      (is (= patched b)))))

(deftest correct-diff-myers-test
  (random-diff->patch-test myers/diff))

(deftest correct-diff-miller-test
  (random-diff->patch-test miller/diff))

(deftest same-edit-distance
  (dotimes [_ 100]
    (let [a (random-string (random-between 100 1000))
          size (count a)
          mutations (random-between 5 (quot size 2))
          groups (random-between 1 (quot mutations 2))
          b (mutate a mutations groups)
          myers-dist (edit-distance (myers/diff a b))
          miller-dist (edit-distance (miller/diff a b))
          fraser-dist (edit-distance (fraser-diff a b))]
      (when (not (= fraser-dist miller-dist myers-dist))
        (do (println "a:" (str a))
            (println "b:" (str b))
            (println "fraser:" fraser-dist)
            (println "myers:" myers-dist)
            (println "miller:" miller-dist)))
      (is (= fraser-dist myers-dist miller-dist)))))
