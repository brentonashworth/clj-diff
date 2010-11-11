(ns clj-diff.test.compare
  (:use [clj-diff.core]
        [clj-diff.performance])
  (:use [clojure.test])
  (:require [clj-diff [myers :as myers]]
            [clj-diff [miller :as miller]])
  (:import name.fraser.neil.plaintext.diff_match_patch))

(defn fraser-distance [a b]
  (let [dmp (diff_match_patch.)
        _ (set! (. dmp Diff_Timeout) 0)
        diffs (.diff_main dmp a b)
        diffs (map #(vector (.toString (.operation %)) (.text %)) (seq diffs))]
    (reduce + (map #(case (first %)
                          "EQUAL" 0
                          (count (last %)))
                   diffs))))

(deftest fraser-distance-test
  (is (= (fraser-distance "aba" "aca")
         2))
  (is (= (fraser-distance "abcabba" "cbabac")
         5))
  (is (= (fraser-distance "nBP8GaFHVls2dI8h9aK1FWdRgevf43"
                          "925BCPcYhT5hs8L9T3K2T5C7U3Lz5v")
         46)))

;; In some curcumstances, the edit distance is slightly higher for
;; myers and miller than it is for fraser. Use the output of these
;; errors to track down the problem.
#_(deftest same-edit-distance
  (dotimes [_ 100]
    (let [a (random-string (random-between 20 80))
          size (count a)
          mutations (random-between 10 (quot size 2))
          groups (random-between 1 (quot mutations 2))
          b (mutate a mutations groups)
          myers-dist (edit-distance (myers/diff a b))
          miller-dist (edit-distance (miller/diff a b))
          fraser-dist (fraser-distance a b)]
      (when (not (= fraser-dist miller-dist myers-dist))
        (do (println "a:" (str a))
            (println "b:" (str b))
            (println "fraser:" fraser-dist)
            (println "myers:" myers-dist)
            (println "miller:" miller-dist)))
      (is (= fraser-dist myers-dist miller-dist)))))
