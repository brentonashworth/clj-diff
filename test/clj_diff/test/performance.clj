(ns clj-diff.test.performance
  (:use [clj-diff.core])
  (:use [clj-diff.performance] :reload)
  (:use [clojure.test]))

(deftest mutate-test
  (let [a "abcdefghijklmnopqrstuvwxyz"
        changes #(count (flatten (map rest (:+ %))))
        largest-group #(apply max (map count (map rest (:+ %))))]
    (is (<= (changes (diff a (mutate a 1 1)))
           1))
    (is (<= (changes (diff a (mutate a 2 1)))
            2))
    (is (<= (changes (diff a (mutate a 5 1)))
            5))
    (is (<= (changes (diff a (mutate a 5 5)))
            5))
    (is (<= (largest-group (diff a (mutate a 5 5)))
            5))
    (is (<= (changes (diff a (mutate a 10 5)))
            10))
    (is (<= (largest-group (diff a (mutate a 10 5)))
            5))))
