(ns clj-diff.test.performance
  (:use [clj-diff [core :only [diff]]])
  (:use [clj-diff [performance :only [mutate]]])
  (:use [clojure.test]))

(deftest mutate-test
  (let [a "abcdefghijklmnopqrstuvwxyz"
        changes #(count (flatten (map rest (:+ %))))
        largest-group #(apply max (map count (map rest (:+ %))))
        t (fn [a m g] (diff a (mutate a m g)))]
    (is (<= (changes (t a 1 1)) 1))
    (is (<= (changes (t a 2 1)) 2))
    (is (<= (changes (t a 5 1)) 5))
    (is (<= (changes (t a 5 5)) 5))
    (is (<= (largest-group (t a 5 5)) 5))
    (is (<= (changes (t a 10 5)) 10))
    (is (<= (largest-group (t a 10 5)) 5))))
