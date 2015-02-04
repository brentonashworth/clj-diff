(ns clj-diff.test.core
  (:use [clj-diff.core :only (diff patch edit-distance
                               levenshtein-distance longest-common-subseq)])
  (:require #+cljs [cemerick.cljs.test :as t]
            #+clj  [clojure.test :as t])
  #+cljs
  (:require-macros [cemerick.cljs.test :as t]))

(t/deftest diff-test
  (let [t (fn [a b] (edit-distance (diff a b)))]
    (t/is (= (t [1 2 3 4 3 2 3 2 1 2 3] [2 3 1 2 3 4 5 4 3])
           10))
    (t/is (= (t "abcab" "cbab")
           3))
    (t/is (= (t "abcabba" "cbabac")
           5))
    (t/is (= (t [{:a 1} {:a 2} {:a 3} {:a 4} {:a 5} {:a 6} {:a 7}]
              [{:a 2} {:a 3} {:a 4} {:a 5} {:a 6} {:a 7} {:a 1}])
           2))
    (t/is (= (t "FWdRgevf43" "T5C7U3Lz5v")
           18))
    (t/is (= (t "s2dI8h9aK1FWdRgevf43"
              "5hs8L9T3K2T5C7U3Lz5v")
           30))
    (t/is (= (t "nBP8GaFHVls2dI8h9aK1FWdRgevf43"
              "925BCPcYhT5hs8L9T3K2T5C7U3Lz5v")
           46))
    (t/is (= (t "aba" "aca")
           2))))

(t/deftest patch-test
  (t/is (= (patch "aba"
                {:+ [[1 \c]], :- [1]})
         "aca"))
  (t/is (= (patch [1 2 3 4 3 2 3 2 1 2 3]
                {:+ [[10 4 5 4 3]], :- [0 3 4 5 6 7]})
         [2 3 1 2 3 4 5 4 3]))
  (t/is (= (patch "abcab"
                {:+ [[2 \b]], :- [0 1]})
         "cbab"))
  (t/is (= (patch "abcabba"
                {:+ [[2 \b] [6 \c]], :- [0 1 5]})
         "cbabac"))
  (t/is (= (patch "we1otec29q"
                {:+ [[9 \g \B \n \B \L \N \C \U \D \B]]
                 :- [0 1 2 3 4 5 6 7 8 9]})
         "gBnBLNCUDB"))
  (t/is (= (patch [{:a 1} {:a 2} {:a 3} {:a 4} {:a 5} {:a 6} {:a 7}]
                {:+ [[6 {:a 1}]], :- [0]})
         [{:a 2} {:a 3} {:a 4} {:a 5} {:a 6} {:a 7} {:a 1}]))
  (t/is (= (patch "FWdRgevf43"
                {:+ [[8 \T \5 \C \7 \U] [9 \L \z \5 \v]]
                 :- [0 1 2 3 4 5 6 7 8]})
         "T5C7U3Lz5v"))
  (t/is (= (patch "s2dI8h9aK1FWdRgevf43"
                {:+ [[-1 \5 \h] [5 \L] [7 \T \3]
                     [18 \2 \T \5 \C \7 \U] [19 \L \z \5 \v]]
                 :- [1 2 3 5 7 9 10 11 12 13 14 15 16 17 18]})
         "5hs8L9T3K2T5C7U3Lz5v"))
  (t/is (= (patch
          "nBP8GaFHVls2dI8h9aK1FWdRgevf43"
          {:+ [[0 \9 \2 \5]
               [1 \C]
               [9 \c \Y \h \T \5 \h]
               [15 \L]
               [17 \T \3]
               [28 \2 \T \5 \C \7 \U]
               [29 \L \z \5 \v]]
           :- [0 3 4 5 6 7 8 9 11 12 13 15 17 19 20 21 22 23 24 25 26 27 28]})
         "925BCPcYhT5hs8L9T3K2T5C7U3Lz5v")))

(t/deftest roundtrip
  (t/are [a b]
       (= b (patch a (diff a b)))

       "aba" "aca"
       "abcabba" "cbabac"
       "FWdRgevf43" "T5C7U3Lz5v"
       "s2dI8h9aK1FWdRgevf43" "5hs8L9T3K2T5C7U3Lz5v"
       "nBP8GaFHVls2dI8h9aK1FWdRgevf43" "925BCPcYhT5hs8L9T3K2T5C7U3Lz5v"
       "aba" "aca"))

(t/deftest edit-distance-test
  (t/is (= (edit-distance (diff "aba" "aca"))
         2))
  (t/is (= (edit-distance "aba" "aca")
         2))
  (t/is (= (edit-distance (diff "abcabba" "cbabac"))
         5))
  (t/is (= (edit-distance "abcabba" "cbabac")
         5))
  (t/is (= (edit-distance (diff "nBP8GaFHVls2dI8h9aK1FWdRgevf43"
                              "925BCPcYhT5hs8L9T3K2T5C7U3Lz5v"))
         46)))

(t/deftest levenshtein-distance-test
  (t/are [a b d] (= (levenshtein-distance a b) d)
       "aba" "aba" 0
       "aba" "ada" 1
       "abca" "aca" 1
       "abma" "aca" 2
       "kitten" "sitting" 3
       "Saturday" "Sunday" 3
       "gumbo" "gambol" 2
       "nBP8GaFHVls2dI8h9aK1FWdRgevf43" "925BCPcYhT5hs8L9T3K2T5C7U3Lz5v" 28
       [1 2 4] [1 2 4 3] 1))

(t/deftest longest-common-subseq-test
  (t/are [a b _ d] (= (longest-common-subseq a b) d)
       "aba" "aba"         => "aba"
       "aba" "ada"         => "aa"
       "abca" "aca"        => "aca"
       "abma" "aca"        => "aa"
       "kitten" "sitting"  => "ittn"
       "Saturday" "Sunday" => "Suday"
       "gumbo" "gambol"    => "gmbo"
       "nBP8GaFHVls2dI8h9aK1FWdRgevf43" "925BCPcYhT5hs8L9T3K2T5C7U3Lz5v" =>
       "BPs89Kv"))

(t/deftest longest-common-subseq-seq-test
  (t/are [a b _ d] (= (longest-common-subseq (seq a) (seq b)) d)
       "kitten" "sitting"  => [\i \t \t \n]
       "Saturday" "Sunday" => [\S \u \d \a \y]
       "gumbo" "gambol"    => [\g \m \b \o]))

(t/deftest longest-common-subseq-clojure-test
  (t/are [a b _ d] (= (longest-common-subseq (seq a) (seq b)) d)
       [:k :i :t :t :e :n] [:s :i :t :t :i :n :g] => [:i :t :t :n]
       [:s :a :t :u :r :d :a :y] [:s :u :n :d :a :y] => [:s :u :d :a :y]
       [{:x 1 :y 3} {:x 2 :y 7} {:x 3 :y 2} {:x 8 :y 3}]
       [{:x 5 :y 3} {:x 1 :y 3} {:x 3 :y 2} {:x 2 :y 8} {:x 8 :y 3}] =>
       [{:x 1 :y 3} {:x 3 :y 2} {:x 8 :y 3}]))
