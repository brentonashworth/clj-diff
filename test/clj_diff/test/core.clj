(ns clj-diff.test.core
  (:use [clj-diff.core] :reload)
  (:use [clojure.test]))

(deftest diff-test
  (let [t (fn [a b] (edit-distance (diff a b)))]
    (is (= (t [1 2 3 4 3 2 3 2 1 2 3] [2 3 1 2 3 4 5 4 3])
           10))
    (is (= (t [nil 1 2] [1 2])
           1))
    (is (= (t [1 2] [nil 1 2])
           1))
    (is (= (t [nil 1 2] [nil 1 2])
           0))
    (is (= (t "abcab" "cbab")
           3))
    (is (= (t "abcabba" "cbabac")
           5))
    (is (= (t [{:a 1} {:a 2} {:a 3} {:a 4} {:a 5} {:a 6} {:a 7}]
              [{:a 2} {:a 3} {:a 4} {:a 5} {:a 6} {:a 7} {:a 1}])
           2))
    (is (= (t "FWdRgevf43" "T5C7U3Lz5v")
           18))
    (is (= (t "s2dI8h9aK1FWdRgevf43"
              "5hs8L9T3K2T5C7U3Lz5v")
           30))
    (is (= (t "nBP8GaFHVls2dI8h9aK1FWdRgevf43"
              "925BCPcYhT5hs8L9T3K2T5C7U3Lz5v")
           46))
    (is (= (t "aba" "aca")
           2))))

(deftest patch-test
  (is (= (patch "aba"
                {:+ [[1 \c]], :- [1]})
         "aca"))
  (is (= (patch [1 2 3 4 3 2 3 2 1 2 3]
                {:+ [[10 4 5 4 3]], :- [0 3 4 5 6 7]})
         [2 3 1 2 3 4 5 4 3]))
  (is (= (patch "abcab"
                {:+ [[2 \b]], :- [0 1]})
         "cbab"))
  (is (= (patch "abcabba"
                {:+ [[2 \b] [6 \c]], :- [0 1 5]})
         "cbabac"))
  (is (= (patch "we1otec29q"
                {:+ [[9 \g \B \n \B \L \N \C \U \D \B]]
                 :- [0 1 2 3 4 5 6 7 8 9]})
         "gBnBLNCUDB"))
  (is (= (patch [{:a 1} {:a 2} {:a 3} {:a 4} {:a 5} {:a 6} {:a 7}]
                {:+ [[6 {:a 1}]], :- [0]})
         [{:a 2} {:a 3} {:a 4} {:a 5} {:a 6} {:a 7} {:a 1}]))
  (is (= (patch "FWdRgevf43"
                {:+ [[8 \T \5 \C \7 \U] [9 \L \z \5 \v]]
                 :- [0 1 2 3 4 5 6 7 8]})
         "T5C7U3Lz5v"))
  (is (= (patch "s2dI8h9aK1FWdRgevf43"
                {:+ [[-1 \5 \h] [5 \L] [7 \T \3]
                     [18 \2 \T \5 \C \7 \U] [19 \L \z \5 \v]]
                 :- [1 2 3 5 7 9 10 11 12 13 14 15 16 17 18]})
         "5hs8L9T3K2T5C7U3Lz5v"))
  (is (= (patch
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

(deftest roundtrip
  (are [a b]
       (= b (patch a (diff a b)))

       [nil 42] [42]
       [42] [42 nil]
       "aba" "aca"
       "abcabba" "cbabac"
       "FWdRgevf43" "T5C7U3Lz5v"
       "s2dI8h9aK1FWdRgevf43" "5hs8L9T3K2T5C7U3Lz5v"
       "nBP8GaFHVls2dI8h9aK1FWdRgevf43" "925BCPcYhT5hs8L9T3K2T5C7U3Lz5v"
       "aba" "aca"))

(deftest edit-distance-test
  (is (= (edit-distance (diff "aba" "aca"))
         2))
  (is (= (edit-distance "aba" "aca")
         2))
  (is (= (edit-distance (diff "abcabba" "cbabac"))
         5))
  (is (= (edit-distance "abcabba" "cbabac")
         5))
  (is (= (edit-distance (diff "nBP8GaFHVls2dI8h9aK1FWdRgevf43"
                              "925BCPcYhT5hs8L9T3K2T5C7U3Lz5v"))
         46)))

(deftest levenshtein-distance-test
  (are [a b d] (= (levenshtein-distance a b) d)
       "aba" "aba" 0
       "aba" "ada" 1
       "abca" "aca" 1
       "abma" "aca" 2
       "kitten" "sitting" 3
       "Saturday" "Sunday" 3
       "gumbo" "gambol" 2
       ;; TODO: The current levenshtein implementation incorrectly
       ;; computes the distance from the diff on the following line.
       ;; See the warning on `clj-diff.core/levenshtein-distance`
       ;; for details of why this fails.
       ;; See also:
       ;; https://github.com/brentonashworth/clj-diff/pull/1#issuecomment-1146424
       ;;"nBP8GaFHVls2dI8h9aK1FWdRgevf43" "925BCPcYhT5hs8L9T3K2T5C7U3Lz5v" 28
       [1 2 4] [1 2 4 3] 1))

(deftest longest-common-subseq-test
  (are [a b _ d] (= (longest-common-subseq a b) d)
       "aba" "aba"         => "aba"
       "aba" "ada"         => "aa"
       "abca" "aca"        => "aca"
       "abma" "aca"        => "aa"
       "kitten" "sitting"  => "ittn"
       "Saturday" "Sunday" => "Suday"
       "gumbo" "gambol"    => "gmbo"
       "nBP8GaFHVls2dI8h9aK1FWdRgevf43" "925BCPcYhT5hs8L9T3K2T5C7U3Lz5v" =>
       "BPs89Kv"))

(deftest longest-common-subseq-seq-test
  (are [a b _ d] (= (longest-common-subseq (seq a) (seq b)) d)
       "kitten" "sitting"  => [\i \t \t \n]
       "Saturday" "Sunday" => [\S \u \d \a \y]
       "gumbo" "gambol"    => [\g \m \b \o]))

(deftest longest-common-subseq-clojure-test
  (are [a b _ d] (= (longest-common-subseq (seq a) (seq b)) d)
       [:k :i :t :t :e :n] [:s :i :t :t :i :n :g] => [:i :t :t :n]
       [:s :a :t :u :r :d :a :y] [:s :u :n :d :a :y] => [:s :u :d :a :y]
       [{:x 1 :y 3} {:x 2 :y 7} {:x 3 :y 2} {:x 8 :y 3}]
       [{:x 5 :y 3} {:x 1 :y 3} {:x 3 :y 2} {:x 2 :y 8} {:x 8 :y 3}] =>
       [{:x 1 :y 3} {:x 3 :y 2} {:x 8 :y 3}]))
