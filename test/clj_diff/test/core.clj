(ns clj-diff.test.core
  (:use [clj-diff.core] :reload)
  (:use [clojure.test]))

(deftest diff-test
  (is (= (diff [1 2 3 4 3 2 3 2 1 2 3] [2 3 1 2 3 4 5 4 3])
         {:+ [[10 4 5 4 3]], :- [0 3 4 5 6 7]}))
  (is (= (diff "abcab" "cbab")
         {:+ [[2 \b]], :- [0 1]}))
  (is (= (diff "abcabba" "cbabac")
         {:+ [[2 \b] [6 \c]], :- [0 1 5]}))
  (is (= (diff [{:a 1} {:a 2} {:a 3} {:a 4} {:a 5} {:a 6} {:a 7}]
               [{:a 2} {:a 3} {:a 4} {:a 5} {:a 6} {:a 7} {:a 1}])
         {:+ [[6 {:a 1}]], :- [0]}))
  (is (= (diff "FWdRgevf43"
               "T5C7U3Lz5v")
         {:+ [[8 \T \5 \C \7 \U] [9 \L \z \5 \v]]
          :- [0 1 2 3 4 5 6 7 8]}))
  (is (= (diff "s2dI8h9aK1FWdRgevf43"
               "5hs8L9T3K2T5C7U3Lz5v")
         {:+ [[-1 \5 \h] [5 \L] [7 \T \3] [18 \2 \T \5 \C \7 \U]
              [19 \L \z \5 \v]]
          :- [1 2 3 5 7 9 10 11 12 13 14 15 16 17 18]}))
  (is (= (diff "nBP8GaFHVls2dI8h9aK1FWdRgevf43"
               "925BCPcYhT5hs8L9T3K2T5C7U3Lz5v")
         {:+ [[0 \9 \2 \5]
              [1 \C]
              [9 \c \Y \h \T \5 \h]
              [15 \L] 
              [17 \T \3]
              [28 \2 \T \5 \C \7 \U]
              [29 \L \z \5 \v]]
          :- [0 3 4 5 6 7 8 9 11 12 13 15 17 19 20 21 22 23 24 25 26 27 28]}))
  (is (= (diff "aba" "aca")
         {:+ [[1 \c]], :- [1]})))

(deftest patch-test
  (is (= (patch "aba"
                {:+ [[1 \c]], :- [1]})
         [\a \c \a]))
  (is (= (patch [1 2 3 4 3 2 3 2 1 2 3]
                {:+ [[10 4 5 4 3]], :- [0 3 4 5 6 7]})
         [2 3 1 2 3 4 5 4 3]))
  (is (= (patch "abcab"
                {:+ [[2 \b]], :- [0 1]})
         [\c \b \a \b]))
  (is (= (patch "abcabba"
                {:+ [[2 \b] [6 \c]], :- [0 1 5]})
         [\c \b \a \b \a \c]))
  (is (= (patch "we1otec29q"
                {:+ [[9 \g \B \n \B \L \N \C \U \D \B]]
                 :- [0 1 2 3 4 5 6 7 8 9]})
         [\g \B \n \B \L \N \C \U \D \B]))
  (is (= (patch [{:a 1} {:a 2} {:a 3} {:a 4} {:a 5} {:a 6} {:a 7}]
                {:+ [[6 {:a 1}]], :- [0]})
         [{:a 2} {:a 3} {:a 4} {:a 5} {:a 6} {:a 7} {:a 1}]))
  (is (= (apply str (patch "FWdRgevf43"
                           {:+ [[8 \T \5 \C \7 \U] [9 \L \z \5 \v]]
                            :- [0 1 2 3 4 5 6 7 8]}))
         "T5C7U3Lz5v"))
  (is (= (apply str (patch "s2dI8h9aK1FWdRgevf43"
                           {:+ [[-1 \5 \h] [5 \L] [7 \T \3]
                                [18 \2 \T \5 \C \7 \U] [19 \L \z \5 \v]]
                            :- [1 2 3 5 7 9 10 11 12 13 14 15 16 17 18]}))
         "5hs8L9T3K2T5C7U3Lz5v"))
  (is (= (apply
          str
          (patch
           "nBP8GaFHVls2dI8h9aK1FWdRgevf43"
           {:+ [[0 \9 \2 \5]
                [1 \C]
                [9 \c \Y \h \T \5 \h]
                [15 \L] 
                [17 \T \3]
                [28 \2 \T \5 \C \7 \U]
                [29 \L \z \5 \v]]
            :- [0 3 4 5 6 7 8 9 11 12 13 15 17 19 20 21 22 23 24 25 26 27 28]}))
         "925BCPcYhT5hs8L9T3K2T5C7U3Lz5v")))

(deftest roundtrip
  (are [a b]
       (= b (apply str (patch a (diff a b))))

       "aba" "aca"))

