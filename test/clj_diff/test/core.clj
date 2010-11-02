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
         {:+ [[6 {:a 1}]], :- [0]})))

(deftest patch-test
  (is (= (patch [1 2 3 4 3 2 3 2 1 2 3]
                {:+ [[10 4 5 4 3]], :- [0 3 4 5 6 7]})
         [2 3 1 2 3 4 5 4 3]))
  (is (= (patch "abcab"
                {:+ [[2 \b]], :- [0 1]})
         [\c \b \a \b]))
  (is (= (patch "abcabba"
                {:+ [[2 \b] [6 \c]], :- [0 1 5]})
         [\c \b \a \b \a \c]))
  (is (= (patch [{:a 1} {:a 2} {:a 3} {:a 4} {:a 5} {:a 6} {:a 7}]
                {:+ [[6 {:a 1}]], :- [0]})
         [{:a 2} {:a 3} {:a 4} {:a 5} {:a 6} {:a 7} {:a 1}])))

