(ns clj-diff.test.optimisations
  (:use [clj-diff.optimisations])
  (:use [clojure.test]))

(deftest common-prefix-test
  (is (= (common-prefix "abcdef" "abcxyz")
         [3 "def" "xyz"]))
  (is (= (common-prefix "xy" "ab")
         [0 "xy" "ab"]))
  (is (= (common-prefix "ab" "ab")
         [2 "" ""])))

(deftest common-suffix-test
  (is (= (common-suffix "defabc" "xyzabc")
         [3 "def" "xyz"]))
  (is (= (common-suffix "xy" "ab")
         [0 "xy" "ab"]))
  (is (= (common-suffix "ab" "ab")
         [2 "" ""])))
