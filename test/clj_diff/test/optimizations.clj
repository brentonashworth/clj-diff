(ns clj-diff.test.optimizations
  (:use [clj-diff.optimizations])
  (:use [clojure.test]))

(deftest common-prefix-test
  (is (= (common-prefix "abcdef" "abcxyz")
         [3 "def" "xyz"]))
  (is (= (common-prefix (seq "abcdef") (seq "abcxyz"))
         [3 [\d \e \f] [\x \y \z]]))
  (is (= (common-prefix "xy" "ab")
         [0 "xy" "ab"]))
  (is (= (common-prefix "ab" "ab")
         [2 "" ""])))

(deftest common-suffix-test
  (is (= (common-suffix "defabc" "xyzabc")
         [3 "def" "xyz"]))
  (is (= (common-suffix (seq "defabc") (seq "xyzabc"))
         [3 [\d \e \f] [\x \y \z]]))
  (is (= (common-suffix "xy" "ab")
         [0 "xy" "ab"]))
  (is (= (common-suffix "ab" "ab")
         [2 "" ""])))

(deftest index-of-test
  (is (= (index-of "abcdef" "cd")
         2))
  (is (= (index-of (seq "abcdef") (seq "cd"))
         2))
  (is (= (index-of "abcdef" "cg")
         -1))
  (is (= (index-of (seq "abcdef") (seq "cg"))
         -1))
  (is (= (index-of "abcdefcdb" "cd" 0)
         2))
  (is (= (index-of (seq "abcdefcdb") (seq "cd") 0)
         2))
  (is (= (index-of "abcdefcdb" "cd" 3)
         6))
  (is (= (index-of (seq "abcdefcdb") (seq "cd") 3)
         6)))

(deftest subsequence-test
  (is (= (subsequence "abcdef" 2)
         "cdef"))
  (is (= (subsequence (seq "abcdef") 2)
         [\c \d \e \f]))
  (is (= (subsequence "abcdef" 2 4)
         "cd"))
  (is (= (subsequence (seq "abcdef") 2 4)
         [\c \d])))

(deftest diff*-test
  (let [t (fn [a b] (#'clj-diff.optimizations/diff* a b (constantly nil)))]
    (is (= (t "" "abc")
           {:+ [[-1 \a \b \c]]
            :- []}))
    (is (= (t "abc" "")
           {:+ []
            :- [0 1 2]}))
    (is (= (t "abc" "xyzabcmnop")
           {:+ [[-1 \x \y \z] [5 \m \n \o \p]]
            :- []}))
    (is (= (t "abc" "abcm")
           {:+ [[2 \m]]
            :- []}))
    (is (= (t "abcm" "abc")
           {:+ []
            :- [3]}))
    (is (= (t "abc" "mabc")
           {:+ [[-1 \m]]
            :- []}))
    (is (= (t "mabc" "abc")
           {:+ []
            :- [0]}))
    (is (= (t "mabc" "abc")
           {:+ []
            :- [0]}))
    (is (nil? (t "abcac" "cbab")))))

(deftest half-match-test
  (let [t #'clj-diff.optimizations/half-match]
    (is (= (t "a" "b")
           nil))
    (is (= (t "bb" "bbg")
           nil))
    (is (= (t "ahgt" "bhahgtgbh")
           nil))
    (is (= (t "aaapppppb" "cpppppdddd")
           ["ppppp" "aaa" "b" "c" "dddd"]))
    (is (= (t "apppppaab" "pppppcdddd")
           ["ppppp" "a" "aab" "" "cdddd"]))
    (is (= (t "apppppaab" "cddddppppp")
           ["ppppp" "a" "aab" "cdddd" ""]))
    (is (= (t (seq "aaapppppb") (seq "cpppppdddd"))
           [[\p \p \p \p \p] [\a \a \a] [\b] [\c] [\d \d \d \d]]))
    (is (= (t "aaappppbb" "cppppddddd")
           nil))))
