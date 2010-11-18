(ns clj-diff.test.myers
  (:use [clj-diff.myers]
        [clj-diff [core :only (edit-distance patch)]])
  (:use [clojure.test]))

(def a1 (vec (cons nil "abcabba")))
(def b1 (vec (cons nil "cbabac")))

(def a3 (vec (cons nil (seq "Udghi4"))))
(def b3 (vec (cons nil (seq "Udhki4"))))

(deftest next-point-test
  (let [t #'clj-diff.myers/next-point]
    (is (= (t 0 0 {1 0})
           [0 0]))
    (is (= (t 1 -1 {0 0 1 0})
           [0 1]))
    (is (= (t 1 1 {0 0 1 0 -1 0})
           [1 0]))
    (is (= (t 2 -2 {0 0 1 1 -1 0})
           [0 2]))
    (is (= (t 2 0 {0 0 1 1 -1 0 -2 2})
           [1 1]))
    (is (= (t 2 2 {0 2 1 1 -1 0 -2 2})
           [2 0]))
    ;; simulate diffing 'aba' and 'aca'
    (is (= (t 1 -1 {1 0, 0 1})
           [1 2]))
    (is (= (t 1 1 {1 0, 0 1})
           [2 1]))
    (is (= (t 2 -2 {1 2, 0 1, -1 1})
           [1 3]))
    (is (= (t 2 0 {1 2, 0 1, -1 1})
           [2 2]))))

(deftest snake-test
  (let [n (dec (count a1))
        m (dec (count b1))
        t (fn [x y] (#'clj-diff.myers/snake a1 b1 x y n m))]
    (is (= (t 0 0) 0))
    (is (= (t 1 0) 1))
    (is (= (t 1 1) 2))
    (is (= (t 3 2) 5))
    (is (= (t 0 2) 2))
    (is (= (t 2 1) 2)))
  (let [n (dec (count a3))
        m (dec (count b3))
        t (fn [x y] (#'clj-diff.myers/snake a3 b3 x y n m))]
    (is (= (t 0 0) 2))
    (is (= (t 3 2) 4))))

(deftest band-dialonals-test
  (let [t #'clj-diff.myers/band-diagonals]
    (is (= (t 0)
           [0]))
    (is (= (t 1)
           [-1 1]))
    (is (= (t 2)
           [-2 0 2]))
    (is (= (t 3)
           [-3 -1 1 3]))))

(def a2 (vec (cons nil "abcab")))
(def b2 (vec (cons nil "cbab")))

(deftest search-d-path-test
  (let [n (dec (count a1))
        m (dec (count b1))
        t (fn [d v] (#'clj-diff.myers/search-d-band a1 b1 d n m v))]
    (is (= (t 0 {1 0})
           [{0 0, 1 0} 0 0]))
    (is (= (t 1 {0 0, 1 0})
           [{0 0, -1 0, 1 1} 0 0]))
    (is (= (t 2 {0 0, -1 0, 1 1})
           [{0 2, -1 0, 1 1, -2 2, 2 3} 0 0]))
    (is (= (t 3 {0 2, -1 0, 1 1, -2 2, 2 3})
           [{0 2, -1 4, 1 5, -2 2, 2 3, -3 3, 3 5} 0 0])))
  (let [n (dec (count a2))
        m (dec (count b2))
        t (fn [d v] (#'clj-diff.myers/search-d-band a2 b2 d n m v))]
    (is (= (t 3 {0 2, -1 0, 1 1, -2 2, 2 3})
           [{0 2, -1 3, 1 5, -2 2, 2 3, -3 2} 5 4])))
  (let [t (fn [d v] (#'clj-diff.myers/search-d-band [nil \a \b \a]
                                                    [nil \a \c \a]
                                                    d 3 3 v))]
    (is (= (t 0 {1 0})
           [{1 0, 0 1} 0 0]))
    (is (= (t 1 {1 0, 0 1})
           [{-1 1, 1 2, 0 1} 0 0]))
    (is (= (t 2 {-1 1, 1 2, 0 1})
           [{-2 1, -1 1, 1 2, 0 3} 3 3]))))

(def ses-results [3 1 {3 {-3 2, 2 3, -2 2, 1 5, -1 3, 0 2},
                       2 {2 3, -2 2, 1 1, -1 0, 0 2},
                       1 {1 1, -1 0, 0 0},
                       0 {0 0}}])

(deftest ses-test
  (is (= (ses a2 b2)
         ses-results))
  (is (= (first (ses a2 b2))
         3)))

(deftest path-test
  (let [t #'clj-diff.myers/path]
    (is (= (apply t a2 b2 ses-results)
           [[0 0] [1 0] [2 0] [3 1] [3 2] [4 3] [5 4]]))
    (is (= (apply t a1 b1 (ses a1 b1))
           [[0 0] [1 0] [2 0] [3 1] [3 2] [4 3] [5 4] [6 4] [7 5] [7 6]]))
    (is (= (apply t [nil \a \b \a] [nil \a \c \a]
                  [2 0 {2 {-2 1, 1 2, -1 1, 0 3}
                        1 {1 2, -1 1, 0 1}
                        0 {0 1}}])
           [[0 0] [1 1] [2 1] [2 2] [3 3]]))))

(deftest path->script-test
  (let [t #'clj-diff.myers/path->script]
    (is (= (t b2 [[0 0] [1 0] [2 0] [3 1] [3 2] [4 3] [5 4]])
           {:+ [[2 \b]], :- [0 1]}))))

(deftest diff-test
  (let [t (fn [a b] (edit-distance (diff a b)))]
    (is (= (t "acebdabbabed" "acbdeacbed")
           6))
    (is (= (t [1 2 3 4 3 2 3 2 1 2 3] [2 3 1 2 3 4 5 4 3])
           10))
    (is (= (t "abcab" "cbab")
           3))
    (is (= (t "abcabba" "cbabac")
           5))
    (is (= (t "IF2qvWMa21" "IFvclxMax1")
           8))
    (is (= (t "Udghi4" "Udhki4")
           2))))

(deftest roundtrip
  (are [a b]
       (= b (patch a (diff a b)))
       
       "aba" "aca"
       "abcabba" "cbabac"
       "acebdabbabed" "acbdeacbed"
       "FWdRgevf43" "T5C7U3Lz5v"
       "s2dI8h9aK1FWdRgevf43" "5hs8L9T3K2T5C7U3Lz5v"
       "nBP8GaFHVls2dI8h9aK1FWdRgevf43" "925BCPcYhT5hs8L9T3K2T5C7U3Lz5v"
       "aba" "aca"))
