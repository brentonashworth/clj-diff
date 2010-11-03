(ns clj-diff.test.myers
  (:use [clj-diff.myers] :reload)
  (:use [clojure.test]))

(def a1 (vec (cons nil "abcabba")))
(def b1 (vec (cons nil "cbabac")))

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

(deftest longest-snake-test
  (let [n (dec (count a1))
        m (dec (count b1))
        t (fn [x y] (#'clj-diff.myers/longest-snake a1 b1 x y n m))]
    (is (= (t 0 0) 0))
    (is (= (t 1 0) 1))
    (is (= (t 1 1) 2))
    (is (= (t 3 2) 5))
    (is (= (t 0 2) 2))
    (is (= (t 2 1) 2))))

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

(deftest optimal-path-test
  (let [t #'clj-diff.myers/optimal-path]
    (is (= (apply t a2 b2 ses-results)
           [[0 0] [1 0] [2 0] [3 1] [3 2] [4 3] [5 4]]))
    (is (= (apply t a1 b1 (ses a1 b1))
           [[0 0] [1 0] [2 0] [3 1] [3 2] [4 3] [5 4] [6 4] [7 5] [7 6]]))
    (is (= (apply t [nil \a \b \a] [nil \a \c \a]
                  [2 0 {2 {-2 1, 1 2, -1 1, 0 3}
                        1 {1 2, -1 1, 0 1}
                        0 {0 1}}])
           [[0 0] [1 1] [2 1] [2 2] [3 3]]))))

(deftest diff-test
  (is (= (diff [1 2 3 4 3 2 3 2 1 2 3] [2 3 1 2 3 4 5 4 3])
         {:+ [[10 4 5 4 3]], :- [0 3 4 5 6 7]}))
  (is (= (diff "abcab" "cbab")
         {:+ [[2 \b]], :- [0 1]}))
  (is (= (diff "abcabba" "cbabac")
         {:+ [[2 \b] [6 \c]], :- [0 1 5]})))
