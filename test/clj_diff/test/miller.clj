(ns clj-diff.test.miller
  (:use [clj-diff.miller] :reload)
  (:use [clojure.test]))

(def a1 (vec (cons nil "acebdabbabed")))
(def b1 (vec (cons nil "acbdeacbed")))

(def a2 (vec (cons nil "abc")))
(def b2 (vec (cons nil "xyz")))

(deftest next-x-test
  (let [t #'clj-diff.miller/next-x]
    (is (= (t 0  {             }) 0))
    (is (= (t 1  {0 2          }) 3))
    (is (= (t 2  {0 2, 1 5     }) 6))
    (is (= (t -1 {0 2, 1 5, 2 6}) 2))

    (is (= (t  0 {            0 0     }) 0))
    (is (= (t -1 {            0 0     }) 0))
    (is (= (t  0 {      -1 0, 0 1     }) 1))
    (is (= (t  0 {      -1 0, 0 1     }) 1))
    (is (= (t -2 {      -1 0, 0 1     }) 0))
    (is (= (t -1 {-2 0, -1 0, 0 1     }) 1))
    (is (= (t  1 {-2 0, -1 1, 0 1     }) 2))
    (is (= (t  0 {-2 0, -1 1, 0 1, 1 2}) 2))))


(deftest snake-test
  (let [n (dec (count a1))
        m (dec (count b1))
        t (fn [k fp] (#'clj-diff.miller/snake a1 b1 n m k fp))]
    ;; p = 0
    (is (= (t  0 {                               }) 2))
    (is (= (t  1 {            0 2                }) 5))
    (is (= (t  2 {            0 2, 1 5           }) 6))
    ;;           {            0 2, 1 5, 2  6     }
    ;; p = 1 
    (is (= (t -1 {            0 2, 1 5, 2  6     }) 2))
    (is (= (t  0 {      -1 2, 0 2, 1 5, 2  6     }) 6))
    (is (= (t  1 {      -1 2, 0 6, 1 5, 2  6     }) 7))
    (is (= (t  2 {      -1 2, 0 6, 1 7, 2  6     }) 8))
    ;;           {      -1 2, 0 6, 1 7, 2  8     }
    ;; p = 2
    (is (= (t -2 {      -1 2, 0 6, 1 7, 2  8     }) 3))
    (is (= (t -1 {-2 3, -1 2, 0 6, 1 7, 2  8     }) 7))
    (is (= (t  0 {-2 3, -1 7, 0 6, 1 7, 2  8     }) 8))
    (is (= (t  1 {-2 3, -1 7, 0 8, 1 7, 2  8     }) 9))
    (is (= (t  3 {-2 3, -1 7, 0 8, 1 9, 2  8     }) 9))
    (is (= (t  2 {-2 3, -1 7, 0 8, 1 9, 2  8, 3 9}) 12))
    ;;           {-2 3, -1 7, 0 8, 1 9, 2 12, 3 9}
    )
  (let [n (dec (count a2))
        m (dec (count b2))
        t (fn [k fp] (#'clj-diff.miller/snake a2 b2 n m k fp))]

    (is (= (t 0 {0 0}) 0))
    (is (= (t -2 {-1 0, 0 1}) 0))
    (is (= (t -1 {-2 0, -1 0, 0 1}) 1))))

(deftest search-p-band-test
  (let [n (dec (count a1))
        m (dec (count b1))
        t (fn [p fp] (#'clj-diff.miller/search-p-band a1 b1 n m 2 p fp))]
    (is (= (t 0 {})
           {0 2, 1 5, 2 6}))
    (is (= (t 1 {0 2, 1 5, 2 6})
           {-1 2, 0 6, 1 7, 2 8}))
    (is (= (t 2 {-1 2, 0 6, 1 7, 2 8})
           {-2 3, -1 7, 0 8, 1 9, 2 12, 3 9})))
  (let [n (dec (count a2))
        m (dec (count b2))
        t (fn [p fp] (#'clj-diff.miller/search-p-band a2 b2 n m 0 p fp))]
    (is (= (t 0 {})
           {0 0}))
    (is (= (t 1 {0 0})
           {-1 0, 0 1}))
    (is (= (t 2 {-1 0, 0 1})
           {-2 0, -1 1, 0 2, 1 2}))
    (is (= (t 3 {-2 0, -1 1, 0 2, 1 2})
           {-3 0, -2 1, -1 2, 0 3, 1 3, 2 3}))))

(deftest ses-test
  (is (= (ses a1 b1)
         [2 2 {0 {0 2, 1 5, 2 6}
               1 {-1 2, 0 6, 1 7, 2 8}
               2 {-2 3, -1 7, 0 8, 1 9, 2 12, 3 9}}]))
  (is (= (ses a2 b2)
         [3 0 {0 {0 0}
               1 {-1 0, 0 1}
               2 {-2 0, -1 1, 0 2, 1 2}
               3 {-3 0, -2 1, -1 2, 0 3, 1 3, 2 3}}])))

(deftest path-test
  (let [fp {0 {            0 2, 1 5, 2 6      }
            1 {      -1 2, 0 6, 1 7, 2 8      }
            2 {-2 3, -1 7, 0 8, 1 9, 2 12, 3 9}}
        t (fn [p fp] (#'clj-diff.miller/path a1 b1 p 2 fp))]
    (is (= (t 2 fp)
           [[0 0] [1 1] [2 2] [3 2] [4 3] [5 4] [5 5] [6 6] [6 7] [7 8] [8 8]
            [9 8] [10 8] [11 9] [12 10]])))
  (let [fp {0 {                  0 0          }
            1 {            -1 0, 0 1          }
            2 {      -2 0, -1 1, 0 2, 1 2     }
            3 {-3 0, -2 1, -1 2, 0 3, 1 3, 2 3}}
        t (fn [p fp] (#'clj-diff.miller/path a2 b2 p 0 fp))]
    (is (= (t 3 fp)
           [[0 0] [0 1] [1 1] [2 1] [3 1] [3 2] [3 3]]))))

(deftest diff-test
  (is (= (diff "acebdabbabed" "acbdeacbed")
         {:+ [[4 \e] [5 \c]] :- [2 7 8 9]}))
  (is (= (diff [1 2 3 4 3 2 3 2 1 2 3] [2 3 1 2 3 4 5 4 3])
         {:+ [[-1 2 3] [3 5 4]], :- [5 6 7 8 9 10]}))
  (is (= (diff "abcab" "cbab")
         {:+ [[0 \c]], :- [0 2]}))
  (is (= (diff "abcabba" "cbabac")
         {:+ [[0 \c] [6 \c]], :- [0 2 5]}))
  (is (= (diff "acbdeacbed" "acebdabbabed")
         {:+ [[1 \e] [7 \b \a \b]] :- [4 6]}))
  (is (= (diff "abc" "xyz")
         {:+ [[-1 \x] [2 \y \z]] :- [0 1 2]})))
