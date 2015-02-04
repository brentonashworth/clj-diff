(ns clj-diff.test.miller
  #+cljs (:require-macros [cemerick.cljs.test :as t])
  (:require [clj-diff.core :as core]
            [clj-diff.miller :as miller]
            #+cljs [cemerick.cljs.test :as t]
            #+clj  [clojure.test :as t]))

(def a1 (vec (cons nil "acebdabbabed")))
(def b1 (vec (cons nil "acbdeacbed")))

(def a2 (vec (cons nil "abc")))
(def b2 (vec (cons nil "xyz")))

(def a3 (vec (cons nil (seq "Udghi4"))))
(def b3 (vec (cons nil (seq "Udhki4"))))

(def a4 (vec (cons nil (seq "IFvclxMax1"))))
(def b4 (vec (cons nil (seq "IF2qvWMa21"))))

(def a5 (vec (cons nil (seq "sobibm"))))
(def b5 (vec (cons nil (seq "sobmgc"))))

(t/deftest next-x-test
  (let [t #'miller/next-x]
    (t/is (= (t 0  {             }) 0))
    (t/is (= (t 1  {0 2          }) 3))
    (t/is (= (t 2  {0 2, 1 5     }) 6))
    (t/is (= (t -1 {0 2, 1 5, 2 6}) 2))

    (t/is (= (t  0 {            0 0     }) 0))
    (t/is (= (t -1 {            0 0     }) 0))
    (t/is (= (t  0 {      -1 0, 0 1     }) 1))
    (t/is (= (t  0 {      -1 0, 0 1     }) 1))
    (t/is (= (t -2 {      -1 0, 0 1     }) 0))
    (t/is (= (t -1 {-2 0, -1 0, 0 1     }) 1))
    (t/is (= (t  1 {-2 0, -1 1, 0 1     }) 2))
    (t/is (= (t  0 {-2 0, -1 1, 0 1, 1 2}) 2))))

(t/deftest snake-test
  (let [n (dec (count a1))
        m (dec (count b1))
        t (fn [k fp] (#'miller/snake a1 b1 n m k fp))]
    ;; p = 0
    (t/is (= (t  0 {                               }) 2))
    (t/is (= (t  1 {            0 2                }) 5))
    (t/is (= (t  2 {            0 2, 1 5           }) 6))
    ;;           {            0 2, 1 5, 2  6     }
    ;; p = 1
    (t/is (= (t -1 {            0 2, 1 5, 2  6     }) 2))
    (t/is (= (t  0 {      -1 2, 0 2, 1 5, 2  6     }) 6))
    (t/is (= (t  1 {      -1 2, 0 6, 1 5, 2  6     }) 7))
    (t/is (= (t  2 {      -1 2, 0 6, 1 7, 2  6     }) 8))
    ;;           {      -1 2, 0 6, 1 7, 2  8     }
    ;; p = 2
    (t/is (= (t -2 {      -1 2, 0 6, 1 7, 2  8     }) 3))
    (t/is (= (t -1 {-2 3, -1 2, 0 6, 1 7, 2  8     }) 7))
    (t/is (= (t  0 {-2 3, -1 7, 0 6, 1 7, 2  8     }) 8))
    (t/is (= (t  1 {-2 3, -1 7, 0 8, 1 7, 2  8     }) 9))
    (t/is (= (t  3 {-2 3, -1 7, 0 8, 1 9, 2  8     }) 9))
    (t/is (= (t  2 {-2 3, -1 7, 0 8, 1 9, 2  8, 3 9}) 12))
    ;;           {-2 3, -1 7, 0 8, 1 9, 2 12, 3 9}
    )
  (let [n (dec (count a2))
        m (dec (count b2))
        t (fn [k fp] (#'miller/snake a2 b2 n m k fp))]
    (t/is (= (t 0 {0 0}) 0))
    (t/is (= (t -2 {-1 0, 0 1}) 0))
    (t/is (= (t -1 {-2 0, -1 0, 0 1}) 1)))
  (let [n (dec (count a3))
        m (dec (count b3))
        t (fn [k fp] (#'miller/snake a3 b3 n m k fp))]
    (t/is (= (t  0 {                    }) 2))
    (t/is (= (t -1 {            0 2     }) 2))
    (t/is (= (t  0 {      -1 2, 0 2     }) 3))
    (t/is (= (t -2 {      -1 2, 0 3     }) 2))
    (t/is (= (t -1 {-2 2, -1 2, 0 3     }) 3))
    (t/is (= (t  1 {-2 2, -1 3, 0 3     }) 4))
    (t/is (= (t  0 {-2 2, -1 3, 0 3, 1 4}) 6)))
  (let [n (dec (count a4))
        m (dec (count b4))
        t (fn [k fp] (#'miller/snake a4 b4 n m k fp))]
    (t/is (= (t   0 {         }) 2))
    (t/is (= (t  -1 {      0 2}) 2))
    (t/is (= (t   0 {-1 2, 0 2}) 3))
    (t/is (= (t  -2 {-1 2, 0 3}) 3))))

(t/deftest p-band-diagonals-test
  (let [t #'miller/p-band-diagonals]
    (t/is (= (t 0 2) [0 1 2]))))

(t/deftest search-p-band-test
  (let [n (dec (count a1))
        m (dec (count b1))
        t (fn [p fp] (#'miller/search-p-band a1 b1 n m 2 p fp))]
    (t/is (= (t 0 {})
           {0 2, 1 5, 2 6}))
    (t/is (= (t 1 {0 2, 1 5, 2 6})
           {-1 2, 0 6, 1 7, 2 8, 3 7}))
    (t/is (= (t 2 {-1 2, 0 6, 1 7, 2 8, 3 7})
           {-2 3, -1 7, 0 8, 1 9, 2 12, 3 9, 4 8})))
  (let [n (dec (count a2))
        m (dec (count b2))
        t (fn [p fp] (#'miller/search-p-band a2 b2 n m 0 p fp))]
    (t/is (= (t 0 {})
           {0 0}))
    (t/is (= (t 1 {0 0})
           {-1 0, 0 1, 1 1}))
    (t/is (= (t 2 {-1 0, 0 1, 1 1})
           {-2 0, -1 1, 0 2, 1 2, 2 2}))
    (t/is (= (t 3 {-2 0, -1 1, 0 2, 1 2, 2 2})
           {-3 0, -2 1, -1 2, 0 3, 1 3, 2 3, 3 3})))
  (let [n (dec (count a3))
        m (dec (count b3))
        t (fn [p fp] (#'miller/search-p-band a3 b3 n m 0 p fp))]
    (t/is (= (t 0 {})
           {0 2}))
    (t/is (= (t 1 {0 2})
           {-1 2, 0 6, 1 4})))
  (let [n (dec (count a4))
        m (dec (count b4))
        t (fn [p fp] (#'miller/search-p-band a4 b4 n m 0 p fp))]
    (t/is (= (t 0 {})
           {0 2}))
    (t/is (= (t 1 {0 2})
           {-1 2, 0 3, 1 3}))
    (t/is (= (t 2 {-1 2, 0 3, 1 3})
           {-2 3, -1 4, 0 5, 1 4, 2 4}))
    (t/is (= (t 3 {-2 3, -1 4, 0 5, 1 4, 2 4})
           {-3 3, -2 4, -1 5, 0 8, 1 6, 2 5, 3 5}))
    (t/is (= (t 4 {-3 3, -2 4, -1 5, 0 8, 1 6, 2 5, 3 5})
           {-4 3, -3 4, -2 5, -1 8, 0 10, 1 9, 2 7, 3 6, 4 6}))))

(def a1-graph {0 {            0 2, 1 5, 2 6           }
               1 {      -1 2, 0 6, 1 7, 2 8, 3 7      }
               2 {-2 3, -1 7, 0 8, 1 9, 2 12, 3 9, 4 8}})

(def a2-graph {0 {                  0 0               }
               1 {            -1 0, 0 1, 1 1          }
               2 {      -2 0, -1 1, 0 2, 1 2, 2 2     }
               3 {-3 0, -2 1, -1 2, 0 3, 1 3, 2 3, 3 3}})

(def a3-graph {0 {      0 2     }
               1 {-1 2, 0 6, 1 4}})

(def a4-graph {0 {                        0  2                    }
               1 {                  -1 2, 0  3, 1 3               }
               2 {            -2 3, -1 4, 0  5, 1 4, 2 4          }
               3 {      -3 3, -2 4, -1 5, 0  8, 1 6, 2 5, 3 5     }
               4 {-4 3, -3 4, -2 5, -1 8, 0 10, 1 9, 2 7, 3 6, 4 6}})

(def a5-graph {0 {0 3}
               1 {1 4, -1 3, 0 4}
               2 {2 6, -2 3, 1 6, -1 4, 0 6}})

(t/deftest ses-test
  (t/is (= (miller/ses a1 b1)
         [2 2 a1-graph]))
  (t/is (= (miller/ses a2 b2)
         [3 0 a2-graph]))
  (t/is (= (miller/ses a3 b3)
         [1 0 a3-graph]))
  (t/is (= (miller/ses a4 b4)
         [4 0 a4-graph]))
  (t/is (= (miller/ses a5 b5)
         [2 0 a5-graph])))

(t/deftest edit-dist-test
  (let [t (fn [p k] (#'miller/edit-dist 0 p k))]
    (t/is (= (t 4 1)  7))
    (t/is (= (t 4 0)  8))
    (t/is (= (t 4 -1) 7))
    (t/is (= (t 2 0) 4))
    (t/is (= (t 2 1) 3))
    (t/is (= (t 2 2) 2))
    (t/is (= (t 1 1) 1))
    (t/is (= (t 0 0) 0)))
  (let [t (fn [p k] (#'miller/edit-dist 2 p k))]
    (t/is (= (t 2 3) 5))
    (t/is (= (t 2 1) 5))
    (t/is (= (t 2 -1) 3))))

(t/deftest p-value-up-test
  (let [t (fn [p k] (#'miller/p-value-up 0 p k))]
    (t/are [p k p*] (= (t p k) p*)
         1 -1 0
         1  0 1
         2  1 2
         3  2 3
         2 -2 1
         2 -1 1
         2  0 2
         3  1 3))
  (let [t (fn [p k] (#'miller/p-value-up 2 p k))]
    (t/are [p k p*] (= (t p k) p*)
         1 -1 0
         1  0 0
         1  1 0
         1  2 1
         2  3 2
         3  4 3)))

(t/deftest p-value-left-test
  (let [t (fn [p k] (#'miller/p-value-left 0 p k))]
    (t/are [p k p*] (= (t p k) p*)
         1  1 0
         1  0 1
         2 -1 2
         3 -2 3
         2  2 1
         2  1 1
         2  0 2
         3 -1 3))
  (let [t (fn [p k] (#'miller/p-value-left 2 p k))]
    (t/are [p k p*] (= (t p k) p*)
         0  1 0
         1  0 1
         2 -1 2
         0  2 0
         1  1 1
         2  0 2
         3 -1 3
         1  3 0
         1  2 1
         2  1 2
         3  0 3
         4 -1 4)))

(t/deftest look-up-test
  (let [t (fn [p x k] (#'miller/look-up a1-graph 2 p x k))]
    (t/is (= (t 2 9 2) {:edit :insert :x 9 :p 2 :k 3 :d 5}))
    (t/is (nil? (t 2 9 3)))
    (t/is (= (t 1 5 0) {:edit :insert :x 5 :p 0 :k 1 :d 1}))
    (t/is (nil? (t 1 7 1))))
  (let [t (partial #'miller/look-up a4-graph 0)]
    (t/is (= (t 4 9 0) {:edit :insert :x 9 :p 4 :k 1 :d 7}))
    (t/is (= (t 3 6 0) {:edit :insert :x 6 :p 3 :k 1 :d 5}))
    (t/is (nil? (t 2 6 1)))
    (t/is (= (t 3 5 -1) {:edit :insert :x 5 :p 2 :k 0 :d 4}))
    (t/is (= (t 1 2 -1) {:edit :insert :x 2 :p 0 :k 0 :d 0}))
    (t/is (nil? (t 2 3 -2))))
  (let [t (partial #'miller/look-up a3-graph 0)]
    (t/is (= (t 1 4 0) {:edit :insert :x 4 :p 1 :k 1 :d 1})))
  (let [t (partial #'miller/look-up a2-graph 0)]
    (t/is (= (t 3 0 -3) {:edit :insert :x 0 :p 2 :k -2 :d 2})))
  (let [t (partial #'miller/look-up a5-graph 0)]
    (t/is (= (t 2 6 0) {:edit :insert :x 6 :p 2 :k 1 :d 3}))
    (t/is (= (t 2 6 1) {:edit :insert :x 6 :p 2 :k 2 :d 2}))))

(t/deftest look-left-test
  (let [t (partial #'miller/look-left a1-graph 2)]
    (t/is (nil? (t 2 9 2)))
    (t/is (= (t 2 9 3) {:edit :delete :x 8 :p 1 :k 2 :d 4}))
    (t/is (= (t 1 8 2) {:edit :delete :x 7 :p 1 :k 1 :d 3}))
    (t/is (nil? (t 1 5 0))))
  (let [t (partial #'miller/look-left a4-graph 0)]
    (t/is (= (t 4 9 0) {:edit :delete :x 8 :p 4 :k -1 :d 7}))
    (t/is (= (t 3 6 0) {:edit :delete :x 5 :p 3 :k -1 :d 5}))
    (t/is (= (t 3 5 -1) {:edit :delete :x 4 :p 3 :k -2 :d 4}))
    (t/is (nil? (t 3 3 -3))))
  (let [t (partial #'miller/look-left a2-graph 0)]
    (t/is (nil? (t 3 0 -3)))))

(t/deftest next-edit-test
  (let [t (fn [p x k] (#'miller/next-edit a1 b1 a1-graph 2 p x k))]
    (t/is (= (t 2 12 2)
           {:edit :insert :x 9 :p 2 :k 3 :d 5}))
    (t/is (= (t 2 9 3)
           {:edit :insert :x 8 :p 2 :k 4 :d 4}))
    (t/is (= (t 2 8 4)
           {:edit :delete :x 7 :p 1 :k 3 :d 3}))
    (t/is (= (t 1 7 3)
           {:edit :delete :x 6 :p 0 :k 2 :d 2}))
    (t/is (= (t 0 6 2)
           {:edit :delete :x 5 :p 0 :k 1 :d 1}))
    (t/is (= (t 0 5 1)
           {:edit :delete :x 2 :p 0 :k 0 :d 0})))
  (let [t (fn [p x k] (#'miller/next-edit a5 b5 a5-graph 0 p x k))]
    (t/is (= (t 2 6 0)
           {:edit :insert :x 6 :p 2 :k 1 :d 3}))
    (t/is (= (t 2 6 1)
           {:edit :insert :x 6 :p 2 :k 2 :d 2}))
    (t/is (= (t 2 6 2)
           {:edit :delete :x 4 :p 1 :k 1 :d 1}))
    (t/is (= (t 1 4 1)
           {:edit :delete :x 3 :p 0 :k 0 :d 0}))))

(def a1-edits [{:edit :delete :x 2 :p 0 :k 0 :d 0}
               {:edit :delete :x 5 :p 0 :k 1 :d 1}
               {:edit :delete :x 6 :p 0 :k 2 :d 2}
               {:edit :delete :x 7 :p 1 :k 3 :d 3}
               {:edit :insert :x 8 :p 2 :k 4 :d 4}
               {:edit :insert :x 9 :p 2 :k 3 :d 5}])

(def a2-edits [{:edit :insert :x 0 :p 0 :k  0 :d 0}
               {:edit :insert :x 0 :p 1 :k -1 :d 1}
               {:edit :insert :x 0 :p 2 :k -2 :d 2}
               {:edit :delete :x 0 :p 3 :k -3 :d 3}
               {:edit :delete :x 1 :p 3 :k -2 :d 4}
               {:edit :delete :x 2 :p 3 :k -1 :d 5}])

(def a3-edits [{:edit :delete :x 2 :p 0 :k 0 :d 0}
               {:edit :insert :x 4 :p 1 :k 1 :d 1}])

(def a4-edits [{:edit :insert :x 2 :p 0 :k  0 :d 0}
               {:edit :insert :x 2 :p 1 :k -1 :d 1}
               {:edit :insert :x 3 :p 2 :k -2 :d 2}
               {:edit :delete :x 3 :p 3 :k -3 :d 3}
               {:edit :delete :x 4 :p 3 :k -2 :d 4}
               {:edit :delete :x 5 :p 3 :k -1 :d 5}
               {:edit :insert :x 8 :p 3 :k  0 :d 6}
               {:edit :delete :x 8 :p 4 :k -1 :d 7}])

(t/deftest edits-test
  (let [t #'miller/edits]
    (t/is (= (t a1 b1 2 2 a1-graph) a1-edits))
    (t/is (= (t a2 b2 3 0 a2-graph) a2-edits))
    (t/is (= (t a3 b3 1 0 a3-graph) a3-edits))
    (t/is (= (t a4 b4 4 0 a4-graph) a4-edits))))

(t/deftest transpose-test
  (let [t #'miller/transpose]
    (t/is (= (t {:edit :delete :x 2 :p 0 :k 0 :d 0})
           {:edit :insert :x 2 :p 0 :k 0 :d 0 }))
    (t/is (= (t {:edit :insert :x 5 :p 0 :k 1 :d 1})
           {:edit :delete :x 4 :p 0 :k -1 :d 1 }))))

(t/deftest edits->script
  (let [t #'miller/edits->script]
    (t/is (= (t b1 a1-edits identity)
           {:+ [[7 \e] [8 \c]] :- [2 5 6 7]}))
    (t/is (= (t a1 a1-edits #'miller/transpose)
           {:+ [[1 \e] [3 \a \b \b]] :- [4 6]}))))

(t/deftest diff-test
  (let [t (fn [a b] (core/edit-distance (core/diff a b)))]
    (t/are [a b _ d] (= (t a b) d)
         "acebdabbabed"          "acbdeacbed"        :=> 6
         "acbdeacbed"            "acebdabbabed"      :=> 6
         "sobibm"                "sobmgc"            :=> 4
         [1 2 3 4 3 2 3 2 1 2 3] [2 3 1 2 3 4 5 4 3] :=> 10
         "abcab"                 "cbab"              :=> 3
         "abcabba"               "cbabac"            :=> 5
         "abc"                   "xyz"               :=> 6
         "IFvclxMax1"            "IF2qvWMa21"        :=> 8
         "Udghi4"                "Udhki4"            :=> 2)))

(t/deftest roundtrip
  (t/are [a b]
       (= b (core/patch a (core/diff a b)))

       "aba" "aca"
       "abcabba" "cbabac"
       "acebdabbabed" "acbdeacbed"
       "FWdRgevf43" "T5C7U3Lz5v"
       "s2dI8h9aK1FWdRgevf43" "5hs8L9T3K2T5C7U3Lz5v"
       "nBP8GaFHVls2dI8h9aK1FWdRgevf43" "925BCPcYhT5hs8L9T3K2T5C7U3Lz5v"
       "aba" "aca"
       "Udghi4" "Udhki4"
       "sobibm" "sobmgc"))
