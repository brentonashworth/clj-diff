(ns clj-diff.performance
  "Measuring performance of different diff algorithms."
  (:use [incanter core charts datasets])
  (:require [clj-diff [core :as core]]
            [clj-diff [myers :as myers]]
            [clj-diff [miller :as miller]]
            [incanter [stats :as stats]])
  (:import name.fraser.neil.plaintext.diff_match_patch))

;;
;; Diff functions for performance tests.
;;
;; Each function will compute a diff and return it.
;;

(defn myers-diff [a b]
  (myers/diff a b))

(defn miller-diff [a b]
  (miller/diff a b))

(defn fraser-diff [a b]
  (let [dmp (diff_match_patch.)]
    (do (set! (. dmp Diff_Timeout) 0)
        (.diff_main dmp a b))))

(defn fraser-distance [diffs]
  (let [diffs (map #(vector (.toString (.operation %)) (.text %)) (seq diffs))]
    (reduce + (map #(case (first %)
                          "EQUAL" 0
                          (count (last %)))
                   diffs))))

(defprotocol EditDistance
  (edit-distance [diffs]))

(extend-protocol EditDistance
  
  clojure.lang.IPersistentMap
  
  (edit-distance [diffs]
                 (core/edit-distance diffs))

  java.util.LinkedList

  (edit-distance [diffs]
                 (fraser-distance diffs)))

(def diff-fns [["Myers" myers-diff]
               ["Miller" miller-diff]
               ["Fraser" fraser-diff]])

(defn random-between
  "Generate a random number between low and high. Can also be passed
  characters as the bounds."
  [lo hi]
  (let [r (java.util.Random.)
        lo (if (char? lo) (int lo) lo)
        hi (if (char? hi) (int hi) hi)
        n (+ 1 (- hi lo))]
    (+ lo (Math/abs (mod (. r nextInt) n)))))

(defn random-string
  "Generage a random string composed of upper and lower case letters and the
  numbers 0 through 9."
  [size]
  (loop [length (random-between size size)
         v []]
    (if (> length 0)
      (let [j (random-between 1 3)]
        (recur (dec length)
               (conj v
                     (cond (= j 1) (char (random-between \a \z))
                           (= j 2) (char (random-between \A \Z))
                           (= j 3) (char (random-between \0 \9))))))
      (apply str v))))

(defn mutate
  "Make n random mutations to the string s. Mutations will be randomly grouped
  into runs of 1 to g."
  [s n g]
  (let [size (count s)
        s (vec s)
        group-size (random-between 1 (min n g))
        additions (partition group-size
                             (take n
                                   (repeatedly #(char (random-between \a \z)))))
        indecies (take (count additions) (shuffle (range size)))
        indecies (map #(let [c (count %2)]
                         (range %1 (+ c %1)))
                      indecies
                      additions)]
    (apply str
           (flatten
            (reduce (fn [a b]
                      (assoc a (first b) (last b)))
                    s
                    (map #(vector %1 %2)
                         (flatten indecies)
                         (flatten additions)))))))

(defn time*
  "Calculate the time, in milliseconds, to run the passed expression. Returns
  a sequence of maps containing the times and return values of each run."
  ([expr]
     (time* 1 expr identity))
  ([n expr]
     (time* n expr identity))
  ([n expr f]
     (map (fn [_] (let [start (. System (nanoTime))
                        ret (expr)
                        stop (. System (nanoTime))]
                    {:time (/ (double (- stop start)) 1000000.0)
                     :result (f ret)}))
          (range 0 n))))

(defn sample
  "For strings a and b, run each diff algorithm 'total-runs' times and then
  calculate stats based on the fastest 'take-top' runs."
  [a b take-top total-runs]
  (map #(let [[alg f] %
              d (take take-top
                      (sort-by :time (time* total-runs
                                            (fn [] (f a b))
                                            edit-distance)))
              times (map :time d)
              distances (distinct (map :result d))
              mean (stats/mean times)
              sd (stats/sd times)]
          {:name alg :mean mean :sd sd
           :distance (apply str (interpose ", " distances))})
       diff-fns))

(defn vary-mutations
  "For a sting on length n, vary the number of mutations that are made to
  the string."
  ([n m-range g]
     (vary-mutations n m-range g 2 3))
  ([n m-range g t r]
     (let [a (random-string n)]
       (flatten
        (map (fn [m] (map #(merge {:mutations m} %)
                          (sample a (mutate a m g) (max t 1) (max r 1))))
             m-range)))))

(defn vary-string-length
  ([n-range m]
     (vary-string-length n-range m 2 3))
  ([n-range m t r]
     (flatten
      (map (fn [n] (map #(merge {:size n} %)
                        (let [a (random-string n)]
                          (sample a (m a) (max t 1) (max r 1)))))
           n-range))))

(defn visualize [title file-name data]
  (let [d (to-dataset (doall data))]
    (view d)
    (with-data d
      (doto (line-chart :mutations :mean
                        :group-by :name
                        :legend true
                        :title (str "Sequence length = " title)
                        :x-label (str "Mutations")
                        :y-label "Time (ms)")
        (view :width 700)
        (save (str "charts/" file-name ".png") :width 700)))))

(defn visualize-2 [title file-name data]
  (let [d (to-dataset (doall data))]
    (view d)
    (with-data d
      (doto (line-chart :size :mean
                        :group-by :name
                        :legend true
                        :title title
                        :x-label "Sequence Length"
                        :y-label "Time (ms)")
        
        (view :width 700)
        (save (str "charts/" file-name ".png") :width 700)))))

(defn test-range [size points]
  (let [mutations (quot size 2)
        step (quot mutations points)]
    (range 1 (inc mutations) step)))

(defn- move-first-to-end* [a]
  (let [s (seq a)
        f (first s)
        s (drop 1 s)]
    (apply str (concat (vec s) [f]))))

(defn- add-in-the-middle* [a]
  (let [split (map #(apply str %) (split-at (/ (count a) 2) (seq a)))]
    (str (first split) "clj-diff" (last split))))

(defn vary-mutation-100 [x n]
  (let [d (vary-mutations 100 (test-range 100 x)
                          5
                          (quot (* n 2) 3)
                          n)]
    (visualize 100 "mutations_100" d)))

(defn vary-mutation-1000 [x n]
  (let [d (vary-mutations 1000 (test-range 1000 x)
                          50
                          (quot (* n 2) 3) n)]
    (visualize 1000 "mutations_1000" d)))

(defn move-first-to-end [x n]
  (let [d (vary-string-length (range 100 10000 (quot 10000 x))
                              move-first-to-end*
                              (quot (* n 2) 3)
                              n)]
    (visualize-2 "Move First Element to End" "length_move_first_to_end" d)))

(defn add-in-the-middle [x n]
  (let [d (vary-string-length (range 100 10000 (quot 10000 x))
                              add-in-the-middle*
                              (quot (* n 2) 3)
                              n)]
    (visualize-2 "Add in the Middle" "length_add_in_middle" d)))

(defn percent-change [max p x n]
  (let [percent (/ p 100.0)
        d (vary-string-length (range 100 max (quot max x))
                              #(mutate % (* (count %) percent) 10)
                              (quot (* n 2) 3)
                              n)]
    (visualize-2 (str p "% change") (str "length_" max "_" p) d)))

(defn suite [x]
  (do
    (vary-mutation-100 x 50)
    (vary-mutation-1000 x 10)
    (percent-change 15000 5 x 3)
    (percent-change 7000 5 x 10)
    (percent-change 5000 10 x 3)
    (percent-change 2000 50 x 3)
    (move-first-to-end x 50)
    (add-in-the-middle x 50)))

(defn performance-tests []
  (suite 10))
