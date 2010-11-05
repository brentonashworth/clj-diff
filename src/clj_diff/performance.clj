(ns clj-diff.performance
  "Measuring performance of different diff algorithms."
  (:use [incanter core charts datasets])
  (:require [clj-diff [core :as core]]
            [clj-diff [myers :as myers]]
            [incanter [stats :as stats]])
  (:import name.fraser.neil.plaintext.diff_match_patch))

(defn random-between [lo hi]
  (let [r (java.util.Random.)
        lo (if (char? lo) (int lo) lo)
        hi (if (char? hi) (int hi) hi)
        n (+ 1 (- hi lo))]
    (+ lo (Math/abs (mod (. r nextInt) n)))))

(defn random-string [size]
  (loop [length (random-between size size)
         v []]
    (if (> length 0)
      (let [j (random-between 1 3)]
        (recur (dec length)
               (conj v
                     (cond (= j 1) (char (random-between \a \z))
                           (= j 2) (char (random-between \A \Z))
                           (= j 3) (char (random-between \1 \9))))))
      (apply str v))))

(defn myers-diff [a b]
  (let [edit-script (myers/diff a b)
        patched (apply str (core/patch a edit-script))]
    (or (= b patched)
        (do
          (println "a:" (str a))
          (println "b:" (str b))
          (println "edit-script:" (str edit-script))
          (println "patched:" (str patched))
          false))))

(defn fraser-diff [a b]
  (let [dmp (diff_match_patch.)
        _ (set! (. dmp Diff_Timeout) 0)
        diffs (.diff_main dmp a b)
        patches (.patch_make dmp diffs)
        patched (first (seq (.patch_apply dmp patches a)))]
    (= b patched)))

(def diff-fns [["Myers Unrefined" myers-diff]
               ["Fraser" fraser-diff]])

(defn time*
  ([expr a b]
     (let [a (a)
           b (b a)
           start (. System (nanoTime))
           ret (expr a b)
           stop (. System (nanoTime))]
       (/ (double (- stop start)) 1000000.0)))
  ([n expr a b]
     (map (fn [_] (time* expr a b)) (range 0 n))))

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

(defn sample
  "n is the size of the initial string. m is the number of mutations and g is
  the mutation group size."
  [n mutate r t]
  (let [a #(random-string n)
        b #(mutate %)]
    (map #(let [[alg f] %
                d (take t (sort (time* r f a b)))
                mean (stats/mean d)
                sd (stats/sd d)]
            (println "size:" n ", alg: " alg ", mean:" mean ", sd:" sd) 
            {:name alg :mean mean :sd sd})
         diff-fns)))

(defn perf-run [n m-range g]
  (flatten
   (map (fn [m] (map #(merge {:mutations m} %)
                     (sample n #(mutate % m g) 20 30)))
        m-range)))

(defn perf-run-2
  ([n-range m]
     (perf-run-2 n-range m 2 3))
  ([n-range m t r]
     (flatten
      (map (fn [n] (map #(merge {:size n} %)
                        (sample n m t r)))
           n-range))))

(defn visualize [title file-name data]
  (let [d (to-dataset data)]
    (with-data d
      (doto (line-chart :mutations :mean
                        :group-by :name
                        :legend true
                        :title (str "Sequence length = " title)
                        :x-label (str "Mutations")
                        :y-label "Time (ms)")
        view
        (save (str "charts/" file-name ".png"))))))

(defn visualize-2 [title file-name data]
  (let [d (to-dataset data)]
    (with-data d
      (doto (line-chart :size :mean
                        :group-by :name
                        :legend true
                        :title title
                        :x-label "Sequence Length"
                        :y-label "Time (ms)")
        view
        (save (str "charts/" file-name ".png"))))))

(defn test-range [size points]
  (let [mutations (/ size 2.0)
        step (/ mutations points)]
    (range 1 (inc mutations) step)))

(defn move-first-to-end [a]
  (let [s (seq a)
        f (first s)
        s (drop 1 s)]
    (apply str (concat (vec s) [f]))))

(defn add-in-the-middle [a]
  (let [split (map #(apply str %) (split-at (/ (count a) 2) (seq a)))]
    (str (first split) "clj-diff" (last split))))

(defn suite [x]
  (let [d1 (perf-run 100 (test-range 100 x) 5)
        d2 (perf-run 1000 (test-range 1000 x) 50)
        d3 (perf-run-2 (range 100 20000 2000) #(mutate % (* (count %) 0.05) 10))
        d4 (perf-run-2 (range 100 10000 1000) #(mutate % (* (count %) 0.10) 10))
        d5 (perf-run-2 (range 100 3000 500) #(mutate % (* (count %) 0.5) 10))
        d6 (perf-run-2 (range 100 10000 1000) move-first-to-end 10 15)
        d7 (perf-run-2 (range 100 10000 1000) add-in-the-middle 10 15)]
    (visualize 100 "mutations_100" d1)
    (visualize 1000 "mutations_1000" d2)
    (visualize-2 "5% change" "length_5" d3)
    (visualize-2 "10% change" "length_10" d4)
    (visualize-2 "50% change" "length_50" d5)
    (visualize-2 "Move First Element to End" "length_move_first_to_end" d6)
    (visualize-2 "Add in the Middle" "length_add_in_middle" d7)))

(defn performance-tests []
  (suite 10))
