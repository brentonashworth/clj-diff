(ns clj-diff.performance
  "Measuring performance of different diff algorithms."
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

(defn random-string [lo hi]
  (loop [length (random-between lo hi)
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

(def diff-fns [["Myers" myers-diff]
               ["Fraser" fraser-diff]])

(defmacro time*
  ([expr]
     `(let [start# (. System (nanoTime))
            ret# ~expr
            stop# (. System (nanoTime))]
        (/ (double (- stop# start#)) 1000000.0)))
  ([n expr]
     `(map (fn [~'x] (time* ~expr)) (range 0 ~n))))

(defn mutate
  "Make n random mutations to the string s."
  [s n]
  (let [size (count s)
        s (vec s)
        indecies (take n (shuffle (range size)))]
    (apply str
           (reduce (fn [a b]
                     (assoc a b (char (random-between \a \z))))
                   s
                   indecies))))

(defn point-test [size mutations]
  (let [a (random-string size size)
        b (mutate a mutations)]
    (map #(let [[n f] %
                d (take 15 (sort (time* 20 (f a b))))
                mean (stats/mean d)
                med (stats/median d)
                sd (stats/sd d)]
            [n mean med sd])
         diff-fns)))
