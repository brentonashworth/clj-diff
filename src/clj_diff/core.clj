(ns clj-diff.core
  "Diff, patch and related functions for Clojure sequences."
  (:require [clj-diff [miller :as miller]]))

(defn diff
  "Create the edit script for transforming sequance a into sequence b.
  An edit script is a map with keys :+ and :- for additions and deletions.
  Additions are represented as a sequence of vectors. The first item in each
  vector is the index where the rest of the items in the vector are to be
  inserted. For example [3 b c] means to insert b an c after whatever is
  in index 3. Deletions are represented as a sequence of indexes to delete.

  For example: the diff of 'abcabba' and 'cbabac' would generate the edit
  script below.

  {:+ [[2 b] [6 c]], :- [0 1 5]}

  An index of -1 may appear in additions and is a special case which means to
  add the elements at the beginning of the sequence."
  [a b]
  (miller/diff a b))

(defn patch*
  [s edit-script]
  (let [s (vec s)
        additions (:+ edit-script)
        deletions (:- edit-script)
        s (reduce (fn [a b]
                    (assoc a b nil))
                  s
                  deletions)
        s (reduce (fn [a b]
                    (let [index (first b)]
                      (if (= index -1)
                        (assoc a 0 (conj (vec (rest b)) (get a 0)))
                        (assoc a index (conj (rest b) (get a index))))))
                  s
                  additions)]
    (filter #(not (nil? %)) (flatten s))))

(defmulti ^{:arglists '([s edit-script])} patch
  "Use the instructions in the edit script to transform the sequence s into
  a new sequence. If the edit script was created by using diff on a and b then
  patch will use the edit script to transform a into b.

  (diff a b) -> x, (patch a x) -> b."
  (fn [s _] (class s)))

(defmethod patch :default
  [s edit-script]
  (patch* s edit-script))

(defmethod patch String
  [s edit-script]
  (apply str (patch* s edit-script)))

(defn edit-distance
  "Calculate the edit distance for the given edit script. The edit distance
  is the minimum number of insertions and deletions required to transform one
  sequence into another."
  [diff]
  (+ (count (:- diff))
     (reduce + (map #(count (drop 1 %)) (:+ diff)))))
