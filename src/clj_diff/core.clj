(ns clj-diff.core
  "Diff and patch for Clojure sequences."
  (:require [clj-diff [myers :as myers]]))

(defn diff
  "Create the edit script that may be used to transform the sequance a into b.
  An edit script is a map with the keys :+ and :- for additions and deletions.
  Additions are represented as a sequence of vectors. The first item in each
  vector is the index where the rest of the items in the vector are to be
  inserted. For example [3 b c] means to insert b an c after whatever is
  in index 3. Deletions are represented as a sequence of indexes to delete.

  For example: the diff of 'abcabba' and 'cbabac' whould generate the edit
  script below.

  {:+ [[2 b] [6 c]], :- [0 1 5]}"
  [a b]
  (myers/diff a b))

(defn patch
  "Use the instructions in the edit script to transform the sequence s into
  a new sequence. If the edit script was created by using diff on a and b then
  patch will use the edit script to transform a into b.

  (diff a b) -> x, (patch a x) -> b."
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
                      (assoc a index (conj (rest b) (get a index)))))
                  s
                  additions)]
    (flatten (filter #(not (nil? %)) s))))

