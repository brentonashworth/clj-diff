(ns clj-diff.core
  "Diff, patch and related functions for Clojure sequences."
  (:require [clj-diff [miller :as miller]]
            [clojure.core.rrb-vector :as fv]))

(defn diff
  "Create the edit script for transforming sequence a into sequence b.
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
  [a b & [opts]]
  (miller/diff a b opts))

(defn- merge-patch
  [s edit-script delete-symbol]
  (let [s         (fv/vec s)
        additions (:+ edit-script)
        deletions (:- edit-script)
        s         (reduce (fn [a b]
                            (assoc a b {::delete (get a b)}))
                    s
                    deletions)

        s         (reduce (fn [a b]
                            (let [index (first b)
                                  items (fv/subvec b 1)]
                              (if (= index -1)
                                (assoc a 0 (fv/vector {::insert items} (get a 0)))
                                (assoc a index (fv/vector (get a index) {::insert items})))))
                    s
                    additions)]
    (mapcat
      #(if (vector? %)
         %
         [%])
      s)))

(defn patch*
  [s edit-script]
  (filter #(not (= ::delete-sentinel %)) (merge-patch s edit-script ::delete-sentinel)))

(defmulti ^{:arglists '([s edit-script])} patch
  "Use the instructions in the edit script to transform the sequence s into
  a new sequence. If the edit script was created by using diff on a and b then
  patch will use the edit script to transform a into b.

  (diff a b) -> x, (patch a x) -> b."
  (fn [s _] (class s)))

(defmethod patch :default
  [s edit-script]
  (into []
    (mapcat
      (fn [item]
        (cond
          (and (map? item) (contains? item :clj-diff.core/delete)) nil
          (and (map? item) (contains? item :clj-diff.core/insert)) (:clj-diff.core/insert item)
          :else (list item))))
    (patch* s edit-script)))

(defmethod patch String
  [s edit-script]
  (apply str
    (mapcat
      (fn [item]
        (cond
          (and (map? item) (contains? item :clj-diff.core/delete)) nil
          (and (map? item) (contains? item :clj-diff.core/insert)) (:clj-diff.core/insert item)
          :else (list item)))
      (patch* s edit-script))))

(defn edit-distance
  "Returns the edit distance between the two passed sequences. May also be
  passed an edit script. The edit distance is the minimum number of insertions
  and deletions required to transform one sequence into another."
  ([a b & [opts]]
   (miller/edit-distance a b opts))
  ([edit-script]
     (+ (count (:- edit-script))
        (reduce + (map #(count (drop 1 %)) (:+ edit-script))))))

(defn- max-or-zero [coll]
  (if (and (coll? coll)
           (not (empty? coll)))
    (apply max coll)
    0))

(defn levenshtein-distance
  "Returns the Levenshtein distance between two sequences. May either be passed
  the two sequences or a diff of the two sequences.

  From [Wikipedia](http://en.wikipedia.org/wiki/Levenshtein_distance):
  The Levenshtein distance between two strings is the minimum number of edits
  needed to transform one string into the other, with the allowable edit
  operations being insertion, deletion and substitution of a single character.

  This function works not only with strings but with any Clojure sequence.

  Warning! Technically this function is estimating the Levenshtein distance
  from a computed diff. Most of the time, it is the same as the real Levenshtein
  distance but in same cases it may be larger. The reason for this is that
  there may be multiple paths through an edit graph with the same edit
  distance but with differing Levenshtein distance. A future improvement to
  the diff algorithm would be to find all paths and prefer the one with the
  minimum Levenshtein distance."
  ([a b]
   (levenshtein-distance (diff a b)))
  ([edit-script]
   (let [additions   (map #(let [index (first %)
                                 items (rest %)]
                             (apply vector index (repeat (count items) :a)))
                       (:+ edit-script))
         max-index   (max (max-or-zero (map first additions))
                       (max-or-zero (:- edit-script)))
         v           (vec (repeat max-index :e))
         patched     (merge-patch v (merge edit-script {:+ additions}) :d)
         edit-groups (filter #(not= :e (first %))
                       (partition-by #(if (= % :e) :e :edit)
                         patched))]
     (reduce + (map (fn [group]
                      (max
                        (transduce
                          (comp
                            (mapcat :clj-diff.core/insert)
                            (filter #{:a})
                            (map (constantly 1)))
                          +
                          group)
                        (transduce
                          (comp
                            (filter #(contains? % :clj-diff.core/delete))
                            (map (constantly 1)))
                          +
                          group)))
                 edit-groups)))))

(defn longest-common-subseq [a b & [opts]]
  (miller/longest-common-subseq a b opts))
