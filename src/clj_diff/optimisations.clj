(ns clj-diff.optimisations
  "Common optimisations for diff algorithms.")

;; Add unit tests for each of the boundary cases below. Make sure
;; both diff and patch work in each situation.

;; Use protocols to provide different implementations for strings and
;; sequences.

(defn common-prefix [a b]
  (let [a (seq a)
        b (seq b)]
    (let [common (count (take-while true? (map #(= %1 %2) a b)))
          a (drop common a)
          b (drop common b)]
      [common a b])))

(defn common-suffix [a b]
  (let [a (vec (seq a))
        b (vec (seq b))]
    (let [common (count (take-while true? (map #(= %1 %2)
                                               (rseq a)
                                               (rseq b))))
          a (take (inc (- (count a) common)) a)
          b (take (inc (- (count b) common)) b)]
      [common a b])))

(defn shortcut [a b]
  (cond (or (nil? a) (nil? b))
        (throw (IllegalArgumentException. "Cannot diff nil."))
        (= a b) {:+ [] :- []}
        (= (count a) 0) {:+ [(vec (concat [-1] (seq b)))]
                         :- []}
        (= (count b) 0) {:+ []
                         :- (vec (range 0 (count a)))}
        :else nil))

(defn diff
  "Wrap the diff function f in pre and post optimisations."
  [a b f]
  (or (shortcut a b)
      (let [[prefix a b] (common-prefix a b)
            [suffix a b] (common-suffix a b)
            diffs (f a b)]
        (if (> prefix 0)
          {:+ (vec (map #(apply vector
                                (+ prefix (first %)) (rest %)) (:+ diffs)))
           :- (vec (map #(+ prefix %) (:- diffs)))}
          diffs))))
