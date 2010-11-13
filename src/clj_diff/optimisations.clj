(ns clj-diff.optimisations
  "Common optimisations for diff algorithms.")

;; Add unit tests for each of the boundary cases below. Make sure
;; both diff and patch work in each situation.

;; Use protocols to provide different implementations for strings and
;; sequences.

(defprotocol Common
  (common-prefix [a b])
  (common-suffix [a b]))

(extend-protocol Common

  clojure.lang.Sequential
  (common-prefix [a b]
                 (let [a (seq a)
                       b (seq b)
                       common (count (take-while true? (map #(= %1 %2) a b)))]
                   [common (drop common a) (drop common b)]))
  (common-suffix [a b]
                 (let [a (vec (seq a))
                       b (vec (seq b))
                       common (count (take-while true? (map #(= %1 %2)
                                                            (rseq a)
                                                            (rseq b))))]
                   [common
                    (take (- (count a) common) a)
                    (take (- (count b) common) b)]))

  java.lang.String
  (common-prefix [^String a ^String b]
                 (let [n (Math/min (.length a) (.length b))
                       i (loop [i 0]
                           (if (< i n)
                             (if (not= (.charAt a i) (.charAt b i))
                               i
                               (recur (inc i)))
                             n))]
                   [i (.substring a i) (.substring b i)]))
  (common-suffix [^String a ^String b]
                 (let [a-length (.length a)
                       b-length (.length b)
                       n (Math/min a-length b-length)
                       i (loop [i 1]
                           (if (<= i n)
                             (if (not= (.charAt a (- a-length i))
                                       (.charAt b (- b-length i)))
                               (dec i)
                               (recur (inc i)))
                             n))]
                   [i
                    (.substring a 0 (- a-length i))
                    (.substring b 0 (- b-length i))])))

(defn shortcut [a b]
  (cond (or (nil? a) (nil? b))
        (throw (IllegalArgumentException. "Cannot diff nil."))
        (= a b) {:+ [] :- []}
        (= (count a) 0) {:+ [(vec (concat [-1] (seq b)))]
                         :- []}
        (= (count b) 0) {:+ []
                         :- (vec (range 0 (count a)))}
        :else nil))

(defn- diff* [a b f]
  (or (cond (= (count a) 0) {:+ [(vec (concat [-1] (seq b)))]
                             :- []}
            (= (count b) 0) {:+ []
                             :- (vec (range 0 (count a)))}
            :else nil)
      (f a b)))

(defn diff
  "Wrap the diff function f in pre and post optimisations."
  [a b f]
  (let [diffs (cond (or (nil? a) (nil? b))
                    (throw (IllegalArgumentException. "Cannot diff nil."))
                    (= a b) {:+ [] :- []}
                    :else nil)]
    (or diffs
        (let [[prefix a b] (common-prefix a b)
              [suffix a b] (common-suffix a b)
              diffs (diff* a b f)]
          (if (> prefix 0)
            {:+ (vec (map #(apply vector
                                  (+ prefix (first %)) (rest %)) (:+ diffs)))
             :- (vec (map #(+ prefix %) (:- diffs)))}
            diffs)))))
