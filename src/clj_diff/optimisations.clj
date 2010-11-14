(ns clj-diff.optimisations
  "Common optimisations for diff algorithms.")

(defprotocol Sequence
  (common-prefix [a b] "Find a common prefix of two sequences")
  (common-suffix [a b] "Find a common suffix of two sequences")
  (index-of [a b] [a b start]
            "Return the index of the first occurance of b in a")
  (subsequence [s start] [s start end]
               "Returns a subsequence of the items in the sequence from
   start (inclusive) to end (exclusive).  If end is not supplied, defaults
   to (count sequence).")
  (concatinate [a b] "Concatinate two sequences."))

(extend-protocol Sequence

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
  (index-of ([a b]
               (index-of a b 0))
            ([a b start]
               (let [b (seq b)
                     size (count b)]
                 (loop [a (drop start (seq a))
                        target b
                        i start]
                   (cond (= (count target) 0)
                         (- i size)
                         (> (count a) 0)
                         (if (= (first target) (first a))
                           (recur (rest a) (rest target) (inc i))
                           (recur (rest a) b (inc i)))
                         :else -1)))))
  (subsequence ([a start]
                  (subvec (vec a) start))
               ([a start end]
                  (subvec (vec a) start end)))
  (concatinate [a b]
               (concat a b))

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
                    (.substring b 0 (- b-length i))]))
  (index-of ([^String a ^String b]
               (.indexOf a b))
            ([^String a ^String b start]
               (.indexOf a b start)))
  (subsequence ([a start]
                  (.substring a start))
               ([a start end]
                  (.substring a start end)))
  (concatinate [a b]
               (str a b)))

(defn- short-within-long
  "Return a diff if the shorter sequence exists in the longer one. No need to
  use the expensive diff algorithm for this."
  [a b ca cb]
  (let [[short long] (if (> ca cb) [b a] [a b])
        i (index-of long short)]
    (if (= i -1)
      nil
      (if (= short a)
        {:+ (filter #(not (nil? %))
                    [(when (> i 0)
                       (vec (concat [-1] (seq (subsequence b 0 i)))))
                     (when (< (+ i ca) cb)
                       (vec (concat [(dec (+ i ca))]
                                    (seq (subsequence b (+ i ca))))))])
         :- []}
        {:+ []
         :- (vec (concat (range 0 i)
                         (range (+ i cb) ca)))}))))

(defn- half-match* [long short i]
  (let [target (subsequence long i (+ i (/ (count long) 4)))]
    (loop [j (index-of short target 0)
           result []]
      (if (= j -1)
        (if (>= (count (or (first result) ""))
                (quot (count long) 2))
          result
          nil)
        (let [prefix-length (first (common-prefix (subsequence long i)
                                                  (subsequence short j)))
              suffix-length (first (common-suffix (subsequence long 0 i)
                                                  (subsequence short 0 j)))
              common (or (first result) "")]
          (recur (index-of short target (inc j))
                 (if (< (count common) (+ prefix-length suffix-length))
                   [(concatinate (subsequence short (- j suffix-length) j)
                                 (subsequence short j (+ j prefix-length)))
                    (subsequence long 0 (- i suffix-length))
                    (subsequence long (+ i prefix-length))
                    (subsequence short 0 (- j suffix-length))
                    (subsequence short (+ j prefix-length))]
                   result)))))))

(defn- half-match
  "Find a subsequence shared by both sequences which is at least half as long
  as the longer sequence. Return a vector of five elements is one is found and
  and nil if not. The five elements are: the common sequence, the prefix
  of sequence a, the suffix of sequence a, the prefix of sequence b and the
  suffix of sequence b."
  [a b]
  (let [[short long] (if (> (count a) (count b)) [b a] [a b])
        short-count (count short)
        long-count (count long)]
    (if (or (< long-count 4)
            (< (* short-count 2) long-count))
      nil
      (let [hm-second-q (half-match* long short (/ (+ long-count 3) 4))
            hm-third-q (half-match* long short (/ (+ long-count 1) 2))
            half-match (cond (and hm-second-q hm-third-q)
                             (if (> (count (first hm-second-q))
                                    (count (first hm-third-q)))
                               hm-second-q
                               hm-third-q)
                             :else (or hm-second-q hm-third-q))]
        (cond (nil? half-match) nil
              (= a long) half-match
              :else [(get half-match 0)
                     (get half-match 3)
                     (get half-match 4)
                     (get half-match 1)
                     (get half-match 2)])))))

(defn- offset-diffs [diffs offset]
  {:+ (vec (map #(apply vector
                        (+ offset (first %)) (rest %)) (:+ diffs)))
   :- (vec (map #(+ offset %) (:- diffs)))})

(declare diff)

(defn- diff*
  "Calculate the diff using the function f only after ensuring that this
  algorithm is required. At this point we know that a and b are different at
  either the beginning, the end, or both. A diff can be calculated manually if
  the length of a or b is 0 or if the smaller of the two sequences is contained
  within the larger."
  [a b f]
  (let [ca (count a)
        cb (count b)]
    (or (cond (= ca 0) {:+ [(vec (concat [-1] (seq b)))]
                        :- []}
              (= cb 0) {:+ []
                        :- (vec (range 0 ca))}
              :else (if-let [diffs (short-within-long a b ca cb)]
                      diffs
                      (if-let [half-match (half-match a b)]
                        (let [common (get half-match 0)
                              a-prefix (get half-match 1)
                              a-suffix (get half-match 2)
                              b-prefix (get half-match 3)
                              b-suffix (get half-match 4)
                              diff-a (diff a-prefix b-prefix f)
                              diff-b (diff a-suffix b-suffix f)]
                          (merge-with concat
                                      diff-a
                                      (offset-diffs diff-b
                                                    (+ (count common)
                                                       (count a-prefix)))))
                        nil)))
        (f a b))))

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
