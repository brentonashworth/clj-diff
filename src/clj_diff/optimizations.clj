(ns clj-diff.optimizations
  "String optimizations for diff algorithms.
  See http://neil.fraser.name/writing/diff/."
  (:import clj_diff.FastStringOps))

(defn common-prefix [^String a ^String b]
  (let [i (FastStringOps/commonPrefix a b)]
    [i (.substring a i) (.substring b i)]))

(defn common-suffix [^String a ^String b]
  (let [i (FastStringOps/commonSuffix a b)]
    [i
     (.substring a 0 (- (.length a) i))
     (.substring b 0 (- (.length b) i))]))

(defn- short-within-long
  "Return a diff if the shorter sequence exists in the longer one. No need to
  use the expensive diff algorithm for this."
  [^String a ^String b ^Integer ca ^Integer cb]
  (let [[short long] (if (> ca cb) [b a] [a b])
        i (int (.indexOf long short))]
    (if (= i -1)
      nil
      (if (= short a)
        {:+ (filter #(not (nil? %))
                    [(when (> i 0)
                       (vec (concat [-1] (seq (.substring b 0 i)))))
                     (when (< (+ i ca) cb)
                       (vec (concat [(dec (+ i ca))]
                                    (seq (.substring b (+ i ca))))))])
         :- []}
        {:+ []
         :- (vec (concat (range 0 i)
                         (range (+ i cb) ca)))}))))

(defn- half-match* [^String long ^String short ^Integer i]
  (let [target (.substring long i (+ i (quot (count long) 4)))]
    (loop [j (.indexOf short target 0)
           result []]
      (if (= j -1)
        (if (>= (count (or (first result) ""))
                (quot (count long) 2))
          result
          nil)
        (let [prefix-length (first (common-prefix (.substring long i)
                                                  (.substring short j)))
              suffix-length (first (common-suffix (.substring long 0 i)
                                                  (.substring short 0 j)))
              common (or (first result) "")]
          (recur (.indexOf short target (inc j))
                 (if (< (count common) (+ prefix-length suffix-length))
                   [(str (.substring short (- j suffix-length) j)
                         (.substring short j (+ j prefix-length)))
                    (.substring long 0 (- i suffix-length))
                    (.substring long (+ i prefix-length))
                    (.substring short 0 (- j suffix-length))
                    (.substring short (+ j prefix-length))]
                   result)))))))

(defn- half-match
  "Find a substring shared by both sequences which is at least half as long
  as the longer sequence. Return a vector of five elements if one is found and
  and nil if not. The five elements are: the common sequence, the prefix
  of sequence a, the suffix of sequence a, the prefix of sequence b and the
  suffix of sequence b."
  [^String a ^String b]
  (let [[short long] (if (> (count a) (count b)) [b a] [a b])
        short-count (count short)
        long-count (count long)]
    (if (or (< long-count 4)
            (< (* short-count 2) long-count))
      nil
      (let [hm-second-q (half-match* long short (quot (+ long-count 3) 4))
            hm-third-q (half-match* long short (quot (+ long-count 1) 2))
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
  both ends. A diff can be calculated manually if the length of a or b is 0
  or if the smaller of the two sequences is contained within the longer."
  [^String a ^String b f]
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
  "Return the diff of a and b. Wrap the diff function f in pre and post
  optimizations. Check for nil and equality. Remove common prefix and suffix."
  [^String a ^String b f]
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
