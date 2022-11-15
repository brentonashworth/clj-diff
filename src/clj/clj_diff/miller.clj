(ns clj-diff.miller
  "Algorithm from 'An O(NP) Sequence Comparison Algorithm' by
   Sun Wu, Udi Manber, Gene Myers and Web Miller.

   Please refer to the above paper while reading this code."
  (:require [clj-diff [optimizations :as opt]]
            [clj-fast.core :as fast]))

(defn- next-x
  "Get the next farthest x value by looking at previous farthest values on the
  diagonal above and below diagonal k. Choose the greater of the farthest x on
  the above diagonal and the farthest x on the diagonal below plus one. fp is
  a map of diagonals => farthest points."
  [^long k fp]
  (max (inc (fast/val-at fp (dec k) -1))
       (fast/val-at fp (inc k) -1)))

(defn- snake
  "Starting at the farthest point on diagonal k, return the x value of the
  point at the end of the longest snake on this diagonal. A snake is a
  sequence of diagonal moves connecting match points on the edit graph."
  ([a b n m k fp]
   (snake a b n m k fp {}))
  ([a b n m k fp opts]
   {:pre [(and (vector? a) (vector? b))]}
   (let [compare (or (:compare opts)
                   (fn [a b]
                     (or
                       (and (string? a) (string? b)
                         (.equals ^String a ^String b))
                       (= a b))))
         x       (next-x k fp)
         y       (- x k)]
     (loop [x x
            y y]
       (if (and (< x n) (< y m) (compare (fast/val-at a (inc x)) (fast/val-at b (inc y))))
         (recur (inc x) (inc y))
         x)))))

(defn- p-band-diagonals
  "Given a p value and a delta, return all diagonals in this p-band."
  [p delta]
  (concat (range (* -1 p) delta)
          (reverse (range (inc delta) (+ delta (inc p))))
          [delta]))

(defn- search-p-band
  "Given a p value, search all diagonals in the p-band for the furthest
  reaching endpoints. Record the furthest reaching endpoint for each p value
  in the map fp. Returns an updated fp map for p. a and b are the two
  sequences and n and m are their lengths respectively. delta is the
  diagonal of the sink and is equal to n - m."
  ([a b n m delta p fp]
   (search-p-band a b n m delta p fp {}))
  ([a b n m delta p fp opts]
   (let [opts (or opts {})]
     (persistent! (reduce (fn [fp next-k]
                            (assoc! fp next-k (snake a b n m next-k fp opts)))
                    (transient fp)
                    (p-band-diagonals p delta))))))

(defn ses
  "Find the size of the shortest edit script (ses). Returns a 3-tuple of the
  size of the ses, the delta value (which is the diagonal of the sink)
  and the fp map. The optimal path from source to sink can be constructed from
  this information."
  ([a b]
   (ses a b {}))
  ([a b opts]
   {:pre [(>= (count a) (count b))]}
   (let [limit-p (:limit-p opts)
         n       (dec (count a))
         m       (dec (count b))
         delta   (- n m)]
     (loop [p  0
            fp {}]

       (cond
         (and (number? limit-p)
           (> p limit-p))
         #_=> [-1 delta fp]

         (= (-> (fast/val-at fp (dec p) {})
              (fast/val-at delta))
           n)
         #_=> [(dec p) delta fp]

         :else #_=> (recur (inc p)
                      (fast/fast-assoc fp p
                        (search-p-band a b n m delta p (fast/val-at fp (dec p) {}) opts))))))))

;;
;; Build the edit script from the map of farthest endpoints.
;;

(defn edit-dist
  "Given a delta, p and k value, calculate the edit distance."
  [delta p k]
  (if (> k delta)
    (+ (* 2 (- p (- k delta))) k)
    (+ (* 2 p) k)))

(defn- p-value-up
  "Calculate the p value that will be used to look up the farthest reaching
  end point for the diagonal above k."
  [delta p k]
  (if (> (inc k) delta) p (dec p)))

(defn- p-value-left
  "Calculate the p value that will be used to look up the farthest reaching
  end point for the diagonal below k."
  [delta p k]
  (if (< (dec k) delta) p (dec p)))

(defn- look-up
  "Get information about the vertex above the one at x on k. If this vertex
  is chosen, it will represent an insertion."
  [graph delta p x k]
  (when (> (- x k) 0)
    (let [up-k (inc k)
          up-p (p-value-up delta p k)
          x* (-> graph
                 (get up-p {})
                 (get up-k -1))]
      (when (and (>= x* 0) (= x x*))
        {:edit :insert
         :x x*
         :p up-p
         :k up-k
         :d (edit-dist delta up-p up-k)}))))

(defn- look-left
  "Get information about the vertex to the left of the one at x on k. If this
  vertex is chosen, it will represent an deletion."
  [graph delta p x k]
  (when (> x 0)
    (let [left-k (dec k)
          left-p (p-value-left delta p k)
          x* (-> graph
                 (get left-p {})
                 (get left-k -1))]
      (when (and (>= x* 0) (= (dec x) x*))
        {:edit :delete
         :x x*
         :p left-p
         :k left-k
         :d (edit-dist delta left-p left-k)}))))

(defn- backtrack-snake
  "Find the x value at the head of the longest snake ending at (x, y)."
  ([a b x y]
   (backtrack-snake a b x y {}))
  ([a b x y opts]
   {:pre [(and (>= x 0) (>= y 0))]}
   (let [opts    (or opts {})
         compare (or (:compare opts) =)]
     (loop [x x
            y y]
       (if (or (= x y 0) (not (compare (fast/val-at a x) (fast/val-at b y))))
         x
         (recur (dec x) (dec y)))))))

;; See the paper for an example of how there are multiple shortest
;; paths through an edit graph.

(defn- next-edit
  "Find the next move through the edit graph which will decrease the
  edit distance by 1."
  ([a b graph delta p x k]
   (next-edit a b graph delta p x k {}))
  ([a b graph delta p x k opts]
  {:post [(= (dec (edit-dist delta p k)) (:d %))]}
  (let [d (edit-dist delta p k)
         head-x (backtrack-snake a b x (- x k) opts)]
    (loop [head-x head-x]
      (let [move (first (filter #(and (not (= ::sentinel %)) ;; <<<===
                                      (= (:d %) (dec d)))
                                (map #(% graph delta p head-x k)
                                     [look-left look-up])))]
        (if (and (< head-x x) (nil? move))
          (recur (inc head-x))
           move))))))

(defn- edits
  "Calculate the sequence of edits from the map of the farthest reaching end
  points."
  ([a b p delta graph]
   (edits {} a b p delta graph))
  ([opts a b p delta graph]
   (let [next-fn (fn [p x k] (next-edit a b graph delta p x k opts))]
    (loop [edits '()
           prev {:x (count a) :p p :k delta
                 :d (edit-dist delta p delta)}]
      (if (= (:d prev) 0)
        edits
        (let [next (next-fn (:p prev) (:x prev) (:k prev))]
           (recur (conj edits next) next)))))))

(defn- transpose
  "If a is shorter than b, then the diff is calculated from b to a and this
  function is used to transpose the results into a diff from a to b."
  [edit]
  (-> edit
      (fast/fast-assoc :edit (if (= :insert (:edit edit)) :delete :insert))
      (fast/fast-assoc :x (- (:x edit) (:k edit)))
      (fast/fast-assoc :k (- (:k edit)))))

(defn- edits->script
  "Convert a sequence of edits into an edit script."
  [b edits f]
  (reduce (fn [script edit]
            (let [{:keys [edit x k]} (f edit)
                  y (inc (- x k))
                  insertions (:+ script)
                  last-insert (last insertions)]
              (if (= edit :delete)
                (fast/fast-assoc script :- (conj (:- script) x))
                (fast/fast-assoc script :+ (let [index (dec x)]
                                             (if (= index (first last-insert))
                                               (conj (vec (butlast insertions))
                                                     (conj last-insert (fast/val-at b y)))
                                               (conj insertions [(dec x) (fast/val-at b y)])))))))
          {:+ []
           :- []}
          edits))

(defn vectorize [& more]
  (map #(vec (cons ::sentinel %)) more))

(defn order->ses
  ([a b]
   (order->ses a b {}))
  ([a b opts]
   (let [[a* b*] (if (> (count b) (count a)) [b a] [a b])]
     [(ses a* b* opts) a* b*])))

(defn seq-diff
  ([a b]
   (seq-diff a b {}))
  ([a b opts]
   (let [[a b] (vectorize a b)
         [es a* b*] (order->ses a b opts)
         edits (apply edits opts a* b* es)]
     (edits->script b edits (if (= a* a) identity transpose)))))

(defn string-dispatch
  ([a b]
   (string-dispatch a b {}))
  ([a b _]
   (when (and (string? a) (string? b)) :string)))

(defmulti ^{:arglists '([a b])} diff
  "Create an edit script that may be used to transform a into b. See doc string
  for clj-diff.core/diff. This function will ensure that diff* is called with
  arguments a and b where a >= b. If the passed values of a and b need to be
  swapped then the resulting path with will transposed."
  string-dispatch)

(defmethod diff :default
  ([a b]
   (diff a b {}))
  ([a b opts]
   (seq-diff a b opts)))

(defmethod diff :string
  ([a b]
   (diff a b {}))
  ([a b opts]
   (opt/diff a b (fn [a b] (seq-diff a b opts)))))

(defn seq-edit-dist
  ([a b]
   (seq-edit-dist a b {}))
  ([a b opts]
   (let [[a b] (vectorize a b)
         [[p & more] a* b*] (order->ses a b opts)]
     (if (neg? p)
       p
       (+ (* 2 p) (- (count a*) (count b*)))))))

(defmulti edit-distance string-dispatch)

(defmethod edit-distance :default
  ([a b]
   (seq-edit-dist a b {}))
  ([a b opts]
    (seq-edit-dist a b opts)))

;; TODO - Modify optimizations so that it can be used here and with
;; longest-common-subseq
(defmethod edit-distance :string
  ([a b]
   (seq-edit-dist a b {}))
  ([a b opts]
    (seq-edit-dist a b opts)))

(defn seq-lcs
  ([a b]
   (seq-lcs a b {}))
  ([a b opts]
   (let [diff      (seq-diff a b opts)
         deletions (:- diff)]
     (filter #(not= % ::d)
       (reduce (fn [coll next]
                 (fast/fast-assoc coll next ::d))
         (vec (seq a))
         deletions)))))

(defmulti longest-common-subseq string-dispatch)

(defmethod longest-common-subseq :default
  ([a b]
   (longest-common-subseq a b {}))
  ([a b opts]
   (seq-lcs a b opts)))

(defmethod longest-common-subseq :string
  ([a b]
   (longest-common-subseq a b {}))
  ([a b opts]
   (apply str (seq-lcs a b opts))))
