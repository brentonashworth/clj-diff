(ns clj-diff.myers
  "Algorithm from 'An O(ND) Difference Algorithm and Its Variations' by
   Eugene W. Myers.

   Please refer to the above paper while reading this code.

   For two sequences a and b, a is on the x axis and b is on the y axis of
   the edit graph. n and m are the lengths of a and b respectively. Diagonal
   k is the diagonal where k = x - y for all points (x, y) on the diagonal."
  (:require [clj-diff [optimizations :as opt]]))

(defn- x-down
  "Get the x value of the furthest reaching d-path on the diagonal below k."
  [v k]
  (get v (dec k) -1))

(defn- x-up
  "Get the x value of the furthest reaching d-path on the diagonal above k."
  [v k]
  (get v (inc k) -1))

(defn- next-point
  "Find the x and y value of the point on diagonal k where we will continue
   our search. To get to diagonal k we will either make a vertical move down
   from the diagonal above or a horizontal move to the right from the diagonal
   below. If the current diagonal is not the top diagonal in the current band
   and the x value of the furthest reaching d-path on the diagonal above is
   greater than the one for the diagonal below, we will move down from above
   diagonal. Otherwise, we move to the right from the lower diagonal. When k is
   the top diagonal in the d-band we always move horizontally from the lower
   diagonal. d is the size of the edit script and v is a map from k -> x of
   the furthest reaching d-paths."
  [d k v]
  (let [up (x-up v k)
        down (x-down v k)
        x (if (and (< k d) (< down up))
            up
            (inc down))
        y (- x k)]
    [x y]))

(defn snake
  "Starting at point (x, y) return the x value of the point at the end of the
  longest snake on this diagonal. A snake is a sequence of diagonal moves
  connecting match points on the edit graph."
  [a b x y n m]
  {:pre [(and (vector? a) (vector? b))]}
  (loop [x x
         y y]
    (if (and (< x n) (< y m) (= (get a (inc x)) (get b (inc y))))
      (recur (inc x) (inc y))
      x)))

(defn- band-diagonals
  "Get all diagonals for d-band d."
  [d]
  (range (* d -1) (inc d) 2))

(defn- search-d-band
  "Given a d value, search all diagonals in the d-band for the furthest
  reaching endpoints. Record the furthest reaching endpoint for each diagonal k
  in the map v. Returns a 3-tuple containing the map of furthest endpionts and
  the x and y value of the sink if it is reached. If the sink is not reached
  then x = y = 0."
  [a b d n m v]
  (loop [diagonals (band-diagonals d)
         v v]
    (if (seq diagonals)
      (let [k (first diagonals)
            [x y] (next-point d k v)
            x (snake a b x y n m)
            y (- x k)
            v (assoc v k x)]
        (if (and (>= x n) (>= y m))
          [v x y] 
          (recur (rest diagonals) v)))
      [v 0 0])))

(defn ses
  "Find the size of the shortest edit script (ses). Returns a 3-tuple of the
  size of the ses, the diagonal on which the sink lies and the map of
  furthest reaching endpoints for each d value. The optimal path form source to
  sink can be constructed from this information."
  [a b]
  (let [n (dec (count a))
        m (dec (count b))]
    (loop [d-range (range (inc (+ n m)))
           endpoints {}]
      (if (seq d-range)
        (let [d (first d-range)
              [v x y] (search-d-band a b d n m
                                     (get endpoints (dec d) {0 {1 0}}))
              endpoints (assoc endpoints d v)]
          (if (and (>= x n) (>= y m))
            [d (- x y) endpoints]
            (recur (rest d-range) endpoints)))
        [(+ n m) (- n m) endpoints]))))

(defn- path
  "Reconstruct the optimal path from source to sink using the information
  produced by the ses function."
  [a b d k endpoints]
  (loop [result '()
         d d
         k k]
    (if (>= d 0)
      (let [x (-> (get endpoints d)
                  (get k))
            y (- x k)
            prev-v (get endpoints (dec d))
            [prev-k b] (if (< (x-down prev-v k) (x-up prev-v k))
                         [(inc k) (x-up prev-v k)]
                         [(dec k) (inc (x-down prev-v k))])
            result (reduce conj
                           (conj result [x y])
                           (reverse
                            (map #(vector % (- % k))
                                 (range b x))))]
        (recur result (dec d) prev-k))
      result)))

(defn- merge-additions
  "Merge sequential additions into a single addition operation."
  [script]
  (assoc script :+
    (reduce (fn [a b]
              (let [l (last a)]
                (if (= (first l) (first b))
                  (-> a
                      butlast
                      vec
                      (conj (vec (conj l (last b)))))
                  (conj a b))))
            []
            (:+ script))))

(defn- path->script
  "Convert a path though an edit graph into an edit script."
  [b path]
  (-> (reduce (fn [e p]
                (let [c (:current e)
                      x (first p)
                      y (last p)
                      delta-x (- x (first c))]
                  (-> (cond (= delta-x (- y (last c)))
                            e
                            (= delta-x 1)
                            (assoc e :- (conj (:- e) (dec x)))
                            :else
                            (assoc e :+ (conj (:+ e) [(dec x) (get b y)])))
                      (assoc :current p))))
              {:current [0 0]
               :+ []
               :- []}
              path)
      (dissoc :current)
      merge-additions))

(defn diff
  "Create an edit script that may be used to transform a into b. See doc string
  for clj-diff.core/diff."
  [a b]
  (opt/diff a b
            (fn [a b] (let [a (vec (cons nil a))
                            b (vec (cons nil b))
                            ses (ses a b)
                            optimal-path (apply path a b ses)]
                        (path->script b optimal-path)))))

