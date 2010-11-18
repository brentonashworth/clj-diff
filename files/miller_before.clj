(ns clj-diff.miller
  "Algorithm from 'An O(NP) Sequence Comparison Algorithm' by
Sun Wu, Udi Manber, Gene Myers and Web Miller.

Please refer to the above paper while reading this code."
  (:require [clj-diff [myers :as myers]]))

(defn- next-x
  "Get the next farthest x value by looking at previous farthest values on the
diagonal above and below diagonal k. Choose the greater of the farthest x on
the above diagonal and the farthest x on the diagonal below plus one. fp is
a map of p values to maps of diagonals => farthest points."
  [k fp]
  (max (inc (get fp (dec k) -1))
       (get fp (inc k) -1)))

(defn- snake
  "Starting at the farthest point on diagonal k, return the x value of the
point at the end of the longest snake on this diagonal. A snake is a
sequence of diagonal moves connecting match points on the edit graph."
  [a b n m k fp]
  (let [x (next-x k fp)
        y (- x k)]
    (myers/snake a b x y n m)))

(defn- search-p-band
  "Given a p value, search all diagonals in the p-band for the furthest
reaching endpoints. Record the furthest reaching endpoint for each p value
in the map fp. Returns an updated fp map for p."
  [a b n m delta p fp]
  (reduce (fn [fp next-k]
            (assoc fp next-k (snake a b n m next-k fp)))
          fp
          (concat (range (* -1 p) delta)
                  (reverse (range (inc delta) (+ delta p)))
                  [delta])))

(defn ses
  "Find the size of the shortest edit script (ses). Returns a 3-tuple of the
size of the ses, the delta value (which is the diagonal of the end point)
and the fp map. The optimal path form source to sink can be constructed from
this information."
  [a b]
  {:pre [(>= (count a) (count b))]}
  (let [n (dec (count a))
        m (dec (count b))
        delta (- n m)]
    (loop [p 0
           fp {}]
      (if (= (-> (get fp (dec p) {})
                 (get delta))
             n)
        [(dec p) delta fp]
        (recur (inc p)
               (assoc fp p
                      (search-p-band a b n m delta p (get fp (dec p) {}))))))))

(defn- dec-p
  "The p-map maps diagonals to the current p value that should be used to
find the farthest x value for this diagonal. Use of this map allows us to
treat fp as a set of stacks, one for each diagonal. This function is called
when you want to pop something off of k's stack."
  [p-map k]
  (let [p (get p-map k -1)]
    (if (>= p 0)
      (assoc p-map k (dec p))
      p-map)))

(defn- farthest-x-on-k
  "Get the current farthers x value of diagonal k."
  [fp p-map k]
  (let [p (get p-map k)
        fp (get fp p {})]
    (get fp k -1)))

(defn- x-up
  "Get the farthest x on the diagonal above diagonal k. This value must be <=
x."
  [fp k x p-map]
  (loop [p-map p-map]
    (let [k* (inc k)
          x* (farthest-x-on-k fp p-map k*)]
      (if (<= x* x)
        [x* p-map]
        (recur (dec-p p-map k*))))))

(defn- x-down
  "Get the farthest x on the diagonal below diagonal k. This value must be <
x."
  [fp k x p-map]
  (loop [p-map p-map]
    (let [k* (dec k)
          x* (farthest-x-on-k fp p-map k*)]
      (if (< x* x)
        [x* p-map]
        (recur (dec-p p-map k*))))))

(defn- path
  "Reconstruct the optimal path from source to sink using the information
produced by the ses function."
  [a b p delta fp]
  (loop [result '()
         p-map (zipmap (keys (get fp p)) (repeat p))
         k delta]
    (if (= (first result) [0 0])
      result
      (let [x (farthest-x-on-k fp p-map k)
            p-map (dec-p p-map k)
            y (- x k)
            [up p-map] (x-up fp k x p-map)
            [down p-map] (x-down fp k x p-map)
            [prev-k b] (if (< down up)
                         [(inc k) up]
                         [(dec k) (inc down)])
            result (reduce conj
                           (conj result [x y])
                           (reverse
                            (map #(vector % (- % k))
                                 (range b x))))]
        (recur result p-map prev-k)))))

(defn- diff*
  "Calculate the optimal path using the miller algorithm. This algorithm
requires that a >= b."
  [a b]
  {:pre [(>= (count a) (count b))]}
  (let [ses (ses a b)
        optimal-path (apply path a b ses)]
    optimal-path))

(defn- transpose
  "Transpose the edit script by reversing each pair of x and y values. If the
path represents the edit path to transform a -> b then transposition will
result in the edit path to transform b -> a."
  [path]
  (map #(vec (reverse %)) path))

(defn diff
  "Create an edit script that may be used to transform a into b. See doc string
for clj-diff.core/diff. This function will ensure that diff* is called with
arguments a and b where a >= b. If the passed values of a and b need to be
swapped then the resulting path with will transposed."
  [a b]
  (let [a (vec (cons nil a))
        b (vec (cons nil b))
        [a* b*] (if (> (count b) (count a)) [b a] [a b])
        optimal-path (diff* a* b*)
        optimal-path (if (= a* a)
                       optimal-path
                       (transpose optimal-path))]
    (myers/path->script b optimal-path)))