^:kindly/hide-code
(ns day08
  {:title "Playground"
   :url "https://adventofcode.com/2025/day/8"
   :extras ""
   :highlights "zipmap, group-by, sort-by"
   :remark "No Manhattan distance? Wow!"}
  (:require [aoc-utils.core :as aoc]))




;; # Day 8: Playground
;;
;; We use the teleporter and find ourselves in a company of Elves trying to
;; connect some junction boxes. We're given a list of their coordinates which
;; looks like this:

(def example "162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689")




;; ## Input parsing
;;
;; Each line contains x, y, z coordinate of one junction box. No need to
;; split on the `,` character and parse each number separately,
;; the `:ints` argument extracts all integers it sees.

(defn parse-data [input]
  (aoc/parse-lines input :ints))

(def example-data (parse-data example))
(def data (parse-data (aoc/read-input 8)))





;; ## Solution
;;
;; Both parts share a similar logic, so we'll do both at once.



;; ### Creating connections
;;
;; Now that we've parsed the input and have a list of junction boxes,
;; we need to connect them depending on their distance.
;;
;; To my surprise, we don't use the Manhattan distance, but a straight-line
;; distance in 3D space. Since we do the same for all pairs, we don't really need
;; to take a square root of the distances.

(defn sq [x] (* x x))

(defn distance-squared [[x1 y1 z1] [x2 y2 z2]]
  (+ (sq (- x2 x1))
     (sq (- y2 y1))
     (sq (- z2 z1))))


;; If we calculate the distance between every junction box in our input, we
;; would create a huge list of 500.000 elements. And then we had to `sort`
;; that list.\
;; By looking at the numbers in our input, we can assume that lots of boxes
;; are very far apart and they will not be directly connected.
;;
;; One thing we can do to speed up creating the connections is to group
;; the nearby boxes. This is how this logic would look like in 1D:
;;
;; ```
;;   A  |  B  |  C  |  D  |  E
;;      |     |     |     |
;; -----+-----+-----+-----+----->
;;      |     |     |     |
;; ```
;;
;; We would try to connect points in the `C` segment only with
;; other points in `B`, `C` and `D` segments. Points in `A` and `E`
;; segments are too far to consider them.\
;; We apply the same logic in 3D by dividing our space in smaller cubes.
;;
;; After some tweaking and experimenting, the size of a cube was picked up to be
;; 20.000 in each direction, and we'll use the same value later on to limit
;; ourselves to only the connections whose distance is less than that.

(def threshold 20000)

(defn cube [box]
  (mapv #(quot % threshold) box))

;; This is how it looks for some of our boxes:

(for [box (take 3 data)]
  [box (cube box)])

;; Now we can create `sorted-connections` of our `boxes`:

(defn sorted-connections [boxes]
  (let [limit (sq threshold)                     ; [1]
        cubes (group-by cube boxes)]             ; [2]
    (sort (for [[cube points] cubes
                nb-cube (aoc/neighbours-27 cube) ; [3]
                a points
                b (cubes nb-cube)
                :when (neg? (compare a b))       ; [4]
                :let [dist (distance-squared a b)]
                :when (< dist limit)]            ; [5]
            [dist a b]))))

;; As mentioned before, we'll limit the connection distance. Since we calculate
;; squared distances, we need to `sq`uare the limit too [1].
;;
;; We will [`group-by`](https://clojuredocs.org/clojure.core/group-by) the boxes
;; by the `cube` they belong to [2] and then iterate through each cube.
;; In fact, we have a very nested for-loop.\
;; For each cube we will calculate all 3x3x3 neighbouring cubes [3] by using my
;; [`aoc/neighbours-27` helper function](https://narimiran.github.io/aoc-utils/aoc-utils.core.html#var-neighbours-27),
;; and then for each point `a` in our cube and each point `b` in a neighbouring
;; cube calculate their distance.\
;; The idea here is to ignore all potential connections between the points that
;; are two or more cubes away from each other, as they'd surely have too large
;; distance between them.
;;
;; We'll further narrow down the number of combinations we explore.
;; The condition at [4] makes sure that for each pair of points we calculate
;; their distance only once (as we will encounter each pair twice).\
;; For the points that are at most one cube away from each other, we will
;; make the connection only when the distance between them is lower than our
;; `limit` [5].


;; This is how it looks for the first few elements of our example:

(->> example-data
     sorted-connections
     (take 4))

;; These are the same connections written in the task, so our logic is correct.
;; We can continue.







;; ### Creating circuits
;;
;; Now we know the order in which the junction boxes need to be connected together.
;; We just don't know _how_. Here's what the task says:
;;
;; > After connecting [the first two junction boxes], there is a single circuit
;; > which contains two junction boxes, and the remaining 18 junction boxes
;; > remain in **their own individual circuits**.
;;
;; The bold part is the key! Every junction box starts as its own circuit,
;; containing only itself.


;; This is a [disjoint-set (union-find) problem](https://en.wikipedia.org/wiki/Disjoint-set_data_structure)
;; and we'll implement the functions needed to do this effectively.
;;
;; The first thing to do is to create circuits.

(defn create-circuits [boxes]
  {:parents (zipmap boxes boxes)      ; [1]
   :sizes   (zipmap boxes (repeat 1)) ; [2]
   :count   (count boxes)})           ; [3]

;; The [`zipmap` function](https://clojuredocs.org/clojure.core/zipmap)
;; creates a hashmap from a collection of keys and a collection of values.
;;
;; Every `box` starts as its own `parent` [1], and `size` of each circuit is
;; one [2]. For Part 2 we'll need to track the number of circuits [3].




;; ### Connecting points
;;
;; To connect two points means joining their circuits into one.
;; We'll follow the union-find algorithm.
;;
;; First, we need a function which will tell us a root of a point:

(defn find-root [parents pt]
  (loop [pt pt]
    (let [p (parents pt)]
      (if (= p pt) pt
          (recur p)))))


;; Next one is a function to connect two sets:

(defn connect [{:keys [sizes parents] :as circuits} a b]
  (let [ra (find-root parents a)
        rb (find-root parents b)]
    (if (= ra rb)                                      ; [1]
      circuits
      (let [[small large] (sort-by sizes [ra rb])]     ; [2]
        (-> circuits
            (assoc-in [:parents small] large)          ; [3]
            (update-in [:sizes large] + (sizes small)) ; [4]
            (update :count dec))))))                   ; [5]

;; If points `a` and `b` have the same root, they are part of the same circuit
;; and there's nothing for us to do [1].
;;
;; Otherwise, we need to know which of the two circuits is larger [2], so that we
;; can add the smaller circuit to the larger one. We can use the
;; [`sort-by` function](https://clojuredocs.org/clojure.core/sort-by) to sort
;; the roots by their `size`.
;;
;; We connect two circuits by making the larger root a parent of the smaller one [3].
;; We need to update its size too [4]. Lastly, by joining two circuits,
;; there is one less circuit present [5].





;; ### Scoring
;;
;; Each part has its own way of calculating the score.
;;
;; In Part 1 we need to find three largest circuits and multiply their sizes:

(defn pt1-score [circuits]
  (->> (:sizes circuits)
       vals
       (sort >)
       (take 3)
       (reduce *)))


;; In Part 2 we need to multiply x coordinates of the last two junction boxes:

(defn pt2-score [[ax _ _] [bx _ _]]
  (* ax bx))






;; ### All together now
;;
;; Time to put all this together and solve the task using the functions
;; defined above.
;;
;; We will create the connections between the circuits only once.
;; We'll keep doing that until we solve Part 2. The solution for Part 1
;; will come along the way.

(defn solve [points rounds]
  (loop [[[_ a b] & conns'] (sorted-connections points)
         circuits (create-circuits points)
         n 1                                ; [1]
         scores []]                         ; [2]
    (let [circuits' (connect circuits a b)] ; [3]
      (if (= 1 (:count circuits'))
        (conj scores (pt2-score a b))       ; [4]
        (recur conns' circuits' (inc n)     ; [5]
               (cond-> scores
                 (= n rounds)               ; [6]
                 (conj (pt1-score circuits'))))))))

;; We will track the number of connections we've created, as in Part 1 we need
;; to calculate the score after some `rounds` [1] (different for the example
;; and the real input).\
;; The `scores` for each part will be held in this vector [2].
;;
;; For each connection `[a b]` we will add it to the existing circuits [3].
;; There are two possibilities:
;; - [4] We've connected everything into one large circuit. We calculate the
;;   `pt2-score` and at this point we have everything we need and we exit the
;;   loop.
;; - [5] Otherwise, we continue with connecting the circuits with the remaining
;;   connections (`conns'`). When we hit the number of rounds needed for Part 1,
;;   we calculate the `pt1-score` [6].

(solve example-data 10)
(solve data 1000)





;; ## Conclusion
;;
;; It took me several attempts until I found a way to model this properly, i.e.
;; without creating footguns along the way.\
;; The initial version had a naive (read: inefficient) implementation,
;; but the current one uses the proper union-find algorithm.
;;
;;
;; Today's highlights:
;; - `zipmap`: create a hashmap from provided collections
;; - `group-by`: group a collection into a hashmap, based on the provided function
;; - `sort-by`: sort a collection, based on the results of a provided function


^:kindly/hide-code
(defn -main [input]
  (let [data (parse-data input)]
    (solve data 1000)))
