^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns day08
  {:title "Playground"
   :url "https://adventofcode.com/2025/day/8"
   :extras ""
   :highlights "hash-set, disj"
   :remark "No Manhattan distance? Wow!"
   :nextjournal.clerk/auto-expand-results? true
   :nextjournal.clerk/toc true}
  (:require [aoc-utils.core :as aoc]))




;; # Day 8: [Playground](https://adventofcode.com/2025/day/8)
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


;; For each pair `[a b]` of points, we will calculate their `distance-squared` and
;; `sort` the resulting list:

(defn sorted-connections [boxes]
  (sort (for [a boxes
              b boxes
              :while (not= a b)]
          [(distance-squared a b) a b])))

;; This is how it looks for the first few elements of our example:

(->> example-data
     sorted-connections
     (take 4))

;; These are the same connections written in the task, so our logic is correct.
;; We can continue.







;; ### Creating circuits
;;
;; Now we know the order in which the junction boxes need to be connected together.
;; We just don't know _how_.
;;
;; > After connecting [the first two junction boxes], there is a single circuit
;; > which contains two junction boxes, and the remaining 18 junction boxes
;; > remain in **their own individual circuits**.
;;
;; The bold part is the key! Every junction box starts as its own circuit,
;; containing only itself.

(defn create-circuits [points]
  (set (map hash-set points)))

;; Notice the usage of both [`set`](https://clojuredocs.org/clojure.core/set) and
;; [`hash-set`](https://clojuredocs.org/clojure.core/hash-set).
;; The former converts a collection into a set, the latter creates a set from
;; the provided _elements_.

(= (set [1 2 3])
   (hash-set 1 2 3))

(take 5 (create-circuits example-data))

;; Each circuit is a set, currently containing just one point.




;; ### Connecting points
;;
;; To connect two points means joining their circuits into one.
;;
;; For this we need to know the current circuit of a point. We can use the
;; [`aoc/find-first` function](https://narimiran.github.io/aoc-utils/aoc-utils.core.html#var-find-first)
;; to give us the first (and only) circuit containing a point:

(defn current-circuit [circuits pt]
  (aoc/find-first #(% pt) circuits))

;; We're ready to `connect` two points:

(defn connect [circuits a b]
  (let [circ-a (current-circuit circuits a)  ; [1]
        circ-b (current-circuit circuits b)] ; [1]
    (if (= circ-a circ-b)
      circuits                               ; [2]
      (-> circuits
          (disj circ-a circ-b)               ; [3]
          (conj (into circ-a circ-b))))))    ; [4]

;; First we find the `current-circuit` of each point [1]. Per the task, if
;; they are already part of the same circuit, there's nothing for us to do [2].\
;; If they belong to different circuits, we remove those circuits using
;; the [`disj` function](https://clojuredocs.org/clojure.core/disj) [3] and in
;; their place we add a circuit which is the union of the two [4].
;; (Instead of `into`, we could have used `set/union` for the same result.)
;;
;; For example:

(let [circuits #{#{1 2} #{3 4} #{5} #{6 7}}]
  (connect circuits 3 5))




;; ### Scoring
;;
;; Each part has its own way of calculating the score.
;;
;; In Part 1 we need to find three largest circuits and multiply their sizes:

(defn pt1-score [circuits]
  (->> circuits
       (map count)
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
         n 1                                         ; [1]
         scores []]                                  ; [2]
    (let [circuits' (connect circuits a b)]          ; [3]
      (if (= 1 (count circuits'))
        (conj scores (pt2-score a b))                ; [4]
        (recur conns' circuits' (inc n)              ; [5]
               (if (= n rounds)
                 (conj scores (pt1-score circuits')) ; [6]
                 scores))))))


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
;; - [5] Otherwise, we continue with creating the circuits with the remaining
;;   connections (`conns'`). If we hit the number of rounds needed for Part 1,
;;   we calculate the `pt1-score` [6].

(solve example-data 10)
(solve data 1000)





;; ## Conclusion
;;
;; It took me several attempts until I found a way to model this properly, i.e.
;; without creating footguns along the way. The key to making this
;; simpler/solvable was right in front of me (in the task text quoted above),
;; but I've read that carefully way too late, when I already had several
;; gun wounds in my foot.
;;
;;
;; Today's highlights:
;; - `hash-set`: create a set from the provided _elements_
;; - `disj`: remove elements from a set


;; ----
;;
;; [< Previous solution](../day07)
;; | [Source code](https://github.com/narimiran/aoc2025/blob/main/clojure/src/day08.clj)
;; | [Next solution >](../day09)



^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(defn -main [input]
  (let [data (parse-data input)]
    (solve data 1000)))
