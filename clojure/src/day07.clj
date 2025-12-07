^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns day07
  {:title "Laboratories"
   :url "https://adventofcode.com/2025/day/7"
   :extras ""
   :highlights "fnil, memoize"
   :remark "I smell something Lanternfishy."
   :nextjournal.clerk/auto-expand-results? true
   :nextjournal.clerk/toc true}
  (:require [aoc-utils.core :as aoc]))




;; # Day 7: [Laboratories](https://adventofcode.com/2025/day/7)
;;
;; We're in a teleporter lab and there's a problem with a tachyon manifold,
;; whose diagram looks like this:

(def example ".......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
...............")

;; A tachyon beam starts at `S` and moves down until it reaches `^` where
;; it splits to the left and right.


;; ## Input parsing
;;
;; You know the saying: If it quacks like a grid problem, use the
;; [`aoc/create-grid` function](https://narimiran.github.io/aoc-utils/aoc-utils.core.html#var-create-grid).
;; Or something like that.

(defn parse-data [input]
  (-> input
      aoc/parse-lines                 ; [1]
      (aoc/create-grid {\S :start     ; [2]
                        \^ :splits})
      (update :start first)))         ; [3]

;; We split the input into lines, and there's no need for any conversion [1],
;; as the `aoc/create-grid` goes character-by-character and extracts the
;; coordinates of those which match the given predicates [2].\
;; Since there is just one `start`, no need to keep a set of all starts,
;; we extract the only value [3].
;;
;; The returned hashmap contains various keys:

(def example-data (parse-data example))
(def data (parse-data (aoc/read-input 7)))




;; ## Part 1
;;
;; In Part 1 all we need to do is count how many times a beam sees a unique `^`
;; on its way from the `start` to the bottom of the grid.
;;
;; There are three ways a beam can move: straight down, down-left or down-right:

{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn down [x y] [x (inc y)])
(defn dl [x y] [(dec x) (inc y)])
(defn dr [x y] [(inc x) (inc y)])

;; We can now start from the top and count the splits along the way:

{:nextjournal.clerk/visibility {:code :show :result :show}}
(defn part-1 [{:keys [start splits height]}] ; [1]
  (loop [queue (conj aoc/empty-queue start)  ; [2]
         seen  #{}
         cnt   0]
    (let [[x y :as pt] (peek queue)
          queue'       (pop queue)
          seen'        (conj seen pt)]
      (cond
        (= y height) cnt                              ; [3]
        (seen pt)    (recur queue' seen cnt)          ; [4]
        (splits pt)  (recur (conj queue'              ; [5]
                                  (dl x y)
                                  (dr x y))
                            seen'
                            (inc cnt))
        :else        (recur (conj queue' (down x y)) ; [6]
                            seen'
                            cnt)))))

;; We immediately destructure the hashmap we created when parsing, as we
;; need these three keys from it [1].\
;; We will do a BFS, and for that we need a queue where we remove its elements
;; from one end and add the elements to the other end of it.
;; `aoc/empty-queue` is just a shorthand so we don't have to type
;; `clojure.lang.PersistentQueue/EMPTY` [2].
;;
;; There are four different cases we can encounter:
;; - [3] If we reached the bottom of our grid, since we're using BFS, we know
;;   that we've seen all the possible splits and we can return their count.
;; - [4] If we've already `seen` the current point, we immediately recur with
;;   the rest of the queue (`queue'`).
;; - [5] If our current point is in `splits`, we split the beam in two and
;;   increase the `cnt` of seen splits.
;; - [6] Otherwise, it is a regular point and we move one row down.



(part-1 example-data)
(part-1 data)





;; ## Part 2
;;
;; In this part we need to count how many different paths through the grid
;; there are.\
;; There are two different approaches we could take: iterative and recursive.
;; We'll explore both.



;; ### Iterative solution
;;
;; The important thing to notice is that we're always moving downwards and
;; that allows us to iterate row by row.\
;; The idea is to have a hashmap for each row, telling us how many beams are
;; at which position. And since we don't have an equivalent of Python's
;; `Counter`, which treats missing keys as a key with a zero value, we need to
;; use the [`fnil` function](https://clojuredocs.org/clojure.core/fnil)
;; where we define what is a default value if a key is missing.

(def += (fnil + 0))

;; Defining it as a `+=` function, makes the following code a bit nicer to read.

(defn split-beams [splits y beams]
  (reduce (fn [beams' [x cnt]]
            (if (splits [x y])
              (-> beams'
                  (update (dec x) += cnt)
                  (update (inc x) += cnt))
              (update beams' x += cnt)))
          {}
          beams))

;; For all `beams` (defined as a hashmap with `x` positions as keys and
;; the number of beams (`cnt`) at those positions as values) in some row `y`,
;; we calculate the resulting `beams'`, depending on if the beam hits
;; `splits` or not. For example:

(let [beams {5 1 , 7 2 , 9 3}
      splits #{[5 0] [7 0]}]
  (split-beams splits 0 beams))

;; One beam at position `5` is split into one beam at `4` and
;; one at `6`. The two beams at position `7` are split into two beams at `6`
;; (three beams total at `6`) and two at `8`.
;; The three beams at `9` keep their position.
;;
;; Time to use this on all rows of the grid.

(defn part-2-iter [{:keys [start splits height]}]
  (->> (range height)                          ; [1]
       (reduce (fn [beams y]                   ; [2]
                 (split-beams splits y beams))
               {(first start) 1})              ; [3]
       vals                                    ; [4]
       (reduce +)))

;; For each row starting from the top [1], we will successively build a hashmap
;; of the beam positions and their counts [2], starting from the single beam
;; we start from [3].\
;; Once we come to the end, we are interested just in the counts of the beams [4],
;; and we take their sum.

(part-2-iter example-data)
(part-2-iter data)




;; ### Recursive solution
;;
;; For a recursive solution to work in a reasonable time, we must use the
;; [`memoize` function](https://clojuredocs.org/clojure.core/memoize)
;; (or write our own version of it) to not recalculate the result every time
;; we are at an already seen position.

(def part-2-recur
  (memoize
   (fn
     ([grid] (part-2-recur grid (:start grid)))         ; [1]
     ([{:keys [splits height] :as grid} [x y]]
      (cond
        (= y height)   1                                ; [2]
        (splits [x y]) (+ (part-2-recur grid (dl x y))  ; [3]
                          (part-2-recur grid (dr x y)))
        :else          (recur grid (down x y)))))))     ; [4]

;; We define two arities of our function just for convenience, so we can
;; initially call it without specifying the start point [1].
;;
;; Our recursion will exit when we reach the bottom. That's one timeline
;; to count [2].\
;; If we're _not_ at the bottom, there are two possible scenarios:
;; - [3] If we hit the `^`, we split the beam in two and for each continue with
;; the recursion.
;; - [4] If there's no `^`, we continue straight down.
;;
;; That's all we need to do. The rest is for the
;; [Recursion Fairy](https://jeffe.cs.illinois.edu/teaching/algorithms/book/01-recursion.pdf)
;; to deal with.

(part-2-recur example-data)
(part-2-recur data)

;; We get the same result for both variants of the solution.





;; ## Conclusion
;;
;; That's it! We survived the weekend!
;;
;; I initially solved the task with the iterative solution, and now that I've
;; written both, I can't decide if I like the iterative or the recursive
;; solution more.
;;
;; Today's highlights:
;; - `fnil`: provide a way for a function to deal with `nil` values
;; - `memoize`: cache the results of a function


;; ----
;;
;; [< Previous solution](../day06)
;; | [Source code](https://github.com/narimiran/aoc2025/blob/main/clojure/src/day07.clj)
;; | [Next solution >](../day08)



^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(defn -main [input]
  (let [data (parse-data input)]
    [(part-1 data)
     (part-2-iter data)]))
