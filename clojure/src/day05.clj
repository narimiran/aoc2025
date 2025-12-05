^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns day05
  {:title "Cafeteria"
   :url "https://adventofcode.com/2025/day/5"
   :extras ""
   :highlights "some, sort"
   :remark "Work sorter, not harder."
   :nextjournal.clerk/auto-expand-results? true
   :nextjournal.clerk/toc true}
  (:require [aoc-utils.core :as aoc]))




;; # Day 5: [Cafeteria](https://adventofcode.com/2025/day/5)
;;
;; We break the (fourth?) wall and reach the cafeteria. The Elves just switched
;; their inventory management system and now they have some problems with it.
;; How typical.
;;
;; But, as always, we can help. They show us their database, which is divided
;; in two paragraphs like this:

(def example "3-5
10-14
16-20
12-18

1
5
8
11
17
32")

;; The first paragraph are ranges of fresh ingredients, and the second one
;; are ingredients. We need to tell them how many of the ingredients are fresh.





;; ## Input parsing
;;
;; We've already dealt with ranges of natural numbers in [Day 2](../day02), so
;; we'll once again use the `:nats` parameter to extract them.\
;; This time in our input we have two paragraphs, separated by a blank line.
;; This kind of input happens frequently so we came prepared for it:
;; the [`aoc/parse-paragraphs` function](https://narimiran.github.io/aoc-utils/aoc-utils.core.html#var-parse-paragraphs)
;; splits the input into two lists, each having lines as a separate elements.
;; The second argument is applied to each line.

(defn parse-data [input]
  (let [[ranges ingredients] (aoc/parse-paragraphs input :nats)]
    [ranges
     (mapv first ingredients)])) ; [1]

;; Since `:nats` will extract _all_ integers on a line into a list, for the
;; second paragraph, where there is just one number, we immediately extract it.


(def example-data (parse-data example))
(def data (parse-data (aoc/read-input 5)))


;; This looks exactly what we need. Notice the huge numbers in the real input.






;; ## Part 1
;;
;; For each ingredient in the second paragraph, we need to check if it is fresh.
;; In other words, we need to check if it is at least in `some` of the ranges
;; in the first paragraph:

(defn fresh? [ranges ingredient]
  (some (fn [[lo hi]] (<= lo ingredient hi)) ranges))

;; The [`some` function](https://clojuredocs.org/clojure.core/some) does
;; exactly that. We destructure each range into its `lo` and `hi` ends, and
;; check if the `ingredient` is between them.



;; Now we need to do that for every ingredient and count how many of them
;; are fresh.\
;; We could `filter` the ingredients and then `count` how many fresh are there,
;; but I have a helper function
;; [`aoc/count-if`](https://narimiran.github.io/aoc-utils/aoc-utils.core.html#var-count-if),
;; which does that in a single step:

(defn part-1 [[ranges ingredients]]
  (aoc/count-if #(fresh? ranges %) ingredients))


(part-1 example-data)
(part-1 data)


;; Two simple oneliners and Part 1 is done!






;; ## Part 2
;;
;; Here we need to find how many _total_ ingredients are considered fresh, i.e.
;; what is the total size of the ranges. And we must be careful when counting
;; because the ranges overlap.\
;; Remember that we noticed that the real input has huge numbers? A naive
;; solution (e.g. creating a set with all numbers in the ranges) won't do the
;; trick here. We need to be smarter than that.
;;
;; The idea is to sort the ranges and then go one by one.\
;; There are two (or three, depending on how you count) situations
;; we can encounter:
;;
;; ```
;; L------------H
;;   L----H                       [1]
;;           L-----H              [2a]
;;                    L-------H   [2b]
;; ```
;;
;; When we have sorted ranges, we know that each subsequent one will start
;; at least where the previous one started, so no need to check the lower
;; end of the ranges.\
;; If the range we're looking at has its `hi` end below the previous one [1],
;; there's nothing we need to do, all of the points are already included.
;; The other situation is if the `hi` end is larger than the previous one:
;; there are some new points we need to include. There are two sub-variants
;; of this case: when the `lo` is below the previously highest point [2a],
;; and when it is larger [2b].
;;
;; Enough talking, let's write some code.

(defn part-2 [[ranges _]] ; [1]
  (->> (sort ranges)      ; [2]
       (reduce (fn [[fresh highest :as acc] [lo hi]]
                 (if (<= hi highest)
                   acc                                ; [3]
                   (let [start (max lo (inc highest)) ; [4]
                         size  (- (inc hi) start)]
                     [(+ fresh size) hi])))
               [0 -1])
       first)) ; [5]



;; For this part we don't need the list of ingredients (`_`), just the
;; `ranges` [1].\
;; Before doing anything else, we `sort` the ranges
;; (the [`sort` function](https://clojuredocs.org/clojure.core/sort) works
;; out of the box for vector elements, no need to manually specify a
;; comparator).
;;
;; If the `hi` of the current range is lower than the highest one so far
;; (the case 1 from the graphical example above), then there's nothing for us to do,
;; we keep the `acc`umulator as is and move to the next range to check [3].\
;; We can deal with the cases 2a and 2b from the example above in one go: we use `max`
;; to find the first new ingredient to include [4] and then calculate the size
;; of new fresh ingredients.
;;
;; The returned accumulator will have two elements, and we're interested only
;; in the first one [5].

(part-2 example-data)
(part-2 data)





;; ## Conclusion
;;
;; The first task this year where a naive (i.e. brute force) approach won't work.
;; But it was easy to come up with a sufficiently smart approach which does
;; the trick for Part 2.
;;
;; And now let's get ready for the weekend (when, traditionally, the tasks
;; are harder). The only weekend in this year's AoC. Oh my.
;;
;;
;; Today's highlights:
;; - `some`: is there at least one element of a collection which satisfies a predicate
;; - `sort`: sort a collection. Shocking, I know.


;; ----
;;
;; [< Previous solution](../day04)
;; | [Source code](https://github.com/narimiran/aoc2025/blob/main/clojure/src/day05.clj)
;; | [Next solution >](../day06)



^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(defn -main [input]
  (let [data (parse-data input)]
    [(part-1 data)
     (part-2 data)]))
