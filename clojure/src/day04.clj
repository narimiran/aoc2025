^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns day04
  {:title "Printing Department"
   :url "https://adventofcode.com/2025/day/4"
   :extras ""
   :highlights "grid helpers"
   :remark "The easiest one this year."
   :nextjournal.clerk/auto-expand-results? true
   :nextjournal.clerk/toc true}
  (:require [aoc-utils.core :as aoc]
            [clojure.set :as set]))




;; # Day 4: [Printing Department](https://adventofcode.com/2025/day/4)
;;
;; We're in the printing department and we're given a plan view of it which
;; looks like this:

(def example "..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.")

;; Every `@` is a large roll of paper (am I the only one who initialy mis-read
;; the task and thought this is about some rolls of _toilet_ paper?)
;; and the problem is that they are blocking the wall to a cafeteria, where
;; we want to go next.
;; There are some forklifts, but they can move a roll of paper only if there
;; are less than four rolls neighbouring it in eight adjacent positions.
;;
;; It was just a question of time where a grid-based task will come up.
;; And we came well-prepared.\
;; My solution will use the
;; [grid-helper functions](https://narimiran.github.io/aoc-utils/intro.html#grids)
;; from my `aoc-utils` library. I'll try to briefly explain each function I use,
;; and I'll link to its documentation so you can get more information there.




;; ## Input parsing
;;
;; We've already met the
;; [`aoc/parse-line` function](https://narimiran.github.io/aoc-utils/aoc-utils.core.html#var-parse-lines)
;; in our previous solutions.
;; The only difference is that here we want a list of characters on each lines,
;; and that is what `:chars` does.
;;
;; Once we have the list of characters, we want to extract only
;; the positions of `@` characters, as those our the rolls of paper we're
;; interested in.

(defn parse-data [input]
  (-> input
      (aoc/parse-lines :chars)
      (aoc/create-grid {\@ :rolls}) ; [1]
      :rolls))                      ; [2]

;; The [`aoc/create-grid` function](https://narimiran.github.io/aoc-utils/aoc-utils.core.html#var-create-grid)
;; does the job for us. Its last argument is a mapping from the characters
;; we're interested in to their name in the produced hashmap [1].\
;; This function produces a hashmap with various useful keys (e.g. the size
;; of the map), but this time we're interested only in the coordinates of
;; the `:rolls` of paper [2].

(def example-data (parse-data example))
(def data (parse-data (aoc/read-input 4)))





;; ## Part 1
;;
;; Now that we have coordinates of all rolls, our task is to find those
;; which are accessible by the forklifts.
;; For each roll we need to check how many of its 8 adjacent coordinates are
;; taken by other rolls. If there are less than four of them, it means the roll
;; is accessible by the forklifts.

(defn accessible-roll? [rolls roll]
  (let [neighbouring-rolls (aoc/neighbours-8 roll rolls)] ; [1]
    (< (count neighbouring-rolls) 4)))                    ; [2]

;; The [`aoc/neighbours-8` function](https://narimiran.github.io/aoc-utils/aoc-utils.core.html#var-neighbours-8)
;; takes a point as the first argument and a predicate we want to filter by as
;; its second argument [1]. Since `rolls` is a set, we can directly use it as
;; a predicate. The result are rolls which are adjacent to the roll
;; we're currently exploring.\
;; The roll is accessible only if it has less than four neighbours [2].
;;
;; We can use now use this function to `filter` all rolls:

(defn accessible [rolls]
  (->> rolls
       (filter #(accessible-roll? rolls %))
       set)) ; [1]

;; We started with `rolls`, which is a `set`.
;; The result of `filter` is a sequence, and we'll convert it back to set,
;; for convenience [1].


;; Our Part 1 task is to count all accessible rolls, and our code reads
;; exactly like that:

(defn part-1 [rolls]
  (count (accessible rolls)))


(part-1 example-data)
(part-1 data)

;; Boom, done!







;; ## Part 2
;;
;; In Part 2 we realize that when we remove some rolls, some new ones become
;; `accessible`. We need to repeat the process until we cannot remove any more
;; rolls.
;;
;; Recursion time! Unlike [yesterday](./day03) when we used `loop` for recursion,
;; today we'll do it by repeatedly calling a function.

(defn part-2
  ([rolls] (part-2 rolls 0)) ; [1]
  ([rolls removed]
   (let [to-remove (accessible rolls)]
     (if (empty? to-remove)  ; [2]
       removed
       (recur (set/difference rolls to-remove)   ; [3]
              (+ removed (count to-remove)))))))

;; Not necessarily needed, but we're defining two arities of the same function
;; so we can call it without specifying zero as the starting number of `removed`
;; rolls [1].
;;
;; We're exiting the recursion when there's nothing `to-remove` [2].\
;; Otherwise, we call the same function (this is what `recur` does here) with
;; new arguments: the remaining rolls can be calculated as a set difference
;; between all rolls and those we can remove [3], and the number of removed
;; rolls is increased by the number of removed in this step.


(part-2 example-data)
(part-2 data)







;; ## Conclusion
;;
;; This was maybe the easiest one this year. Especially if you have grid-helpers
;; ready. But, knowing AoC, this usually means something difficult is coming
;; up very soon.\
;; And we can expect harder grid-based task(s) later on.
;;
;; This task was ideal to make some animations of rolls removal, but unfortunately
;; I didn't have the time to do it today. Maybe I'll add something later on,
;; time permitting.
;;
;; Today's highlights:
;; - `aoc/create-grid`: helper for tasks like this one
;; - `aoc/neighbours-8`: get 8 neighbours of a point which satisfy a predicate


;; ----
;;
;; [< Previous solution](day03)
;; | [Source code](https://github.com/narimiran/aoc2025/blob/main/clojure/src/day04.clj)
;; | [Next solution >](day05)



^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(defn -main [input]
  (let [data (parse-data input)]
    [(part-1 data)
     (part-2 data)]))
