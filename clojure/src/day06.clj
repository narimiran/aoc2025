^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns day06
  {:title "Trash Compactor"
   :url "https://adventofcode.com/2025/day/6"
   :extras ""
   :highlights "comp, partition-by, take-nth"
   :remark "Advent of Parsing."
   :nextjournal.clerk/auto-expand-results? true
   :nextjournal.clerk/toc true}
  (:require [aoc-utils.core :as aoc]
            [clojure.string :as str]))




;; # Day 6: [Trash Compactor](https://adventofcode.com/2025/day/6)
;;
;; We fell in the kitchen and landed in garbage smasher. The only way out is
;; if help the youngest cephalopod with her math homework
;; (on a Saturday morning!?).
;;
;; The homework has multiple numbers in some _columns_ which we need to either
;; add or multiply, looking like this:

(def example "123 328  51 64 .
 45 64  387 23 .
  6 98  215 314
*   +   *   +  ")

;; (I've added `.` at the end of the first two rows in the example above
;; to stop my editor from trimming the trailing whitespace on file save.
;; You can ignore it. Or now you can't anymore.)





;; ## Input parsing
;;
;; A majority of today's task is input parsing (different for each part),
;; so let's do that.
;;
;; For each column, we need to extract the numbers in it and the operator.
;; Let's start with numbers, as that is the only difference between the two
;; parts.



;; ### Part 1 numbers

(defn number-columns [number-rows]
  (->> number-rows
       (map #(map parse-long %)) ; [1]
       aoc/transpose))           ; [2]


;; In every row (outer `map`) we need to parse each element (inner `map`) [1].
;; To convert a list of rows to a list of columns, we use
;; [`aoc/transpose`](https://narimiran.github.io/aoc-utils/aoc-utils.core.html#var-transpose) [2].\
;; Here's an example:

(number-columns [["12" "34"] ["56" "78"]])



;; ### Part 2 numbers
;;
;; In Part 2 we are told that numbers don't quite work as we thought.
;; Yes, they are in columns, but each number is in its own column.
;; We need to parse them differently.

(defn vertical-numbers [input]
  (->> (aoc/parse-lines input :chars) ; [1]
       butlast                        ; [2]
       aoc/transpose                  ; [3]
       ; see below the state at this point
       (map (comp parse-long          ; [4]
                  str/trim            ; [5]
                  str/join))          ; [6]
       (partition-by nil?)            ; [7]
       ; see below the state at this point
       (take-nth 2)))                 ; [8]

;; This time we cannot just blindly parse words. Each space is significant.
;; We'll convert the input into a list of lines, each containing all `:chars`
;; in it [1].
;; As the last line contains the operators, we don't need it here [2].
;; Just as before, we convert the list of rows into a list of cols [3].
;;
;; At this point we have a list of columns which looks like this for the
;; example input:
;; ```
;; [[\1 \space \space]
;;  [\2 \4 \space]
;;  [\3 \5 \6]
;;  [\space \space \space]
;;  [\3 \6 \9]
;;  [\2 \4 \8]
;;  [\8 \space \space]
;;  [\space \space \space]
;;  [\space \3 \2]
;;  [\5 \8 \1]
;;  [\1 \7 \5]
;;  [\space \space \space]
;;  [\6 \2 \3]
;;  [\4 \3 \1]]
;; ```
;;
;; Now, for each (`map`) column, we use
;; [`comp`](https://clojuredocs.org/clojure.core/comp) to create a
;; composition of three functions. We apply them right-to-left.
;; First we convert a column to a string [6] (`[\1 \space \space]` becomes `"1  "`),
;; we remove all whitespace characters [5] (`"1  "` becomes `"1"`)
;; and then convert it to an int [4] (`"1"` becomes `1`).
;;
;; With this conversion, the columns consisting of only spaces become `nil`,
;; which is great as we need them to separate the numbers into separate
;; groups: we use the
;; [`partition-by` function](https://clojuredocs.org/clojure.core/partition-by)
;; to do exactly that [7].
;;
;; The list at this point looks like this:
;; ```
;; ((1 24 356) (nil) (369 248 8) (nil) (32 581 175) (nil) (623 431 4))
;; ```
;;
;; The only thing remaining to get the numbers we need is to take every
;; second element of that list with the
;; [`take-nth` function](https://clojuredocs.org/clojure.core/take-nth) [8].
;;
;; Wow, that was a lot of transformations. Are you still here?





;; ## Solution
;;
;; With the number parsing behind us, we've done 98% of the work for this task.
;; We can now do the remaining 2% and solve it.

(defn calculate [operators numbers]
  (->> (map (fn [op nums] (apply op nums)) operators numbers) ; [1]
       (reduce +)))

;; A nice feature of the [`map` function](https://clojuredocs.org/clojure.core/map)
;; is that it can take multiple collections and iterate through their items
;; in parallel [1].
;; This is exactly what we need, for each column we need the operator and
;; the numbers to apply the operation on.


;; Let's put all this together:

(defn solve [input]
  (let [lines (aoc/parse-lines input #(re-seq #"\d+|\*|\+" %)) ; [1]
        operators (mapv {"+" + , "*" *} (last lines))          ; [2]
        numbers-1 (number-columns (butlast lines))
        numbers-2 (vertical-numbers input)]
    [(calculate operators numbers-1)
     (calculate operators numbers-2)]))

;; To extract what we need from each line, we'll use regex: we're interested
;; in the digits and the operators [1].\
;; We'll transform the string representation of the operators to the functions.
;; We can use a hashmap as a function to do that elegantly [2].
;;
;; Once we get the numbers for each part, we `calculate` the result for both.

(solve example)
(solve (aoc/read-input 6))






;; ## Conclusion
;;
;; Advent of Code usually has a large focus on input parsing,
;; but today's task takes the crown. I don't remember if we ever had a task
;; where we had to parse the input twice, differently for each part.
;;
;; Once we've figured out the parsing (interactive development via REPL was
;; of great help today), the rest was quite easy.
;;
;; Today's highlights:
;; - `comp`: compose multiple functions
;; - `partition-by`: split a collection every time a predicate returns a different
;;   value
;; - `take-nth`: take every n-th element of a collection



;; ----
;;
;; [< Previous solution](../day05)
;; | [Source code](https://github.com/narimiran/aoc2025/blob/main/clojure/src/day06.clj)
;; | [Next solution >](../day07)



^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(defn -main [input]
  (solve input))
