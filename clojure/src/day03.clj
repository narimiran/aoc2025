^:kindly/hide-code
(ns day03
  {:title "Lobby"
   :url "https://adventofcode.com/2025/day/3"
   :extras ""
   :highlights "type-hints, partial"
   :remark "Recursion made easy."}
  (:require [aoc-utils.core :as aoc]))




;; # Day 3: Lobby
;;
;; We moved from the gift shop to a large lobby. There is an escalator
;; having some power-problems we can solve.\
;; Nearby we find some batteries we can use. They look like this:

(def example "987654321111111
811111111111119
234234234234278
818181911112111")

;; Each line is a battery bank consisting of multiple batteries with
;; their single-digit _joltage_ rating.





;; ## Input parsing
;;
;; This input is the easiest one so far this year to parse.\
;; We need to split it into lines (exactly what
;; [`aoc/parse-lines`](https://narimiran.github.io/aoc-utils/aoc-utils.core.html#var-parse-lines)
;; does), and then for each line we'll convert each character into a digit
;; by using `:digits` as the second argument to that function.

(defn parse-data [input]
  (aoc/parse-lines input :digits))

(def example-data (parse-data example))
(def data (parse-data (aoc/read-input 3)))

;; The parsed example looks like this:
example-data




;; ## Strongest battery
;;
;; Our task is to find maximum _joltage_ of each battery bank, which we get
;; by joining (two) batteries together in reading order, without rearranging them.
;; This means we need to be careful when picking the first battery, as the
;; second one must come after it, i.e. we must know the index of the first
;; battery before searching for the second one.
;;
;; One option would be to create a sequence of `(index, value)` pairs of batteries
;; and then use the built-in
;; [`max-key` function](https://clojuredocs.org/clojure.core/max-key).
;; The problem with this approach is that this function returns the _last_
;; maximal value, which will cause trouble.\
;; Consider these batteries: `[2 7 5 7 3]`. There are multiple maximal values,
;; and if we would use `max-key`, it would pick the latter `7` so we would end up
;; with `73` jolts instead of `77`.
;;
;; Another approach is to use `max` and then take `.indexOf` it,
;; but this would mean we would have to go through the batteries twice.
;; (Not unreasonable since our input is relatively small, but we can do better.)
;;
;; We can do it in one pass by rolling our own function:

(defn strongest-index ^long [batteries ^long start ^long end] ; [1]
  (loop [i start
         best-idx start
         best-v -1]
    (if (= i end)
      best-idx
      (let [v (long (batteries i))]
        (cond
          (= v 9)      i                                      ; [2]
          (> v best-v) (recur (inc i) i v)                    ; [3]
          :else        (recur (inc i) best-idx best-v))))))   ; [4]

;; The `^long` type-hints here allow some noticeable performance benefits, and
;; we'll take that free gift [1].
;; Also for the `v`, we need cast it to `long`, so that the loop variable
;; `best-v` stays primitive (otherwise it becomes an object).\
;; We're talking about microseconds here, but when the whole task takes less
;; than a millisecond, it becomes significant.
;;
;; If we've found battery with `9` jolts, we know there can't be anything
;; more powerful than it and we can exit immediately [2].\
;; Otherwise, we check if the current value is strictly larger than the
;; previous maximum and if so, its index and value become the new accumulator [3].
;; If not, we continue our search with previous best index and value [4].

(strongest-index [2 7 5 7 3] 0 5)

;; It correctly picks up the first `7` at index `1`. We can proceed.







;; ## Max joltage
;;
;; Now we're ready to write the function which will find the maximum
;; joltage of batteries in a single bank.
;; Parts 1 and 2 differ only in the amount of batteries we need to consider.
;;
;; The plan is as follows:\
;; We will find the first battery, taking into the account that we should
;; leave enough space for the remaining batteries, and then, knowing its index,
;; we proceed with finding the second one, etc.
;; Rinse and repeat until we find all batteries we need.\
;; If it sounds like a recursion, it's because it is.
;;
;; We could have a recursive function calling itself, but here I find it nicer
;; to use the [`loop` macro](https://clojuredocs.org/clojure.core/loop).\
;; We'll track several variables in the loop:
;; - `start`: an index from which onwards we're considering possible candidates
;; - `end`: a final index we can use, taking into an account the remaining
;;   batteries we need to find
;; - `remaining`: how many more batteries we need to find
;; - `joltage`: total joltage we've collected so far

(defn max-joltage [amount batteries]
  (loop [start 0
         end (- (count batteries) (dec amount))
         remaining amount
         joltage 0]
    (if (zero? remaining)                             ; [1]
      joltage
      (let [idx (strongest-index batteries start end) ; [2]
            v (long (batteries idx))]
        (recur (inc idx)
               (inc end)                              ; [3]
               (dec remaining)
               (+ (* 10 joltage) v))))))              ; [4]

;; The most important part of a recursion is knowing when to quit.
;; In our case here, it is when there are `zero?` batteries `remaining` to
;; pick up [1]: we just return the collected `joltage` so far.
;;
;; We limit ourselves to allowed `start` and `end` and find the strongest battery
;; in that range [2].
;;
;; For the next iteration we can consider one more battery on the right [3]
;; and we're adding the power of the battery we've just found to the
;; total `joltage` [4].
;;
;; This should be it, but let's check if it works as expected for different
;; amounts of batteries we need:

(let [batteries [8 9 7 9 2 7 3 5]]
  (for [amount (range 9)]
    (max-joltage amount batteries)))

;; This looks correct. Time to solve the task.







;; ## Solutions

;; We need to count joltage for each bank and then sum everything up.
;; This is what the
;; [`aoc/sum-map` function](https://narimiran.github.io/aoc-utils/aoc-utils.core.html#var-sum-map)
;; does: it applies a function to each element of a collection and then takes
;; a sum of the result. (It is a shorter way of writing `(reduce + (map f xs))`).

(defn total-output [amount banks]
  (aoc/sum-map #(max-joltage amount %) banks))


;; We can define functions for each part by partially applying the `amount` of
;; wanted batteries in each bank. There is a built-in
;; [`partial` function](https://clojuredocs.org/clojure.core/partial) which
;; does exactly that:

(def part-1 (partial total-output 2))
(def part-2 (partial total-output 12))



;; And now we can call those functions as usual:

(part-1 example-data)
(part-1 data)


;; And the solution for Part 2 is:

(part-2 example-data)
(part-2 data)






;; ## Conclusion
;;
;; This was a nice task!\
;; I expected Part 2 to have battery banks being vertical, but this was more
;; interesting to solve.
;;
;; It's good to get a taste of recursion on something not mind-bending.\
;; But what is mind-bending is that 25% of this year's AoC is already behind us!
;;
;; Today's highlights:
;; - type hints: sometimes a simple `^long` type hint or casting to `long`
;;   can help with the performance
;; - `partial`: partial application of a function



^:kindly/hide-code
(defn -main [input]
  (let [data (parse-data input)]
    [(part-1 data)
     (part-2 data)]))
