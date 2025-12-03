^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns day03
  {:title "Lobby"
   :url "https://adventofcode.com/2025/day/3"
   :extras ""
   :highlights "reduce-kv, reduced, partial"
   :remark "Recursion made easy."
   :nextjournal.clerk/auto-expand-results? true
   :nextjournal.clerk/toc true}
  (:require [aoc-utils.core :as aoc]))




;; # Day 3: [Lobby](https://adventofcode.com/2025/day/3)
;;
;; We moved from the gift shop to a largy lobby. There is an escalator
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





;; ## Strongest battery
;;
;; Our task is to find maximum _joltage_ of each battery bank, which we get
;; by joining (two) batteries together in reading order, without rearanging them.
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
;; and if we would use `max-key` we would end up with `73` jolts instead of `77`.
;;
;; Another approach is to use `max` and then take `.indexOf` it,
;; but this would mean we would have to go through the batteries twice.
;; (Not unreasonable since our input is relatively small, but we can do better.)
;;
;; We can do it in one pass by rolling our own function:

(defn strongest-battery [batteries]
  (reduce-kv (fn [[max-idx max-v] idx v]       ; [1]
               (cond
                 (= v 9)     (reduced [idx v]) ; [2]
                 (> v max-v) [idx v]           ; [3]
                 :else       [max-idx max-v]))
             [0 0]
             batteries))

;; We could have converted our vector of batteries into the one with
;; `(index, value)` pairs, but I find it nicer to use the
;; [`reduce-kv` function](https://clojuredocs.org/clojure.core/reduce-kv)
;; which does that automatically for us [1].
;; The first argument to `reduce-kv` is a function which takes three arguments:
;; accumulator, key, value. This is usually used for hashmaps, but it also
;; works for vectors where the key is the index of an element.
;;
;; If we've found battery with `9` jolts, we know there can't be anything
;; more powerful than it and we can exit immediately with
;; [`reduced`](https://clojuredocs.org/clojure.core/reduced) [2].\
;; Otherwise, we check if the current value is strictly larger than the
;; previous maximum and if so, its index and value become the new accumulator.
;;

(strongest-battery [2 7 5 7 3])

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
    (if (zero? remaining)  ; [1]
      joltage
      (let [candidates (subvec batteries start end)    ; [2]
            [idx v]    (strongest-battery candidates)] ; [3]
        (recur (+ start (inc idx))       ; [4]
               (inc end)                 ; [5]
               (dec remaining)
               (+ (* 10 joltage) v)))))) ; [6]

;; **The most important part of a recursion is knowing when to quit.**
;; In our case here, it is when there are `zero?` batteries `remaining` to
;; pick up [1]: we just return the collected `joltage` so far.
;;
;; Otherwise, we need to find the valid `candidates` among the battery bank [2].
;; We use the [`subvec` function](https://clojuredocs.org/clojure.core/subvec),
;; which does what we want with great `O(1)` performance.\
;; Once we've limited ourselves to the valid candidates, we can now find
;; the `strongest-battery` among them [3].
;; Be careful, this will return `idx` of a given subvector so we need
;; to _add_ it to the index it `start`ed from [4].\
;; For the next iteration we can consider one more battery on the right [5]
;; and we're adding the power of the battery we've just found to the
;; total `joltage` [6].
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
;; - `reduce-kv`: can be used on vectors to get indices
;; - `reduced`: early exit from `reduce`
;; - `partial`: partial application of a function


;; ----
;;
;; [< Previous solution](day02)
;; | [Source code](https://github.com/narimiran/aoc2025/blob/main/clojure/src/day03.clj)
;; | [Next solution >](day04)



^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(defn -main [input]
  (let [data (parse-data input)]
    [(part-1 data)
     (part-2 data)]))
