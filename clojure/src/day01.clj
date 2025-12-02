^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns day01
  {:title "Secret Entrance"
   :url "https://adventofcode.com/2025/day/1"
   :extras "bench"
   :highlights "subs, mapcat, reductions"
   :remark "Harder than expected for day 1."
   :nextjournal.clerk/auto-expand-results? true
   :nextjournal.clerk/toc true}
  (:require [aoc-utils.core :as aoc]))


;; # Day 1: [Secret Entrance](https://adventofcode.com/2025/day/1)
;;
;; Aaaand we're back!
;;
;; It looks like this year we have 12 days to help Elves decorate the North Pole.
;;
;; Some quick links before we start:
;; - I'll try to make these notebooks beginner-friendly (especially for the easier
;;   tasks), but if you're completely new to Clojure, last year I've made a
;;   [Quick intro to Clojure](https://narimiran.github.io/aoc2024/clojure_intro/),
;;   but it would be wise to pick up some other resources too.
;; - I'll be extensively using the helper functions from my
;;   [aoc-utils library](https://narimiran.github.io/aoc-utils/intro.html).
;; - My writing here assumes that you've solved at least Part 1 of the day you're
;;   reading so you're familiar what the Part 2 is about.
;;
;; ----
;;
;; So, let's dive in!
;;
;; For today's task, we're standing next to a safe dial and we're given a document
;; containing a sequence of rotations which looks like this:

(def example "L68
L30
R48
L5
R60
L55
L1
L99
R14
L82")

;; ...and we need to rotate the safe dial according to these instructions.




;; ## Input parsing
;;
;; Each line of the input consits of a direction (either `L` or `R`) and
;; a number of turns we need to take in that direction.\
;; Since `L` means lowering the number on the safe dial, we'll multiply
;; the number of turns with `-1`.
;;
;; One option would be to destructure each line into `[direction & digits]`,
;; but then we would have to do `(apply str digits)` to concatenate them back.\
;; My approach is for digits we take a substring from index 1 onwards with the
;; [`subs` function](https://clojuredocs.org/clojure.core/subs).

(defn parse-line [line]
  (* (if (= \L (first line)) -1 1)
     (parse-long (subs line 1))))

(parse-line "L234")

;; It works correctly. Now we need to do that for every line of the input.\
;; This is what the
;; [`parse-lines` function](https://narimiran.github.io/aoc-utils/aoc-utils.core.html#var-parse-lines)
;; from my [aoc-utils library](https://narimiran.github.io/aoc-utils/intro.html)
;; does: it takes a multi-line input, splits it into lines and maps a given
;; function to each line:

(defn parse-data [input]
  (aoc/parse-lines input parse-line))

;; We can now parse both the example and our real input:

(def example-data (parse-data example))
(def data (parse-data (aoc/read-input 1)))


;; The dial has 100 numbers on it, from 0 to 99, and it starts at number 50.
;; Let's define those so we don't repeat ourselves later on:

(def size 100)
(def start 50)





;; ## Part 1
;;
;; In Part 1 we need to count how many times the dial stops at the value zero
;; after a rotation.

(defn part-1 [data]
  (-> (reduce (fn [[zeros start] n]
                (let [end (mod (+ start n) size)]     ; [1]
                  [(if (zero? end) (inc zeros) zeros) ; [2]
                   end]))
              [0 start]                               ; [3]
              data)
      first))                                         ; [4]

;; We will `reduce` through the input data.\
;; We need to track two values [3]: the number of times we reached zero, and
;; the current value on the dial.
;; We need to stay in the 0-99 range of the dial [1] and if result of a rotation
;; is zero, we increase the count [2].\
;; The `reduce` function will return both the number of times we've seen
;; zero and the final value on the dial. We're interested only in the former [4].

(part-1 example-data)
(part-1 data)

;; Woohoo! Our first star this year!






;; ## Part 2
;;
;; We've realized we had made a wrong assumption (oh boy, this hits too close
;; to home). We need to count _every_ time we reach zero, either at the end of
;; the rotation or during the rotation process.



;; ### Initial solution
;;
;; We could do this by making one by one step of each rotation and checking
;; if we've hit zero. This is doable since all numbers in the input are
;; smaller than 1000, but I haven't realized that when I was solving it so I
;; opted for a "smarter" solution. Which turned out to be not-so-smart when
;; I've hit several edge cases which produced a wrong result.

(defn part-2 [data]
  (-> (reduce (fn [[zeros start] n]
                (let [end (+ start n)]
                  [(+ zeros
                      (abs (quot end size))        ; [1]
                      (if (>= start 1 0 end) 1 0)) ; [2]
                   (mod end size)]))               ; [3]
              [0 start]
              data)
      first))                                      ; [4]


;; If the result of a turn (in absolute value) is higher than 100, e.g. -765, it means
;; that we crossed zero at least 7 times [1].\
;; The condition [2] covers cases where we start from a positive starting position
;; (`(>= start 1)`) and we either stop at or pass through zero (`(>= 0 end)`).
;; If we started from a zero, we don't cross it (and we counted it already on
;; a previous turn).\
;; Our end position on the dial is always positive [3].
;;
;; Once we finish rotating the dial, as in the first part, we're only interested
;; in the number of times we've seen zero [4].

(part-2 example-data)
(part-2 data)


;; Congrats! Day 1 solved!





;; ### Step-by-step solution
;;
;; This is a simpler solution with a smaller (but still non-zero :')) possibility of
;; hitting some edge case, but I initially opted for the solution written above.
;; (And had 4 wrong answers in the process. But don't tell anybody.)
;;
;; First, we'll convert the data we have (a list of rotations) into a list
;; of step-by-step movements.\
;; Here the [`mapcat` function](https://clojuredocs.org/clojure.core/mapcat) comes
;; handy: we create a flat list, instead of a nested one.

(defn convert-data [data]
  (mapcat (fn [n]
            (repeat (abs n) (if (pos? n) 1 -1)))
          data))

;; This is how the converted data looks like:

(convert-data [-3 2 -1 4])



;; With the data converted into the list of single steps, we can now make each step
;; with the [`reductions` function](https://clojuredocs.org/clojure.core/reductions).
;; This function, unlike `reduce` which produces just the final value, keeps all
;; immediate values. Just what we need here.
;;
;; The only thing remaining is to count how many times we've seen zero.\
;; I'm doogfeeding by using the
;; [`aoc/count-if` function](https://narimiran.github.io/aoc-utils/aoc-utils.core.html#var-count-if),
;; but the same can be achieved by using `filter` and then `count`.

(defn part-2-simpler [data]
  (->> data
       convert-data
       (reductions (fn [acc n]
                     (mod (+ acc n) size))
                   start)
       (aoc/count-if zero?)))

(part-2-simpler example-data)
(part-2-simpler data)






;; ### Performance comparison
;;
;; Ok, the inital solution is more complicated. Is it at least faster than the
;; simple solution?
;;
;; To produce meaningful results, we'll use the
;; [`criterium` library](https://github.com/hugoduncan/criterium).

;; ```clj
;; (require '[criterium.core :as c])
;;
;; (c/quick-bench (part-2 data))
;;
;; Evaluation count : 4788 in 6 samples of 798 calls.
;;              Execution time mean : 137.994088 µs
;;     Execution time std-deviation : 16.934511 µs
;;    Execution time lower quantile : 126.338762 µs ( 2.5%)
;;    Execution time upper quantile : 163.531903 µs (97.5%)
;;                    Overhead used : 1.834727 ns
;;
;;
;; (c/quick-bench (part-2-simpler data))
;;
;; Evaluation count : 12 in 6 samples of 2 calls.
;;              Execution time mean : 71.898534 ms
;;     Execution time std-deviation : 5.535438 ms
;;    Execution time lower quantile : 67.693688 ms ( 2.5%)
;;    Execution time upper quantile : 80.372340 ms (97.5%)
;;                    Overhead used : 1.834727 ns)
;; ```
;;
;; Even though all turns are small (smaller than 1000), there is ~500x
;; performance difference between two solutions.
;; To be honest, I didn't expect such a large difference.
;;
;; Even if we `convert-data` beforehand (so we don't measure it), the
;; difference is still ~300x.






;; ## Both parts at once
;;
;; If we take a look and compare our `part-1` and `part-2` functions, we'll
;; notice they are basically the same, effectively only one line
;; (how to count zeros) is different.
;; Let's make a function which solves both parts at once:

(defn both-parts [data]
  (-> (reduce (fn [[start pt-1 pt-2] n]
                (let [end  (+ start n)
                      end' (mod end size)]
                  [end'
                   (if (zero? end') (inc pt-1) pt-1)
                   (+ pt-2
                      (abs (quot end size))
                      (if (>= start 1 0 end) 1 0))]))
              [start 0 0]
              data)
      rest))

(both-parts example-data)
(both-parts data)





;; ## Conclusion
;;
;; This was much harder than expected for Day 1, and I'm including there the
;; input too, which was not just numbers (so it would've been easier to parse
;; for beginners).\
;; The advice for myself would be not to try to come up with clever solutions
;; at 6 a.m., especially for these early days.
;; But at least we've got some nice performance boost out of it.
;;
;; Today's highlights:
;; - `subs`: take a substring
;; - `mapcat`: flatten what would be a nested list
;; - `reductions`: keep immediate values of a reduction


;; ----
;;
;; [Source code](https://github.com/narimiran/aoc2025/blob/main/clojure/src/day01.clj)
;; | [Next solution >](day02)



^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(defn -main [input]
  (let [data (parse-data input)]
    (both-parts data)))
