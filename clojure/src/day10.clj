^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns day10
  {:title "Factory"
   :url "https://adventofcode.com/2025/day/10"
   :extras ""
   :highlights "constantly, cond->, keep"
   :remark "Divide and conquer."
   :nextjournal.clerk/auto-expand-results? true
   :nextjournal.clerk/toc true}
  (:require [aoc-utils.core :as aoc]))




;; # Day 10: [Factory](https://adventofcode.com/2025/day/10)
;;
;; We're in a factory and we're given the remains of the manual for the machines
;; there. Each machine is on its own line in the manual, and that lines contains
;; indicator lights diagram we need to reach (inside of `[ ... ]`), button
;; wiring schematics (inside of multiple `( ... )`) and some _joltage_
;; requirements (inside of `{ ... }`). It looks like this:

(def example "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}")




;; ## Input parsing
;;
;; To parse a line, we could use regex. The pattern is quite simple and readable:
;; `#"\[([^]]+)\]|\(([^)]+)\)+|\{([^}]+)\}"`, it is easy for everybody to
;; understand this, no need to explain it.
;;
;; Or we could do it differently.

(defn parse-line [line]
  (let [[lights & tl] (aoc/parse-input line :words)        ; [1]
        lights (mapv #(= % \#)
                     (subs lights 1 (dec (count lights)))) ; [2]
        joltages (aoc/integers (last tl))                  ; [3]
        buttons (mapv aoc/integers (butlast tl))]          ; [4]
    [lights buttons joltages]))

;; We split the line on all whitespace characters. The first element are the
;; `lights` and the rest (`tl`) contains both `buttons` and `joltages` [1].
;; Unfortunately, we can't destructure like in Python with `a, *bs, c = some_list`,
;; i.e. extracting the first (`a`) and the last (`c`) element, and then
;; everything in between is put in `bs`.
;;
;; To convert the `lights` into something we can use later, we remove the first and
;; the last character (`[` and `]`, respectively), and the rest we convert to
;; a vector where `#` gets a `true` value and `.` is `false` [2]. (It's important
;; that it is `false` and not `nil`; can't use set `#{\#}` as a predicate.)
;;
;; For `joltages`, we just need to extract all integers from the last element [3].
;; We do the same thing for _each_ `button` [4].
;;
;; The result of this looks like this:

(parse-line "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}")



;; To parse the input, we split it into lines and process each line using
;; the `parse-line` function above.

(defn parse-data [input]
  (aoc/parse-lines input parse-line))

(def example-data (parse-data example))

^{:nextjournal.clerk/auto-expand-results? false}
(def data (parse-data (aoc/read-input 10)))






;; ## Part 1
;;
;; For each machine we need to find the fewest total presses of the buttons
;; needed to create the state of the lights in the manual from a starting state
;; where all lights are off.
;;
;; Each button toggles the lights at the indices indicated. E.g. a button
;; `[1 3]` toggles the lights and indices 1 and 3.\
;; We can `reduce` over the current `state` and for each index `idx` in
;; the `button`, we flip the switch with the
;; [`not` function](https://clojuredocs.org/clojure.core/not):

(defn toggle [state button]
  (reduce (fn [state idx] (update state idx not)) state button))

;; For example:

(let [state (vec (repeat 5 false))
      button [0 2 3]]
  (toggle state button))


;; The key insight for solving the task is that we should press each button
;; either zero times or one time at most. There's no need to press it more
;; (like it is shown in the task example to throw us off), as pressing two times
;; is the same as not pressing at all, etc.

(let [state (vec (repeat 5 false))
      button [0 2 3]]
  (= state
     (-> state
         (toggle button)
         (toggle button))))


;; For each machine we will go button by button, and for each button we have an
;; option to press it or not. Meaning if we have `n` buttons, there will be
;; `2^n` states to explore. Fortunately, the largest amount of buttons in
;; our input is 13 so this won't explode in our face. Even still, we can
;; easily reduce the search space by dismissing the states which won't
;; improve the best result so far.


(defn press-buttons [[goal buttons _]]
  (let [initial-lights (mapv (constantly false) goal) ; [1]
        initial-state [initial-lights buttons 0]]     ; [2]
    (loop [[[lights buttons presses] & states'] (list initial-state)
           best-result 999999]
      (cond
        (nil? lights)   best-result             ; [3]
        (= lights goal) (recur states' presses) ; [4]

        (and (seq buttons)                      ; [5]
             (< (inc presses) best-result))     ; [6]
        (let [[button & buttons'] buttons
              lights'             (toggle lights button)]
          (recur (conj states'
                       [lights' buttons' (inc presses)] ; [7]
                       [lights  buttons' presses])      ; [8]
                 best-result))

        :else           (recur states' best-result))))) ; [9]


;; When we start, all lights are off. One way to create a vector of initial
;; lights with the same size as our `goal` is to use the
;; [`constantly` function](https://clojuredocs.org/clojure.core/constantly)
;; which returns a function which will always return the provided argument [1].\
;; The initial state consists of all lights turned off, all buttons ready
;; to be pressed, and zero button presses so far [2].
;;
;; We will exit the loop when there are no more states to explore [3].
;;
;; If the current `lights` match the `goal` we're trying to achieve, we've
;; found a new best result (we will see in a minute why this is true), and
;; we continue exploring the rest of the states in attempt to find an even
;; better result [4].
;;
;; If there are still `buttons` to press [5] and if there's a chance we
;; could improve the best result so far [6], we have two options:
;; - [7] We press the current `button`, toggling lights to their new state
;;   (`lights'`).
;; - [8] We skip pressing the current button. The `lights` remain as they
;;   were. (Notice the lack of `'`.)
;;
;; We add both those scenarios to the `states'` we wish to explore next with
;; the remaining `buttons'`.
;;
;; Otherwise, we explore the remaining `states'` to see if we can improve
;; the best result [9].

(press-buttons (first example-data))



;; All that is left to do is to do this for every machine and then take the
;; sum of all results. We already know from the earlier tasks that the
;; [`aoc/sum-map` function](https://narimiran.github.io/aoc-utils/aoc-utils.core.html#var-sum-map)
;; does exactly that.

(defn part-1 [data]
  (aoc/sum-map press-buttons data))


(part-1 example-data)
(part-1 data)








;; ## Part 2
;;
;; Thanks to [this brilliant insight](https://old.reddit.com/r/adventofcode/comments/1pk87hl/2025_day_10_part_2_bifurcate_your_way_to_victory/),
;; there is a relatively easy solution for Part 2, which we will implement here.\
;; I wont't repeat in detail what was said in the linked post, you should
;; definitely read it.
;;
;; For this to work, we need to modify the `press-buttons` function to not
;; just search for the best best result, but to give _all_ button `presses`
;; which would give the `goal` state.

(def press-buttons'
  (memoize
   (fn [buttons goal]
     (let [initial-lights (mapv (constantly false) goal)
           initial-state [initial-lights buttons []]]        ; [1]
       (loop [[[lights buttons presses] & states'] (list initial-state)
              results #{}]                                   ; [2]
         (let [results' (cond-> results
                          (= lights goal) (conj presses))]   ; [3]
           (cond
             (nil? lights) results ; [4]

             (seq buttons)         ; [5]
             (let [[button & buttons'] buttons
                   lights'             (toggle lights button)
                   presses'            (conj presses button)]
               (recur (conj states'
                            [lights' buttons' presses']
                            [lights  buttons' presses])
                      results'))

             :else (recur states' results'))))))))


;; We now track not just the number of button presses, but we need to know
;; exactly what buttons were pressed [1].\
;; The `results` will be a set of all button-pressing combinations which
;; give us the wanted solution [2]. When we weach the `goal`, we add the
;; current `presses` to the `results` [3].
;; To conditionally update `results`, only if we reached the `goal`, we can
;; use the [`cond->` macro](https://clojuredocs.org/clojure.core/cond-%3E).
;;
;; The exit condition is the same as before: when there are no more states
;; to explore [4].\
;; The line [5] is the key difference: we're not interested in just the best
;; result (with the fewest button presses), we will continue exploring the
;; states for _every_ button (not) pressed.
;;
;; For example, for the first line in our example:

(let [[goal buttons _] (first example-data)]
  (press-buttons' buttons goal))


;; So we have a way to solve a single recursion step, and now we need
;; to find a way to go to a new state for the next recursion step.

(defn new-state [joltages presses]
  (let [joltages' (reduce #(update %1 %2 dec) ; [1]
                          joltages
                          (flatten presses))] ; [2]
    (when (not-any? neg? joltages')           ; [3]
      [(mapv #(quot % 2) joltages')           ; [4]
       (count presses)])))

;; For each result we will calculate the remaining `joltages'` of each light
;; by `dec`reasing the current joltage of that light every time a button
;; has modified it [1]. To convert a nested list of button-presses to
;; a flat list of light-modifications, we use the
;; [`flatten` function](https://clojuredocs.org/clojure.core/flatten) [2].
;;
;; If any of the new light joltages is negative, it means we've reached
;; an illegal state and we'll return `nil` [3].
;; Otherwise, we need to know the `count` of button presses, and we
;; prepare our next state by dividing the new joltages by two.
;; (Why? It is explained in the linked insight.)
;;
;; Here's an example:

(let [[_ buttons joltages] (first example-data)
      goal (mapv odd? joltages)]
  (->> (press-buttons' buttons goal)
       (mapv #(new-state joltages %))))



;; Time to put it all together:

(def press-buttons-2
  (memoize
   (fn [buttons joltages]
     (if (every? zero? joltages) 0           ; [1]
         (->> joltages
              (map odd?)                     ; [2]
              (press-buttons' buttons)       ; [3]
              (keep #(new-state joltages %)) ; [4]
              (map (fn [[joltages' presses]] ; [5]
                     (+ presses
                      (* 2 (press-buttons-2 buttons joltages')))))
              (reduce min 100000))))))       ; [6]

;; If our recursion reached the state where every remaining joltage is zero,
;; there's no more presses to be made and we return zero [1].\
;; Otherwise, our current `goal` is defined by parity of the current joltages [2].
;; We press the buttons to reach that goal [3] and then use the
;; [`keep` function](https://clojuredocs.org/clojure.core/keep) to
;; return only valid new states [4], as defined above.
;;
;; For each new state we re-enter the recursion and the total number of
;; button presses for a current state is calculated as a sum of current
;; button presses and twice the button presses of a new state. [5]
;; (You know why twice because you've already read the linked insight.)
;;
;; In the end, we need to find the minimum amount of button presses of all
;; the states explored [6].


(defn part-2 [data]
  (aoc/sum-pmap (fn [[_ b j]] (press-buttons-2 b j)) data))

;; All that is left to do is to call the above function for each row of
;; our input and sum the results. To do this in a bit less time, we will
;; use the [`aoc/sum-pmap` function](https://narimiran.github.io/aoc-utils/aoc-utils.core.html#var-sum-pmap),
;; which uses the [`pmap` function](https://clojuredocs.org/clojure.core/pmap)
;; to apply the function in parallel.

(part-2 example-data)
(part-2 data)


;; Wow, this was an interesting journey!



;; ## Conclusion
;;
;; Part 1 was a nice and relatively easy task.
;;
;; Part 2 uses the [brilliant idea](https://old.reddit.com/r/adventofcode/comments/1pk87hl/2025_day_10_part_2_bifurcate_your_way_to_victory/)
;; on how to approach this task and recursively split it into smaller tasks
;; we know how to solve (from Part 1).
;;
;; Today's highlights:
;; - `constantly`: create a function that always returns the same value
;; - `cond->`: conditionally update an expression
;; - `keep`: return non-nil results of applying a function to the elements of
;;   a collection


;; ----
;;
;; [< Previous solution](../day09)
;; | [Source code](https://github.com/narimiran/aoc2025/blob/main/clojure/src/day10.clj)
;; | [Next solution >](../day11)



^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(defn -main [input]
  (let [data (parse-data input)]
    [(part-1 data)
     (part-2 data)]))
