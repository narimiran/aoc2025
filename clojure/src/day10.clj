^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns day10
  {:title "Factory"
   :url "https://adventofcode.com/2025/day/10"
   :extras ""
   :highlights "keep, distinct, juxt, frequencies, group-by"
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
;; that it is `false` and not nil`; can't use set `#{\#}` as a predicate.)
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
  (loop [[[lights buttons presses] & states'] (list [goal buttons 0])
         best-result 999999]
    (cond
      (nil? lights)          best-result              ; [1]
      (every? false? lights) (recur states' presses)  ; [2]

      (and (seq buttons)                              ; [3]
           (< (inc presses) best-result))             ; [4]
      (let [[button & buttons'] buttons
            lights'             (toggle lights button)]
        (recur (conj states'
                     [lights' buttons' (inc presses)] ; [5]
                     [lights  buttons' presses])      ; [6]
               best-result))

      :else (recur states' best-result))))            ; [7]


;; Since going from lights off to lights on is the same as going from lights
;; on to lights off, we will start with the `goal` and stop when we reach
;; a state with all lights turned off.
;;
;; We will exit the loop when there are no more states to explore [1].
;;
;; If the current `lights` are all off, we've
;; found a new best result (we will see in a minute why this is true), and
;; we continue exploring the rest of the states in attempt to find an even
;; better result [2].
;;
;; If there are still `buttons` to press [3] and if there's a chance we
;; could improve the best result so far [4], we have two options:
;; - [5] We press the current `button`, toggling lights to their new state
;;   (`lights'`).
;; - [6] We skip pressing the current button. The `lights` remain as they
;;   were. (Notice the lack of `'`.)
;;
;; We add both those scenarios to the `states'` we wish to explore next with
;; the remaining `buttons'`.
;;
;; Otherwise, we explore the remaining `states'` to see if we can improve
;; the best result [7].

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
;; I won't repeat in detail what was said in the linked post, you should
;; definitely read it.
;;
;; We're not interested anymore in just the best score with the fewest presses
;; for a goal we want to achieve: for each machine we will calculate _all_
;; possible subsets of (not) pressing each button:

(defn all-subsets [buttons]
  (loop [subsets [[]]
         [button & buttons'] buttons]
    (if (nil? button)
      subsets
      (let [subsets' (map #(conj % button) subsets)]
        (recur (into subsets subsets') buttons')))))

(all-subsets [1 2 3])



;; For each button-pressing combination, we want to know two things:
;; - how much each joltage has changed
;; - how many buttons we've pressed
;;
;; To get a vector with those things, we will have to apply two different
;; functions to the button-combination, and for that we can use
;; the [`juxt` function](https://clojuredocs.org/clojure.core/juxt).

(def press-results (juxt (comp frequencies flatten) count))

;; The first result is achieved by [`flatten`ing](https://clojuredocs.org/clojure.core/flatten)
;; the buttons presses and then calculating the
;; [`frequencies`](https://clojuredocs.org/clojure.core/frequencies).\
;; The second result is just the `count` of buttons pressed.
;;
;; Here's an example where we press three buttons:

(let [pressed-buttons [[0] [0 1] [0 1 2]]]
  (press-results pressed-buttons))

;; We get the vector with two results we wanted. The first element is a hashmap
;; telling us how much the joltage has been change at each index:
;; index 0 by 3, index 1 by 2, index 2 by 1.



;; Different button combinations can produce different joltages,
;; but the same light parity:

(defn light-parity [joltages]
  (->> joltages
       (keep (fn [[k v]] (when (odd? v) k)))
       set))

(= (light-parity {0 3 , 1 2 , 2 1})
   (light-parity {0 11 , 1 0 , 2 33}))



;; We will [`group-by`](https://clojuredocs.org/clojure.core/group-by) the
;; results by `light-parity`.

(defn all-states [buttons]
  (->> buttons
       all-subsets
       (map press-results)
       (group-by (comp light-parity first)) ; [1]
       (#(update-vals % distinct))))        ; [2]

;; The result of `press-results` is a vector: we need its first element
;; (joltages) to calculate `light-parity` [1].\
;; There will be some duplicated results (we make the same joltage changes
;; with two different combinations of buttons, with the same number of
;; button presses), so we will keep only
;; the [`distinct`](https://clojuredocs.org/clojure.core/distinct) values [2].


(let [[_ buttons _] (first example-data)]
  (all-states buttons))

;; This result might look like a mess, but it is something we can work with.
;; And we're close to the finish line.
;;
;; So we have a vector of initial joltages that looks like `[3 5 4 7]`
;; and for some button combination we now have a vector with a
;; hashmap of changed joltages and the number of button presses,
;; like `[{1 2, 3 1, 0 2, 2 1} 3]`.
;;
;; We'll need to calculate a `new-state` with the remaining joltages:

(defn new-state [joltages [deltas presses]]
  (let [joltages' (reduce-kv (fn [acc idx v]
                                (update acc idx - v)) ; [1]
                             joltages
                             deltas)]
    (when (not-any? neg? joltages')                   ; [2]
      [(mapv #(quot % 2) joltages')                   ; [3]
       presses])))

;; To calculate the remaining `joltages'` of each light at an index `idx`,
;; we will subtract the amount we've just pressed at the same index [1].
;;
;; If any of the new light joltages is negative, it means we've reached
;; an illegal state and we'll return `nil` [2].
;; Otherwise, we still need to know the number of button `presses`, and we
;; prepare our next state by dividing the new joltages by two.
;; (Why? It is explained in the linked insight.)
;;
;; Here's an example:

(let [joltages [3 5 4 7]
      results [{1 2, 3 1, 0 2, 2 1} 3]]
  (new-state joltages results))




;; How do we get from the initial state to the wanted result? By using
;; recursion and dividing the problem until we come to the end:

(defn press-buttons-2 [states joltages]
  (if (every? zero? joltages) 0                                  ; [1]
      (let [parity (light-parity (map-indexed vector joltages))] ; [2]
        (->> (states parity)                                     ; [3]
             (keep #(new-state joltages %))                      ; [4]
             (map (fn [[joltages' presses]]                      ; [5]
                    (+ presses
                       (* 2 (press-buttons-2 states joltages')))))
             (reduce min 100000)))))                             ; [6]

;; If our recursion reached the state where every remaining joltage is zero,
;; there's no more presses to be made and we return zero [1].\
;; Otherwise, we calculate the `light-parity` of the current `joltages`.
;; That function expects a hashmap: a vector with `[index value]` pairs
;; can disguise as one [2].
;;
;; We are interested only in those `states` with the current `parity` [3].
;; For each state we calculate the `new-state` and then use the
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
  (aoc/sum-pmap (fn [[_ buttons joltages]]
                  (press-buttons-2 (all-states buttons) joltages))
                data))

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
;; we know how to solve.
;;
;; Today's highlights:
;; - `keep`: return non-nil results of applying a function to the elements of
;;   a collection
;; - `distinct`: remove duplicates in a collection
;; - `juxt`: create a vector of applying different functions to an argument
;; - `frequencies`: count the appearances of elements in a collection
;; - `group-by`: group elements of a collection by the result of a funciton


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
