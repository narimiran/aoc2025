^:kindly/hide-code
(ns day04
  {:title "Printing Department"
   :url "https://adventofcode.com/2025/day/4"
   :extras "bench, animation"
   :highlights "grid helpers, cond->, run!"
   :remark "The easiest one this year."}
  (:require [aoc-utils.core :as aoc]
            [quil.core :as q]
            [quil.middleware :as m]
            [scicloj.kindly.v4.kind :as kind]))




;; # Day 4: Printing Department
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

;; Every `@` is a large roll of paper (am I the only one who initially misread
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
;; and I'll link to their documentation so you can get more information there.




;; ## Input parsing
;;
;; We've already met the
;; [`aoc/parse-lines` function](https://narimiran.github.io/aoc-utils/aoc-utils.core.html#var-parse-lines)
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
;; we're interested in to their name in the produced hashmap [1]. Here we're
;; only interested in the `@` character.\
;; This function produces a hashmap with various useful keys (e.g. the size
;; of the map), but this time we're interested only in the coordinates of
;; the `:rolls` of paper, so we immediately extract that [2].

(def example-data (parse-data example))
(def data (parse-data (aoc/read-input 4)))

;; As an example, here are ten coordinates of the rolls (the full list is too long):
(take 10 example-data)





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
;; a predicate. The result of that function are rolls which are adjacent to the
;; roll we're currently exploring.\
;; The roll is accessible only if it has less than four neighbours [2].
;;
;; We can use now use this function to `filter` all rolls and keep only those
;; which are `accessible`:

(defn accessible [rolls]
  (filter #(accessible-roll? rolls %) rolls))


;; Our Part 1 task is to count all accessible rolls, and our code reads
;; exactly like that:

(defn part-1 [rolls]
  (count (accessible rolls)))


(part-1 example-data)
(part-1 data)

;; Boom, done!







;; ## Part 2
;;
;; ### Initial solution
;;
;; In Part 2 we realize that when we remove some rolls, some new ones become
;; `accessible`. We need to repeat the process until we cannot remove any more
;; rolls.

(defn remove-accessible [rolls]
  (reduce (fn [acc r]
            (cond-> acc                           ; [1]
              (accessible-roll? acc r) (disj r))) ; [2]
          rolls
          rolls))

;; Here we use the [`cond->` macro](https://clojuredocs.org/clojure.core/cond-%3E) [1],
;; but the same could have been written with `if` like this:
;;
;; ```clj
;;  (if (accessible-roll? acc r)
;;    (disj acc r)
;;    acc))
;; ```

;; We go through all of the rolls and for those which are accessible we'll remove
;; them from the `acc` set with the
;; [`disj` function](https://clojuredocs.org/clojure.core/disj) [2].



;; Now we need to repeat that until no more removals are possible.

(defn part-2 [remove-fn initial-rolls] ; [1]
  (let [n (count initial-rolls)]
    (loop [rolls initial-rolls
           c n]
      (let [rolls' (remove-fn rolls)
            c' (count rolls')]
        (if (= c c')                   ; [2]
          (- n c')                     ; [3]
          (recur rolls' c'))))))

;; Let's not hardcode the remove function [1], as there might be another, faster,
;; way of removing the elements ;) \
;; We will repeatedly remove rolls inside of `loop`. When there are no removals [2],
;; we will exit the loop and calculate the amount of rolls removed [3].


(part-2 remove-accessible example-data)
(part-2 remove-accessible data)





;; ### Remove in parallel
;;
;; Why should we go one by one roll when trying to remove them, when we could
;; start from multiple rolls in parallel?

(defn parallel-remove [rolls]
  (let [chunks (partition-all 100 rolls) ; [1]
        result (atom rolls)]             ; [2]
    (->> chunks
         (pmap #(run! (fn [r]            ; [3]
                        (when (accessible-roll? @result r)
                          (swap! result disj r))) ; [4]
                      %))
         doall) ; [5]
    @result))   ; [6]

;; We will divide the rolls into `chunks` of 100 elements each with the
;; [`partition-all` function](https://clojuredocs.org/clojure.core/partition-all) [1].\
;; Each chunk will read from and write to the same `result`, so we'll make it an
;; [`atom`](https://clojuredocs.org/clojure.core/atom) [2].
;;
;; Since we'll be updating the atom, we're not interested in the result of that
;; operation. Instead of using `map` or `reduce`, we should be using the
;; [`run!` function](https://clojuredocs.org/clojure.core/run!) [3] to go through
;; each roll in a chunk.
;;
;; Similar to our logic in the `remove-accessible` function above, once we find
;; a roll which can be removed, we need to update the `result` atom with the
;; [`swap!` function](https://clojuredocs.org/clojure.core/swap!) [4].
;;
;; Since `pmap` is lazy and we're interested in its side-effects, we need to force
;; it to run with the [`doall` function](https://clojuredocs.org/clojure.core/doall) [5].\
;; Once the job is done, we dereference the atom (using `@`) and return it [6].


;; We can use the same `part-2` function as we did originally, and we get the same
;; results:

(part-2 parallel-remove example-data)
(part-2 parallel-remove data)





;; ### Just count
;;
;; After some thinking about the performance, I found two places where we're
;; doing unnecessary work.
;;
;; In the `accessible-roll?` function we first create a vector of all neighbours,
;; just to count their amount. We can count them directly, and even exit the
;; process immediately if we already found four neighbours.

(defn accessible-roll-faster? [rolls [x y]]
  (loop [i 0
         nbs 0]
    (cond
      (= nbs 4) false      ; [1]
      (> i (+ 4 nbs)) true ; [2]
      :else
      (let [[dx dy] (aoc/offsets-8 i)]
        (recur (inc i)
               (if (rolls [(+ x dx) (+ y dy)])
                 (inc nbs)
                 nbs))))))

;; If we already found four neighbours, a roll is not accessible [1].
;; If we're checking e.g. 6th neighbouring position and we found just
;; no neighbours so far, even if all remaining positions contain a
;; neighbour the roll will still be accessible [2].



;; The second place when we were doing extra work in the original `part-2`
;; function is to have the `remove-fn` return all the rolls that remain
;; and then to count how many of them are left. And then repeat that in
;; a loop until there's no more changes at some point.
;;
;; Here's another approach: In a single loop we will track the remaining
;; `rolls` (as we need to check if a current roll is accessible), we will
;; have a "stack" of rolls we need to check, and we'll directly count
;; the amount of `removed` rolls:

(defn remove-directly [rolls]
  (loop [rolls rolls
         [r & stack'] (seq rolls)
         removed 0]
    (cond
      (nil? r) removed                                  ; [1]
      (and (rolls r) (accessible-roll-faster? rolls r)) ; [2]
      (recur (disj rolls r)
             (into stack' (aoc/neighbours-8 r rolls))   ; [3]
             (inc removed))
      :else (recur rolls stack' removed))))             ; [4]

;; If there are no more rolls in the stack, we return the amount of
;; `removed` rolls [1].
;;
;; If the current roll `r` is still in `rolls` and it is accessible [2],
;; we remove it from the `rolls`, we push the neighbours of it to the
;; stack [3] to (re-)check if they are now accessible, and increase the
;; removed count.\
;; Otherwise, we move on to the next roll on the stack.


(remove-directly example-data)
(remove-directly data)






;; ### Packed data
;;
;; Currently, every coordinate is represented by the `[x y]` vector.
;; We can, instead, use a single number in a form `(x + mult*y)`.
;;
;; My `aoc-utils` library has a
;; [`create-hashed-grid` function](https://narimiran.github.io/aoc-utils/aoc-utils.core.html#var-create-hashed-grid)
;; to do exactly that. The default multiplier for `y` coordinate is 1000, which is
;; more than enough for our small grid here.

(defn parse-packed-data [input]
  (-> input
      (aoc/parse-lines :chars)
      (aoc/create-hashed-grid {\@ :rolls}) ; [1]
      :rolls))

;; The only change from the original function is that we now use
;; `create-hashed-grid` instead of `create-grid` [1].


(def packed-data (parse-packed-data (aoc/read-input 4)))

;; Neighbours of a point are now just numbers:

(def ^:const nb-8
  [-1001 -1000 -999
      -1        1
     999  1000 1001])

(defn accessible-roll-packed? [rolls ^long pt]
  (loop [i 0
         nbs 0]
    (cond
      (= nbs 4) false
      (> i (+ 4 nbs)) true
      :else
      (let [delta (long (nb-8 i))]      ; [1]
        (recur (inc i)
               (if (rolls (+ pt delta)) ; [2]
                 (inc nbs)
                 nbs))))))

;; The only changes are the way we calculate a neighbour at lines [1] and [2].

;; We can now calculate the result for Part 1 in the same way we did earlier:

(defn part-1-packed [rolls]
  (count (filter #(accessible-roll-packed? rolls %) rolls)))

(part-1-packed packed-data)




;; For Part 2, we use the same logic as before:

(defn remove-packed [rolls]
  (loop [rolls rolls
         [r & stack'] (seq rolls)
         removed 0]
    (cond
      (nil? r) removed
      (and (rolls r) (accessible-roll-packed? rolls r))
      (recur (disj rolls r)
             (into stack' (filter rolls (map #(+ r %) nb-8)))
             (inc removed))
      :else (recur rolls stack' removed))))

(remove-packed packed-data)






;; ## Performance comparison
;;
;; Like in our [Day 1 solution](day01.html), we will compare the performance of
;; our three approaches with the
;; [`criterium` library](https://github.com/hugoduncan/criterium).

;; ```clj
;; (require '[criterium.core :as c])
;;
;; (c/quick-bench (part-2 remove-accessible data))
;; ```
;;
;; ```
;; Evaluation count : 6 in 6 samples of 1 calls.
;;              Execution time mean : 145.054417 ms
;;     Execution time std-deviation : 2.346185 ms
;;    Execution time lower quantile : 142.505658 ms ( 2.5%)
;;    Execution time upper quantile : 147.581288 ms (97.5%)
;;                    Overhead used : 1.637089 ns)
;; ```
;;
;; ```clj
;; (c/quick-bench (part-2 parallel-remove data))
;; ```
;;
;; ```
;; Evaluation count : 24 in 6 samples of 4 calls.
;;              Execution time mean : 35.226995 ms
;;     Execution time std-deviation : 7.882422 ms
;;    Execution time lower quantile : 28.640494 ms ( 2.5%)
;;    Execution time upper quantile : 45.392904 ms (97.5%)
;;                    Overhead used : 1.637089 ns)
;; ```

;; About 4x speedup! Not bad for a small difference: we just changed
;; the function which removes the rolls and took the advantage of parallelization
;; on modern hardware.

;; ```clj
;; (c/quick-bench (remove-directly data))
;; ```
;;
;; ```
;; Evaluation count : 30 in 6 samples of 5 calls.
;;              Execution time mean : 21.649870 ms
;;     Execution time std-deviation : 561.582719 µs
;;    Execution time lower quantile : 21.116228 ms ( 2.5%)
;;    Execution time upper quantile : 22.291778 ms (97.5%)
;;                    Overhead used : 1.637089 ns)
;; ```
;;
;; Even without any parallelization, this version gains another 1.5x
;; speedup.


;; ```clj
;; (c/quick-bench (remove-packed packed-data))
;; ```
;;
;; ```
;; Evaluation count : 84 in 6 samples of 14 calls.
;;              Execution time mean : 7.297688 ms
;;     Execution time std-deviation : 112.757439 µs
;;    Execution time lower quantile : 7.176062 ms ( 2.5%)
;;    Execution time upper quantile : 7.458879 ms (97.5%)
;;                    Overhead used : 1.637089 ns)
;; ```
;;
;; Another 3x speedup compared to the next-best solution.
;; Making it 20x faster than the initial solution.









;; ## Animation
;;
;; This task is ideal to make an animation of how the forklifts are removing
;; the rolls.
;;
;; As usual (if you're interested, see the
;; [visualizations I did for AoC 2016](https://github.com/narimiran/advent_of_code_2016?tab=readme-ov-file#visualizations)),
;; I'm using the [Quil library](http://quil.info) to create animations.
;;
;; I won't be explaining what each part of the code does, but here it is in
;; its entirety so you can experiment with it yourself.


(defn build-states [rolls]
  (loop [accessible-states [[]]
         rolls rolls]
    (let [to-remove (accessible rolls)]
      (if (empty? to-remove)
        accessible-states
        (recur (conj accessible-states to-remove)
               (reduce disj rolls to-remove))))))

(defn draw-rolls [rolls]
  (doseq [[x y] rolls]
    (q/ellipse x y 0.9 0.9)))

(defn setup []
  (q/scale 7)
  (q/frame-rate 8)
  (q/no-stroke)
  (q/background 15 15 33)
  (q/fill 150)
  (q/ellipse-mode :corner)
  (draw-rolls data)
  (q/delay-frame 500)
  (build-states data))


(defn draw-state [state]
  (when (empty? state)
    (q/delay-frame 2000)
    (q/exit))
  (q/fill 255 255 102)
  (q/scale 7)
  (draw-rolls (first state))
  #_(q/save-frame "/tmp/imgs/day04-####.jpg"))


(comment
  (q/sketch
   :size [960 960]
   :setup #'setup
   :update rest
   :draw #'draw-state
   :middleware [m/fun-mode]))

;; To make a video from the frames created above, I've used the following command:\
;; `ffmpeg -framerate 8 -i /tmp/imgs/day04-%04d.jpg -vf tpad=start_duration=1:start_mode=clone:stop_mode=clone:stop_duration=2 -c:v libx264 -pix_fmt yuv420p imgs/day04.mp4`
;;
;; And the result is:

(kind/hiccup
 ^:kindly/hide-code
 [:video {:controls true}
  [:source {:src "https://i.imgur.com/UKHxdoR.mp4"
            :type "video/mp4"}]
  "Your browser does not support the video tag."])






;; ## Conclusion
;;
;; This was maybe the easiest one this year. Especially if you have grid-helpers
;; ready. But, knowing AoC, this usually means something difficult is coming
;; up very soon.\
;; And we can expect harder grid-based task(s) later on.
;;
;; We took the opportunity and made an animation of roll-removal.
;;
;;
;; Today's highlights:
;; - `aoc/create-grid`: helper for tasks like this one
;; - `aoc/neighbours-8`: get 8 neighbours of a point which satisfy a predicate
;; - `cond->`: conditional threading
;; - `run!`: run a function on each element for its side-effects



^:kindly/hide-code
(defn -main [input]
  (let [data (parse-packed-data input)]
    [(part-1-packed data)
     (remove-packed data)]))
