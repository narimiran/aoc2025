^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns day12
  {:title "Christmas Tree Farm"
   :url "https://adventofcode.com/2025/day/12"
   :extras ""
   :highlights ""
   :remark "The most disappointing AoC task ever?"
   :nextjournal.clerk/auto-expand-results? true
   :nextjournal.clerk/toc true}
  (:require [aoc-utils.core :as aoc]))




;; # Day 12: [Christmas Tree Farm](https://adventofcode.com/2025/day/12)
;;
;; There are some presents to be given to young Elves and we need to find
;; if they fit in given regions. The situation looks like this:

(def example "0:
###
##.
##.

1:
###
##.
.##

2:
.##
###
##.

3:
##.
###
##.

4:
###
#..
###

5:
###
.#.
###

4x4: 0 0 0 0 2 0
12x5: 1 0 1 0 2 2
12x5: 1 0 1 0 3 2")

;; ...and the way the real input is created we can ignore all but the last
;; paragraph, which tells us the size of a region and an amount of each present
;; that we should try to fit in that region.



;; ## Input parsing
;;
;;  To split the input into paragraphs, we have the
;; [`aoc/parse-paragraphs` function](https://narimiran.github.io/aoc-utils/aoc-utils.core.html#var-parse-paragraphs)
;; ready.\
;; We will extract integers from the last paragraph.

(defn parse-data [input]
  (last (aoc/parse-paragraphs input :ints)))


(def example-data (parse-data example))
(def data (parse-data (aoc/read-input 12)))





;; ## Solution
;;
;; The solution which works for the real input is very simple. Maybe
;; even too simple.\
;; Every present has a 3x3 shape, i.e. its area is 9.
;; We won't try anything fancy and we'll just check if we can fit every
;; present in its own 3x3 area.
;;
;; Each line of the parsed input contains multiple integers. The first two
;; are the `w`idth and the `h`eight of a region, and the rest are the
;; `amounts` of each present.\
;; The presents fit if their total area is not larger than the area of
;; a region.

(defn fits? [[w h & amounts]]
    (<= (* 9 (reduce + amounts)) (* w h)))

;; And to solve the task, we just count those lines that satisfy the
;; above condition.

(defn solve [measures]
  (aoc/count-if fits? measures))

(solve data)

;; That's it.
;;
;; That's it?!





;; ## Conclusion
;;
;; I usually like easy tasks. But this one wasn't easy, it was lame.\
;; What a disappointing way to end what was an interesting AoC year.
;;
;; No highlights today.


;; ----
;;
;; [< Previous solution](../day11)
;; | [Source code](https://github.com/narimiran/aoc2025/blob/main/clojure/src/day12.clj)
;; | [Next solution >](../day13)



^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(defn -main [input]
  (let [data (parse-data input)]
    (solve data)))
