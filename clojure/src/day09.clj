^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns day09
  {:title "Movie Theater"
   :url "https://adventofcode.com/2025/day/9"
   :extras ""
   :highlights "every?, pmap, ffirst"
   :remark "The hardest one so far."
   :nextjournal.clerk/auto-expand-results? true
   :nextjournal.clerk/toc true}
  (:require [aoc-utils.core :as aoc]))




;; # Day 9: [Movie Theater](https://adventofcode.com/2025/day/9)
;;
;; We're in a movie theater and instead of watching Die Hard, we need to help
;; Elves with floor decorations. There are some red tiles at the following
;; coordinates:

(def example "7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3")




;; ## Input parsing
;;
;; Each line represents x and y coordinates of one tile. We've already
;; parsed stuff like that [yesterday](../day08) so nothing new here:

(defn parse-data [input]
  (aoc/parse-lines input :ints))

(def example-data (parse-data example))
(def data (parse-data (aoc/read-input 9)))



;; ## Part 1
;;
;; Our first task is to find the area of the largest rectangle whose two
;; diagonal points are the red tiles we've just parsed.
;; Since we can see the future and know what Part 2 brings, we'll not just
;; calculate the largest area, but also do some additional work.
;; For each pair of points, we will create a vector of four values:
;; `[min-x max-x min-y max-y]`, as we'll need that later.

(defn create-box [[ax ay] [bx by]]
  [(min ax bx) (max ax bx) (min ay by) (max ay by)])


;; From that vector, we can easily calculate the `area`:

(defn area [[x1 x2 y1 y2]]
  (* (inc (- x2 x1))
     (inc (- y2 y1))))


;; Now we'll create a list of all rectangles sorted by their area.

(defn largest-rectangles [pts]
  (->> (for [a pts
             b pts
             :while (not= a b)
             :let [box (create-box a b)]]
         [(area box) box])       ; [1]
       (sort (comp - compare)))) ; [2]

;; For each rectangle, we want to know its `area` and the minimal and maximal
;; values for each coordinate [1].\
;; To sort the results in the descending order we negate the result of
;; [`compare`](https://clojuredocs.org/clojure.core/compare) [2].
;;
;; Now, the solution for Part 1 is the first element of this sorted list.
;; But we'll do it later, in the same function we'll use to solve Part 2.






;; ## Part 2
;;
;; In this part we need to find the largest rectangle which is fully contained
;; inside of the polygon whose coordinates the Elves gave us.
;;
;; Initially I wrote a solution which involved compressing the coordinates
;; of a polygon, calculating all points which are inside of the polygon
;; (in a quite convoluted way), and then for every possible rectangle check
;; if all of its points (both on the edges and vertices and inside of it) are
;; contained in the set of points inside of the polygon.\
;; If it sounds complicated, just know that it was _more_ complicated than
;; it sounds. :')
;;
;; It turns out there is a much simpler way.
;;
;; For each rectangle box we calculated earlier, we need to check if
;; a polygon line is slicing through it.

(defn not-slicing? [[p-x1 p-x2 p-y1 p-y2] [r-x1 r-x2 r-y1 r-y2]]
  (or (<= p-x2 r-x1)   ; polygon line completely on the left
      (>= p-x1 r-x2)   ; polygon line completely on the right
      (<= p-y2 r-y1)   ; polygon line completely above
      (>= p-y1 r-y2))) ; polygon line completely below

;; If a rectangle is `inside?` of a polygon, that means that
;; [`every?`](https://clojuredocs.org/clojure.core/every_q)
;; line of a polygon is `not-slicing?` it.

(defn inside? [polygon-boxes rect]
  (every? (fn [box]
            (not-slicing? box rect))
          polygon-boxes))


;; And that's it. That's all we need to solve the problem.

(defn solve [polygon]
  (let [rectangles (largest-rectangles polygon)
        polygon' (conj polygon (first polygon))                  ; [1]
        polygon-lines (map create-box polygon' (rest polygon'))] ; [2]
    [(ffirst rectangles)          ; [3]
     (->> rectangles
          (pmap (fn [[area rect]] ; [4]
                  (when (inside? polygon-lines rect)
                    area)))
          (some identity))]))     ; [5]

;; To "close" the polygon, we add its first point to the end [1].
;;
;; We'll transform each polygon line with `create-box`, which will
;; automatically take care of dealing with lines being horizontal or
;; vertical [2]:

(let [[a b c] example-data]
  [(create-box a b)
   (create-box b c)])

;; We've created a sorted list of `largest-rectangles`.
;; The largest rectangle is the first element of it. Its area
;; is the first element of that first element. To get that (which is the
;; solution for Part 1) we can use the
;; [`ffirst` function](https://clojuredocs.org/clojure.core/ffirst) [3].
;;
;; We will use that sorted list of largest rectangles to find the first one
;; which is completely inside of the polygon.\
;; We could use `filter`, but we will take an advantage of modern hardware
;; and do this in parallel with the
;; [`pmap` function](https://clojuredocs.org/clojure.core/pmap) [4].\
;; We are interested in the first truthy value and we can get it with
;; `(some identity coll)` [5].

(solve example-data)
(solve data)




;; ## Conclusion
;;
;; This one was the hardest one for me so far this year.\
;; It took me a while to come up with a way to check if a rectangle is inside
;; of a polygon. And then, it turns out that was an overkil and there is
;; a much simpler solution possible.
;;
;; Today's highlights:
;; - `every?`: is a predicate true for every element of a collection?
;; - `pmap`: map in parallel
;; - `ffirst`: first element of first element


;; ----
;;
;; [< Previous solution](../day08)
;; | [Source code](https://github.com/narimiran/aoc2025/blob/main/clojure/src/day09.clj)
;; | [Next solution >](../day10)



^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(defn -main [input]
  (let [data (parse-data input)]
    (solve data)))
