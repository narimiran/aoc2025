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
;; diagonal points are the red tiles we've just parsed. This means that if
;; we have the coordinates of those two points, we can easily
;; calculate the `area`:

(defn area [[ax ay] [bx by]]
  (* (inc (abs (- ax bx)))
     (inc (abs (- ay by)))))


;; Since we can see the future and know what Part 2 brings, we'll not just
;; calculate the largest area, but we'll make a list of all rectangles sorted
;; by their area.

(defn largest-rectangles [pts]
  (->> (for [a pts
             b pts
             :while (not= a b)]
         [(area a b) a b])       ; [1]
       (sort (comp - compare)))) ; [2]

;; For each rectangle, we want to know its area and the points `a` and `b`
;; which form it [1].\
;; To sort the results in the descending order we negate the result of
;; [`compare`](https://clojuredocs.org/clojure.core/compare) [2].
;;
;; Now, the solution for Part 1 is the first element of this sorted list.
;; But we'll do it in the same function used to solve Part 2.




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
;; Given a rectangle defined with points `[r1 r2]`, we can check if a
;; polygon line `[p1 p2]` is going through it.

(defn not-slicing? [[p1x p1y] [p2x p2y] [r1x r1y] [r2x r2y]]
  (or (<= (max p1x p2x) (min r1x r2x))   ; polygon line completely on the left
      (<= (max r1x r2x) (min p1x p2x))   ; polygon line completely on the right
      (<= (max p1y p2y) (min r1y r2y))   ; polygon line completely above
      (<= (max r1y r2y) (min p1y p2y)))) ; polygon line completely below

;; If a rectangle is `inside?` of a polygon, that means that
;; [`every?`](https://clojuredocs.org/clojure.core/every_q)
;; line of a polygon is `not-slicing?` it.

(defn inside? [polygon-lines r1 r2]
  (every? (fn [[p1 p2]]
            (not-slicing? p1 p2 r1 r2))
          polygon-lines))


;; And that's it. That's all we need to solve the problem.

(defn solve [polygon]
  (let [rectangles (largest-rectangles polygon)
        polygon' (conj polygon (first polygon))              ; [1]
        polygon-lines (map vector polygon' (rest polygon'))] ; [2]
    [(ffirst rectangles)         ; [3]
     (->> rectangles
          (pmap (fn [[area a b]] ; [4]
                  (when (inside? polygon-lines a b)
                    area)))
          (some identity))]))    ; [5]

;; To "close" the polygon, we add its first point to the end [1] so that
;; we can create all `polygon-lines` [2].
;;
;; We create the sorted list of `rectangles`.
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
