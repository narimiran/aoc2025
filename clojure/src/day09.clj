^:kindly/hide-code
(ns day09
  {:title "Movie Theater"
   :url "https://adventofcode.com/2025/day/9"
   :extras ""
   :highlights "every?, peek"
   :remark "The hardest one so far."}
  (:require [aoc-utils.core :as aoc]))




;; # Day 9: Movie Theater
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
;; parsed stuff like that [yesterday](day08.html) so nothing new here:

(defn parse-data [input]
  (aoc/parse-lines input :ints))

(def example-data (parse-data example))
(def data (parse-data (aoc/read-input 9)))

example-data




;; ## Part 1
;;
;; Our first task is to find the `area` of the largest rectangle whose two
;; diagonal points are the red tiles we've just parsed.

(defn rect-area [[ax ay] [bx by]]
  (* (inc (abs (- ax bx)))
     (inc (abs (- ay by)))))

(let [a (example-data 0)
      b (example-data 4)]
  [a b (rect-area a b)])

;; This looks correct. We'll find the largest area together with our task
;; for Part 2, so let's switch to that.







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
;; If it sounds complicated, just know that it was even _more_ complicated than
;; it sounds. :')
;;
;; It turns out there is a much simpler way.
;;
;; For each pair of points `a` and `b` which form a rectangle, we'll create
;; a vector of four values: `[min-x max-x min-y max-y]`.

(defn create-box [[ax ay] [bx by]]
  [(min ax bx) (max ax bx) (min ay by) (max ay by)])

(let [a (example-data 0)
      b (example-data 5)]
  [a b (create-box a b)])

;; For each rectangle box, we need to check if a polygon line is slicing through it.

(defn not-slicing? [[p-x1 p-x2 p-y1 p-y2] [r-x1 r-x2 r-y1 r-y2]]
  (or (<= p-x2 r-x1)   ; polygon line completely on the left
      (>= p-x1 r-x2)   ; polygon line completely on the right
      (<= p-y2 r-y1)   ; polygon line completely above
      (>= p-y1 r-y2))) ; polygon line completely below

;; For example, `p-x2` is the right-most coordinate of a polygon, and if that
;; is smaller than `r-x1` (the left-most coordinate of a rectangle), it means
;; that the polygon line (either horizontal or vertical, it doesn't matter)
;; is completely on the left of the rectangle.\
;; The same logic is applied to the remaining cases.


;; If a rectangle is `inside?` of a polygon, that means that
;; [`every?`](https://clojuredocs.org/clojure.core/every_q)
;; line of a polygon is `not-slicing?` it:

(defn inside? [polygon-lines rect]
  (every? #(not-slicing? % rect) polygon-lines))



;; We'll need to know a `line-length` of each polygon line:

(defn line-length [[x1 x2 y1 y2]]
  (+ (- x2 x1) (- y2 y1)))

;; Now we can create polygon lines from the provided points.

(defn create-polygon-lines [pts]
  (->> pts
       (cons (peek pts))         ; [1]
       (map create-box pts)      ; [2]
       (sort-by line-length >))) ; [3]

;; To "close" the polygon, we add its last point (we grab it efficiently with the
;; [`peek` function](https://clojuredocs.org/clojure.core/peek)) to the beginning using
;; the [`cons` function](https://clojuredocs.org/clojure.core/cons) [1].
;;
;; We'll transform each polygon line with `create-box` by providing two
;; sequences of points to the `map` function [2].
;; This automatically takes care of dealing with lines being horizontal or
;; vertical:

(let [[a b c] example-data]
  [(create-box a b) (create-box b c)])

;; Not really necessary to solve the task, but we'll sort the polygon lines so that
;; the longer lines appear before shorter lines [3].
;; This is done to gain (lots of) performance, as there is a higher chance that a longer
;; line is slicing a rectangle, and we're short-circuiting on the first slice.

;; And that's it. That's all we need to solve the problem.


(defn solve [pts]
  (let [n (count pts)
        polygon-lines (create-polygon-lines pts)]
    (loop [i 1 , j 0         ; [1]
           pt-1 0 , pt-2 0]  ; [2]
      (cond
        (>= i n) [pt-1 pt-2] ; [3]
        (= j i) (recur (inc i) 0 pt-1 pt-2) ; [4]
        :else
        (let [a (pts i)
              b (pts j)
              area (rect-area a b)]     ; [5]
          (recur i (inc j)
                 (max pt-1 area)        ; [6]
                 (if (and (> area pt-2) ; [7]
                          (inside? polygon-lines (create-box a b)))
                   area
                   pt-2)))))))

;; We need to go through all pairs of points. We'll use indices `i` and `j`
;; to get the pairs [1]. We'll update `pt-1` and `pt-2` when we improve the
;; score for each part.
;;
;; When we come to the end of indices, we return the result [3].
;; As a rectangle with diagonal points A and B is the same as the one with
;; points B and A, we consider only a half of all point combinations [4].
;;
;; On a regular loop step, we calculate `rect-area` of two points [5],
;; update the `pt-1` result [6], and only if the current `area` is better
;; than the current best result for Part 2 [7] we do an expensive check if
;; the rectangle is `inside?` the `polygon-lines` and update the `pt-2` result.


(solve example-data)
(solve data)





;; ## Conclusion
;;
;; This one was the hardest one for me so far this year.\
;; It took me a while to come up with a way to check if a rectangle is inside
;; of a polygon. And then, it turns out my original idea was an overkill and
;; there is a much simpler solution possible.
;;
;; Today's highlights:
;; - `every?`: is a predicate true for every element of a collection?
;; - `peek`: efficiently grab the last element of a vector


^:kindly/hide-code
(defn -main [input]
  (let [data (parse-data input)]
    (solve data)))
