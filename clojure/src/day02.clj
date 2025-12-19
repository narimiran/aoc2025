^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns day02
  {:title "Gift Shop"
   :url "https://adventofcode.com/2025/day/2"
   :extras ""
   :highlights "partition, re-matches"
   :remark "Easier than Day 1."
   :nextjournal.clerk/auto-expand-results? true
   :nextjournal.clerk/toc true}
  (:require [aoc-utils.core :as aoc]))




;; # Day 2: [Gift Shop](https://adventofcode.com/2025/day/2)
;;
;; Today we're at the North Pole gift shop and we're given a list of product
;; IDs and we need to find invalid IDs among those.
;; The list looks like this:

(def example "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,
1698522-1698528,446443-446449,38593856-38593862,565653-565659,
824824821-824824827,2121212118-2121212124")





;; ## Input parsing
;;
;; Before we start, we need to extract values from that list.
;; The IDs are given as ranges, e.g. `11-22`, meaning there are IDs from `11`
;; to `22` (including both ends of the range).
;;
;; Our real input is just one long line, and the example is split into multiple
;; lines just for our convenience. In both cases, we don't need to split the input
;; on the newlines, so we can use the
;; [`aoc/parse-input` function](https://narimiran.github.io/aoc-utils/aoc-utils.core.html#var-parse-input).\
;; This function takes various keywords (or a custom function) as its second
;; argument (check the documentation on the link above), but in this case using
;; `:ints` would produce `[11 -22]` for the example above, which is not what we
;; want.\
;; I've updated this function to now also take `:nats`, which gives
;; list of natural (non-negative) integers, ignoring any `-` signs it sees in
;; the string, correctly producing `[11 22]`.
;;
;; We now have a list of _all_ positive integers in the input string, and to get
;; back the lower and upper value of each range we need to
;; [`partition`](https://clojuredocs.org/clojure.core/partition) the list:

(defn parse-data [input]
  (->> (aoc/parse-input input :nats)
       (partition 2)))

(def example-data (parse-data example))
(def data (parse-data (aoc/read-input 2)))

;; We can work with this. Let's continue with our Part 1 task.








;; ## Part 1
;;
;; We need to find invalid IDs, which are those where the ID consists of two
;; identical halves, e.g. `567567` is invalid and `565656` isn't.

(defn invalid-id? [id]
  (let [id-string (str id)
        half (/ (count id-string) 2)]
    (= (subs id-string 0 half)
       (subs id-string half))))

(invalid-id? 567567)
(invalid-id? 565656)


;; Now we have to check all IDs in a given range to see which ones are invalid:

(defn check-range [lo hi]
  (filter invalid-id?           ; [1]
          (range lo (inc hi)))) ; [2]

;; We will go through the range of IDs and we want to keep only invalid ones [1].
;; The `range` function doesn't include the upper bound, so we need to take
;; that into an account [2].

(check-range 4 30)
(check-range 998 1012)


;; Our task is to add up all invalid IDs:

(defn sum-invalids [check-fn [lo hi]] ; [1]
  (reduce + (check-fn lo hi)))

;; We are passing in a `check-fn` because we now have a luxury of knowing
;; what Part 2 brings. We have our ranges represented as a two-element list,
;; so we will immediately destructure it [1].

(sum-invalids check-range [4 30])


;; Everything works correctly.
;; To calculate the total sum, we need to apply the `sum-invalids` function
;; to each range in the input, and then take a sum of the results. With the
;; [`aoc/sum-pmap` function](https://narimiran.github.io/aoc-utils/aoc-utils.core.html#var-sum-pmap)
;; we can do that in parallel, to speed things up a bit.

(defn total-invalids [data check-fn]
  (aoc/sum-pmap #(sum-invalids check-fn %) data))

(total-invalids example-data check-range)
(total-invalids data check-range)

;; The first star is here!







;; ## Part 2
;;
;; In Part 2 we discover that the invalid IDs are those which have some
;; substring repeated _several_ times (not only twice as in Part 1).
;; This means that now the ID `565656` becomes invalid, as the `56` substring
;; repeats three times.
;;
;; Here we could continue with a similar approach we had in Part 1, having to
;; check if an ID string consists just of some substring (of a different length),
;; e.g. check if ID `123456` consists of only `1`s, or just `12`s, or just
;; `123`s, etc. But there is a better way.
;;
;; Enter regular expressions!
;;
;; I hope you're at least somewhat familiar with regular expressions so this
;; won't look like some gibberish. But I'll still try to explain it bit by bit.
;; If you understand regex well, feel free to skip the next paragraph.
;;
;; To match a single digit, we use `\d`.
;; We need to check if some substring of _one-or-more_ digits (`\d+`) (notice the plus)
;; repeats all the way from the start to the end of a string.\
;; We could use `^` and `$` to match the start and the end respectively, or
;; we can use the [`re-matches` function](https://clojuredocs.org/clojure.core/re-matches)
;; which will return a match only if the _whole_ string matches the pattern,
;; compared to the [`re-find` function](https://clojuredocs.org/clojure.core/re-find)
;; which returns a match if any _part_ of the string matches the pattern.\
;; To capture the substring, we enclose it in the parentheses: `(\d+)`.
;; We can now backreference this capture with `\1`.
;;
;; This now would be pattern which we would use for Part 1: there's a substring
;; which repeats one (and only one) more time before the end of the string.
;; For example:

(def part-1-pattern #"(\d+)\1")

(re-matches part-1-pattern "567567")
(re-matches part-1-pattern "565656")

;; To make this work for Part 2, all we need to do is to allow the captured
;; substring to repeat more than once, and we do that by changing `\1` to `\1+`.

(def part-2-pattern #"(\d+)\1+")

(re-matches part-2-pattern "567567")
(re-matches part-2-pattern "565656")


;; That's it! All it remains is to use this new check for each number of each
;; range.

(defn check-range-2 [lo hi]
  (filter #(re-matches part-2-pattern (str %))
          (range lo (inc hi))))

;; We can now pass this new function to the `total-invalids` function we've
;; used in Part 1, and it should produce the result for Part 2.

(total-invalids example-data check-range-2)
(total-invalids data check-range-2)

;; Solved!









;; ## Conclusion
;;
;; This one looks more straightforward than the yesterday's task: the input is
;; easier to parse and it is clearer what we have to do to solve it.\
;; Knowing regex helps, especially for Part 2, but this is solvable even
;; without that knowledge.
;;
;; Today's highlights:
;; - `partition`: split a sequence into chunks of equal length
;; - `re-matches`: check if a _whole_ string matches a regex pattern


;; ----
;;
;; [< Previous solution](../day01)
;; | [Source code](https://github.com/narimiran/aoc2025/blob/main/clojure/src/day02.clj)
;; | [Next solution >](../day03)



^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(defn -main [input]
  (let [data (parse-data input)]
    [(total-invalids data check-range)
     (total-invalids data check-range-2)]))
