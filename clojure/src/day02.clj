^:kindly/hide-code
(ns day02
  {:title "Gift Shop"
   :url "https://adventofcode.com/2025/day/2"
   :extras "bench"
   :highlights "partition, re-matches"
   :remark "Easier than Day 1."}
  (:require [aoc-utils.core :as aoc]
            [clojure.string :as str]))




;; # Day 2: Gift Shop
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

;; The parsed data looks like this:
example-data

;; We can work with this. Let's continue with our Part 1 task.








;; ## Part 1
;;
;; We need to find invalid IDs, which are those where the ID consists of two
;; identical halves, e.g. `567567` is invalid and `565656` isn't.

(def pow10s
  (long-array [1 10 100 1000 10000 100000 1000000]))

(defn identical-halves? [^long n]
  (let [digits (aoc/count-digits n)]          ; [1]
    (and (even? digits)                       ; [2]
         (let [half (aget ^longs pow10s
                          (quot digits 2))]   ; [3]
           (= (quot n half) (rem n half)))))) ; [4]

;; To count the number of digits of a number, we have a handly helper
;; [`aoc/count-digits`](https://narimiran.github.io/aoc-utils/aoc-utils.core.html#var-count-digits) [1].
;; We'll only consider the numbers which can be cut in two halves of identical
;; lengths [2].\
;; To cut a number in half, we need to find a power of 10 which we can use,
;; e.g. for 6-digit number, that would be 10^3 [3]. Using that `half`, we
;; can compare numbers on the both sides of it [4].


(identical-halves? 567567)
(identical-halves? 565656)



(defn sum-invalids [invalid-fn [lo hi]] ; [1]
  (transduce                            ; [2]
   (filter invalid-fn)                  ; [3]
   +
   (range lo (inc hi))))                ; [4]

;; We have our ranges represented as a two-element list, so we will immediately
;; destructure it [1].
;;
;; The [`transduce` function](https://clojuredocs.org/clojure.core/transduce)
;; is similar to `reduce`, but more efficient [2].\
;; First we `filter` the numbers to keep only those for which the `invalid-fn`
;; returns a truthy value [3], and then take the sum of those numbers.
;; We didn't hardcode the `invalid-fn` as `identical-halves?` because we
;; have a luxury of knowing what Part 2 brings.\
;; The `range` function doesn't include the upper bound, so we need to take
;; that into an account [4].


;; To calculate the total sum, we need to apply the `sum-invalids` function
;; to each range in the input, and then take a sum of the results. With the
;; [`aoc/sum-pmap` function](https://narimiran.github.io/aoc-utils/aoc-utils.core.html#var-sum-pmap)
;; we can do that in parallel, to speed things up a bit.

(defn total-invalids [data invalid-fn]
  (aoc/sum-pmap #(sum-invalids invalid-fn %) data))

(total-invalids example-data identical-halves?)
(total-invalids data identical-halves?)

;; The first star is here!







;; ## Part 2
;;
;; In Part 2 we discover that the invalid IDs are those which have some
;; substring repeated _several_ times (not only twice as in Part 1).
;; This means that now the ID `565656` becomes invalid, as the `56` substring
;; repeats three times.




;; ### Regular expressions
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
;; which returns a match if _any part_ of the string matches the pattern.\
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

(defn repeated-substring? [n]
  (re-matches part-2-pattern (str n)))


;; We can now pass this new function to the `total-invalids` function we've
;; used in Part 1, and it should produce the result for Part 2.

(total-invalids example-data repeated-substring?)
(total-invalids data repeated-substring?)

;; Solved!





;; ### Without regex
;;
;; We can also solve Part 2 without regex with a string concat "trick".
;;
;; If we have a string consisting of two parts, `A` and `B`, and we want to
;; check if `A` and `B` are the same, we can duplicate the original string,
;; so `AB` becomes `ABAB`.\
;; Now, if `A = B`, that also means that `B = A`. So, if we start searching
;; for the original `AB` pattern (starting not from the very beginning of
;; the string), we will eventually find it when we encounter the `BA` substring.\
;; If `A` and `B` are not the same, we would find the `AB` pattern only when
;; we move past the original string.
;;
;; Here is how our logic looks like for two examples:
;; ```
;; 567|567
;;  A | B
;;
;; 567|567||567|567
;;  A | B || A | B
;;     ^^^  ^^^
;;    original string found
;; ```
;;
;; ```
;; 123|789
;;  A | B
;;
;; 123|789||123|789
;;  A | B || A | B
;;          ^^^ ^^^
;;     found only after the original length
;; ```
;;
;; In code, it looks like this:

(defn repeated-substring-2? [n]
  (let [s (str n)            ; [1]
        ss (str s s)]        ; [2]
    (< (str/index-of ss s 1) ; [3]
       (count s))))          ; [4]

;; We convert a number to string `s` [1], and create a double-string `ss` [2].
;; We need to check at which index of `ss` we will find the original string `s`,
;; starting from index `1` [3].\
;; If we found it before the end of the original string `s`, it means that we
;; have encountered a number with a repeated substring.

(total-invalids example-data repeated-substring-2?)
(total-invalids data repeated-substring-2?)

;; We get the same result. Success!






;; ### Char by char
;;
;; Instead of comparing substrings, we can compare characters at given positions.
;;
;; For example, if our substring length is 3, we check the characters that are at
;; multiples-of-3 distance from each other:
;;
;; ```
;; 123|123|123
;; ^   ^   ^
;; 0   3   6
;; 0   0   0  (mod 3)
;; ```

(defn repeated-substring-3? [n]
  (let [s   (Long/toString n)                 ; [1]
        len (count s)]
    (loop [sublen (quot len 2)]               ; [2]
      (cond
        (zero? sublen) false                  ; [3]
        (and (zero? (rem len sublen))         ; [4]
             (loop [idx sublen]
               (cond
                 (= idx len) true             ; [5]
                 (not= (.charAt s (rem idx sublen))
                       (.charAt s idx)) false ; [6]
                 :else (recur (inc idx)))))
        true                                  ; [7]
        :else (recur (dec sublen))))))        ; [8]

;; We are chasing maximum performance, so we'll use the specialized `Long/toString`
;; instead of general `str` to convert a number to string [1].
;;
;; We start from the maximum length of a substring [2].\
;; If we tried every possible substring length and we didn't find any matches,
;; there are no repeated substrings [3].
;;
;; If the current substring length cleanly divides the original string's length [4],
;; we proceed to check with `loop` if characters at indices `idx` (starting from
;; the substring length) match those of a substring on the same position. If there's
;; a mismatch [6], the whole condition [4] is `false` and we drop to the `:else` branch
;; and retry our luck with a smaller substring length [8].\
;; If there was no mismatch along the way and we came to the end of the string [5],
;; our condition [4] is `true` and we fount our solution and exit the outer `loop` [7].
;;
;; P.S. We use Java's `.charAt` instead of Clojure's `get`, as it is significantly faster.

(total-invalids example-data repeated-substring-3?)
(total-invalids data repeated-substring-3?)

;; Still the same result as before.






;; ### Performance comparison
;;
;; To regex or not to regex, that is the question.

;; ```clj
;; (require '[criterium.core :as c])
;;
;; (c/quick-bench (total-invalids data repeated-substring?))
;; ```

;; ```
;; Evaluation count : 18 in 6 samples of 3 calls.
;;              Execution time mean : 44.040410 ms
;;     Execution time std-deviation : 4.576698 ms
;;    Execution time lower quantile : 38.477877 ms ( 2.5%)
;;    Execution time upper quantile : 49.527380 ms (97.5%)
;;                    Overhead used : 1.867496 ns)
;; ```

;; ```clj
;; (c/quick-bench (total-invalids data repeated-substring-2?))
;; ```

;; ```
;; Evaluation count : 24 in 6 samples of 4 calls.
;;              Execution time mean : 33.577091 ms
;;     Execution time std-deviation : 1.364652 ms
;;    Execution time lower quantile : 31.998935 ms ( 2.5%)
;;    Execution time upper quantile : 34.863190 ms (97.5%)
;;                    Overhead used : 1.867496 ns)
;; ```

;; The non-regex version is 1.33x faster. Not a mind-blowing difference, but
;; still significant.
;;
;; How about our char-by-char solution?


;; ```clj
;; (c/quick-bench (total-invalids data repeated-substring-3?))
;; ```

;; ```
;; Evaluation count : 78 in 6 samples of 13 calls.
;;              Execution time mean : 8.249629 ms
;;     Execution time std-deviation : 519.204099 µs
;;    Execution time lower quantile : 7.796738 ms ( 2.5%)
;;    Execution time upper quantile : 9.059101 ms (97.5%)
;;                    Overhead used : 1.834820 ns)
;; ```

;; Wow, 4x faster than the non-regex version!








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

^:kindly/hide-code
(defn -main [input]
  (let [data (parse-data input)]
    [(total-invalids data identical-halves?)
     (total-invalids data repeated-substring-3?)]))
