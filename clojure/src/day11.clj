^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns day11
  {:title "Reactor"
   :url "https://adventofcode.com/2025/day/11"
   :extras ""
   :highlights ""
   :remark "Surprisingly easy."
   :nextjournal.clerk/auto-expand-results? true
   :nextjournal.clerk/toc true}
  (:require [aoc-utils.core :as aoc]))




;; # Day 11: [Reactor](https://adventofcode.com/2025/day/11)
;;
;; The Elves have installed a new server rack and now need our help with
;; connecting some devices with cables.
;; They give us a list of connections between the devices which looks like
;; this:

(def example "aaa: you hhh
you: bbb ccc
bbb: ddd eee
ccc: ddd eee fff
ddd: ggg
eee: out
fff: out
ggg: out
hhh: ccc fff iii
iii: out")

;; Each line tells us how to connect a device's outputs to the correct devices.
;; For example, `aaa: you hhh` means we need to connect device `aaa` to
;; `you` and `hhh`. And the connection is directed, there is no `you` to
;; `aaa` or `hhh` to `aaa` connection.





;; ## Input parsing
;;
;; We're dealing with an acyclic graph, so let's build it immediately in the
;; parsing phase.


(defn build-graph [lines]
  (reduce (fn [acc [start & ends]]
            (assoc acc start ends))
          {}
          lines))

(defn parse-data [input]
  (-> input
      (aoc/parse-lines :keywords #"\W+") ; [1]
      build-graph))                      ; [2]

;; For each line in the input, we will split the string on `\W+`, which is
;; a regex speak for "one or more non-characters".
;; We will convert each name of a device into a keyword for convenience [1].
;; (I've added `:keywords` to the
;; [aoc-utils library](https://narimiran.github.io/aoc-utils/aoc-utils.core.html)
;; after I've seen this task.)\
;; Since the graph is acyclic, creating a hashmap representation of it is
;; straightforward [2].

(def example-data (parse-data example))
(def data (parse-data (aoc/read-input 11)))




;; ## Part 1
;;
;; For Part 1 we need to count all paths from `:you` to `:out`.
;;
;; For acyclic graphs, when traversing them we don't need to remember what
;; places we've visited, we just go go go until we reach the end.
;;
;; We'll do it recursively. Every time we come to `:out`, we count that as
;; one successful path. If we come to the end of the graph (no neighbours of a
;; current point), the path was not successful. If there are some neighbours
;; of a current point, we try to reach `:out` with all of them.

(def paths
  (memoize                                        ; [1]
   (fn [graph curr end]
     (if (= curr end) 1                           ; [2]
         (if-let [nbs (graph curr)]
           (aoc/sum-map #(paths graph % end) nbs) ; [3]
           0)))))                                 ; [4]

;; If there is a path `A - B - C - D - out` and `H - C - D - out` it means we will
;; visit points `C` and `D` multiple times. And we already know from the first
;; visit it will be a successful path. We will use our friend
;; [`memoize`](https://clojuredocs.org/clojure.core/memoize) that we met
;; in the [Day 7 solution](../day07). [1]\
;; For Part 1 we could ignore this inefficiency of these multiple visits,
;; but for Part 2 it is crucial to memoize.
;;
;; If our `curr`ent point is the `end` we wanted, that counts as one successful
;; path [2]. Otherwise, if there are neighbours of the current point, for
;; each of them we recursively call this function and sum up all successful
;; paths [3]. If we reached the end (no neighbours), it was an unsuccessful
;; path and we return zero [4].
;;
;; For Part 1 we need to count the paths from `:you` to `:out`:

(defn part-1 [graph]
  (paths graph :you :out))

(part-1 example-data)
(part-1 data)


;; Easy-peasy!






;; ## Part 2
;;
;; There is different example for Part 2:

(def example-2 "svr: aaa bbb
aaa: fft
fft: ccc
bbb: tty
tty: ccc
ccc: ddd eee
ddd: hub
hub: fff
eee: dac
dac: fff
fff: ggg hhh
ggg: out
hhh: out")

(def example-data-2 (parse-data example-2))

;; Now, our task is to find the number of paths starting from `:svr`,
;; passing through both `:dac` and `:fft`, and ending with `:out`.
;;
;; As expected, the search space is huge. Memoizing will be important, but it
;; is not enough. In a true recursive fashion, let's break the problem down
;; into simpler problems.
;;
;; If our task is to count the ways we can go from point `A` to point `C` via
;; point `B`, we can split it in counting the ways from `A` to `B` and then
;; the ways from `B` to `C`. The final result is achieved by multiplying
;; these two intermediate results.
;;
;; There is another thing to notice. Since our graph is acyclic, this means
;; there is _either_ a connection from `:dac` to `:fft` or a connection
;; from `:fft` to `:dac`, it can't be both.\
;; So we will first check which of these two is a valid connection, and
;; depending on that we will calculate the remaining paths.


(defn part-2 [graph]
  (let [fft-dac (paths graph :fft :dac)] ; [1]
    (if (zero? fft-dac)                  ; [2]
      (* (paths graph :svr :dac)
         (paths graph :dac :fft)
         (paths graph :fft :out))
      (* (paths graph :svr :fft)
         fft-dac
         (paths graph :dac :out)))))

;; As stated above, we first calculate the number of paths from `:fft`
;; to `:dac` [1].
;; If there are zero paths [2], then we know we need to do
;; `svr -> dac -> fft -> out`.
;; Otherwise, we correctly guessed the correct direction and we calulate
;; the remaining steps of the `svr -> fft -> dac -> out`.

(part-2 example-data-2)
(part-2 data)





;; ## Conclusion
;;
;; This one was a surprisingly easy and straightforward task, escpecially for
;; the next to last task this year. But I guess we deserved some breathing
;; room after yesterday's task.
;;
;; No new functions to highlight today.


;; ----
;;
;; [< Previous solution](../day10)
;; | [Source code](https://github.com/narimiran/aoc2025/blob/main/clojure/src/day11.clj)
;; | [Next solution >](../day12)



^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(defn -main [input]
  (let [data (parse-data input)]
    [(part-1 data)
     (part-2 data)]))
