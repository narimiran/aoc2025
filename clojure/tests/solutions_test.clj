(ns solutions-test
  (:require
   [aoc-utils.core :as aoc]
   day01 day02 day03 day04
   day05 day06 day07 day08
   day09 day10 day11 day12
   [clojure.test :refer [deftest is]]))



(defmacro check-day [day test-results real-results]
  (let [day        (format "%02d" day)
        full-day   (str "day" day)
        main-func  (symbol full-day "-main")
        test-name  (symbol (str full-day "-test"))
        test-input (str day "_test")]
    `(deftest ~test-name
       (when ~test-results
         (is (= ~test-results (~main-func (aoc/read-input ~test-input)))))
       (is (= ~real-results (~main-func (aoc/read-input ~day)))))))


(check-day 1 [3 6] [1168 7199])
(check-day 2 [1227775554 4174379265] [28146997880 40028128307])
(check-day 3 [357 3121910778619] [17034 168798209663590])
(check-day 4 [13 43] [1543 9038])
(check-day 5 [3 14] [661 359526404143208])
(check-day 6 [4277556 3263827] [4449991244405 9348430857627])
(check-day 7 [21 40] [1605 29893386035180])
(check-day 8 nil [352584 9617397716])
(check-day 9 [50 24] [4781235324 1566935900])
(check-day 10 [7 33] [538 20298])
(check-day 11 [5 0] [555 502447498690860])
(check-day 12 nil 575)
