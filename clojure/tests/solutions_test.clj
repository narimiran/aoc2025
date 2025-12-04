(ns solutions-test
  (:require
   [aoc-utils.core :as aoc]
   day01 day02 day03 day04
   ;day05 day06 day07 day08
   ;day09 day10 day11 day12
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
