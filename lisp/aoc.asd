(defsystem "aoc"
  :version "0.0.1"
  :depends-on (:cl-ppcre)
  :serial t
  :components ((:module "src"
                :components
                ((:file "aoc")
                 (:file "day01")
                 (:file "day02")
                 (:file "day03")
                 (:file "day04")
                 (:file "day05")
                 (:file "day06")
                 (:file "day07")
                 (:file "day08")
                 (:file "day09")
                 ;; (:file "day10")
                 (:file "day11")
                 (:file "day12"))))
  :description "Advent of Code")
