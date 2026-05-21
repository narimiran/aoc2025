# Advent of Code 2025


All my Advent of Code repos:

* [AoC 2015 in Nim, Python](https://github.com/narimiran/advent_of_code_2015)
* [AoC 2016 in Python, Clojure (+ visualizations)](https://github.com/narimiran/advent_of_code_2016)
* [AoC 2017 in Nim, OCaml, Python](https://github.com/narimiran/AdventOfCode2017)
* [AoC 2018 in Nim, Python, Racket](https://github.com/narimiran/AdventOfCode2018)
* [AoC 2019 in OCaml, Python, Clojure](https://github.com/narimiran/AdventOfCode2019)
* [AoC 2020 in Nim, one liner-y Python, Racket](https://github.com/narimiran/AdventOfCode2020)
* [AoC 2021 in Python, Racket](https://github.com/narimiran/AdventOfCode2021)
* [AoC 2022 in Python, Clojure](https://github.com/narimiran/AdventOfCode2022)
* [AoC 2023 in Clojure](https://github.com/narimiran/AdventOfCode2023)
* [AoC 2024 in Clojure (Clerk notebooks), Python, Elixir](https://github.com/narimiran/aoc2024)
* [AoC 2025 in Clojure (Clerk notebooks), Common Lisp](https://github.com/narimiran/aoc2025) (this repo)


&nbsp;

This year it is a Clojure time, again.
(May 2026 EDIT: And a Common Lisp time, for the first time ever. See below.)

Last year's experiment with [Clerk notebooks](https://clerk.vision) was a success
(IMO; you can see [my AoC 2024 notebooks](https://narimiran.github.io/aoc2024) for yourself),
so I'll use the notebook format once again.

To make this notebook format "work", I'll have to write the code in a different way
than I usually do:
I'll not just present the final solution, but the whole process
from reading the input and then slowly building the required blocks
until we come with the solution.




## Solutions


### Clojure

It is the best to _not_ read the raw code in this repo.
Instead, read the notebooks.

The notebooks are available at: https://narimiran.github.io/aoc2025



### Common Lisp

Common Lisp solutions were written in May 2026 and are loosely based on my existing Clojure solutions.
This is my first time using Common Lisp: if you have any advice how to improve the solutions and/or write them more idiomatically, I would love to hear it - please open an issue.

The solutions are in the [`lisp/src`](lisp/src) directory.
To run them (this is mostly a note to myself, so I don't forget):

- open a REPL
- run `(ql:quickload "aoc")`
- run `(in-package #:aoc)`
- now, for each day there is a function which runs both parts, e.g. `(day01)`, `(day11)`, etc.
- to get some evaluation statistics, use `(time (day05))`
