(defpackage #:aoc
  (:use #:cl)
  (:local-nicknames (:re :cl-ppcre))
  (:export #:input-path
           #:read-input
           #:integers
           #:parse-input

           #:day01
           #:day02
           #:day03
           #:day04
           #:day05
           #:day06
           #:day07
           #:day08
           #:day09
           ;; #:day010
           #:day11
           #:day12))


(in-package #:aoc)



(defun input-path (file)
  (let ((name (if (integerp file)
                  (format nil "~2,'0d" file)
                  file)))
    (asdf:system-relative-pathname
     :aoc
     (concatenate 'string "../inputs/" name ".txt"))))

(defun read-input (file)
    (string-right-trim
     '(#\Newline)
     (uiop:read-file-string (input-path file))))


(defun integers (s &key (negative? t))
  (mapcar #'parse-integer
          (re:all-matches-as-strings
           (if negative?
               "-?\\d+"
               "\\d+")
           s)))

(defun string->digits (s)
  (loop for ch across s
        for n = (parse-integer (string ch) :junk-allowed t)
        when n collect n))

(defun words (s &optional word-sep)
  (re:split (or word-sep "\\s+") s))


(defun parse-input (s parse-fn &optional word-sep)
  (let ((f (if (functionp parse-fn)
               parse-fn
               (case parse-fn
                 (:int #'parse-integer)
                 (:ints #'integers)
                 (:nats (lambda (x) (integers x :negative? nil)))
                 (:digits #'string->digits)
                 (:chars (lambda (x) (coerce x 'list)))
                 (:words (lambda (x) (words x word-sep)))
                 (:string #'identity)))))
    (funcall f s)))
