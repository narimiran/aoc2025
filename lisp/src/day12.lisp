(in-package #:aoc)


(defun parse-sizes (filename)
  (with-open-file (input-stream (aoc:input-path filename))
    (loop with acc = '()
          for line = (read-line input-stream nil nil)
          while line
          if (string= "" line)
            do (setf acc '())
          else
            do (push (aoc:integers line) acc)
          finally (return acc))))

(defun fitsp (line)
  (destructuring-bind (w h &rest amounts) line
    (<= (* 9 (reduce #'+ amounts))
        (* w h))))

(defun day12 (&optional (filename 12))
  (let ((lines (parse-sizes filename)))
    (loop for line in lines
          count (fitsp line))))
