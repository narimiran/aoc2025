(in-package #:aoc)


(defun parse-rotation (line)
  (* (if (char= #\L (char line 0)) -1 1)
     (parse-integer (subseq line 1))))

(defun day01 (&optional (filename 1))
  (with-open-file (input-stream (aoc:input-path filename))
    (loop with size = 100
          with pos = 50
          with p1 = 0
          with p2 = 0
          for line = (read-line input-stream nil nil)
          while line
          do (let ((end (+ pos (parse-rotation line))))
               (multiple-value-bind (q r) (truncate end size)
                 (let ((end_ (if (minusp r) (+ r size) r))
                       (rounds (abs q)))
                   (setf pos end_
                         p1  (if (zerop end_) (1+ p1) p1)
                         p2  (+ p2
                                rounds
                                (if (>= pos 1 0 end) 1 0))))))
          finally (return (values p1 p2)))))
