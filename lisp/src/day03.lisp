(in-package #:aoc)


(defun strongest-battery (batteries start end)
  (declare (type simple-string batteries)
           (type fixnum start end))
  (let ((zero-code (char-code #\0)))
    (loop with max-idx = start
          with max-val = 0
          for idx from start below end
          for bat = (- (char-code (aref batteries idx))
                       zero-code)
          do (when (> bat max-val)
               (setf max-idx idx
                     max-val bat)
               (when (= bat 9) (loop-finish)))
          finally (return (values max-idx max-val)))))

(defun max-joltage (amount batteries)
    (loop with start = 0
          with joltage = 0
          repeat amount
          for end from (- (length batteries) (1- amount))
          do (multiple-value-bind (idx val) (strongest-battery batteries start end)
               (setf start   (1+ idx)
                     joltage (+ (* 10 joltage) val)))
          finally (return joltage)))


(defun day03 (&optional (filename 3))
  (with-open-file (input-stream (aoc:input-path filename))
    (loop for line = (read-line input-stream nil nil)
          while line
          sum (max-joltage  2 line) into p1
          sum (max-joltage 12 line) into p2
          finally (return (values p1 p2)))))
