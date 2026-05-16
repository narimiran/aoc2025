(in-package #:aoc)


(defun find-splits (filename)
  (with-open-file (input-stream (aoc:input-path filename))
    (loop repeat 2
          do (read-line input-stream))
    (loop for line = (read-line input-stream nil nil)
          while line
          do (read-line input-stream)
          collect (loop for ch across line
                        for col from 0
                        when (char= ch #\^)
                        collect col))))


(defun day07 (&optional (filename 7))
  (let* ((splits (find-splits filename))
         (start (caar splits))
         (state (make-hash-table))
         (state_ (make-hash-table))
         (split-count 0))
    (setf (gethash start state) 1)
    (loop for row in splits
          do (progn
               (maphash (lambda (pos v)
                          (if (member pos row)
                              (progn
                                (incf split-count 1)
                                (incf (gethash (1- pos) state_ 0) v)
                                (incf (gethash (1+ pos) state_ 0) v))
                              (incf (gethash pos state_ 0) v)))
                        state)
               (rotatef state state_)
               (clrhash state_)))
    (values split-count
            (loop for v being the hash-values of state
                  sum v))))
