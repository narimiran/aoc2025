(in-package #:aoc)


(defun parse-columns-1 (filename)
  (with-open-file (input-stream (aoc:input-path filename))
    (loop with columns = nil
          for line = (read-line input-stream nil nil)
          for tokens = (cl-ppcre:all-matches-as-strings "\\S+" line)
          while line
          do (setf columns
                   (if (null columns)
                       (mapcar #'list tokens)
                       (mapcar #'cons tokens columns)))
          finally (return columns))))

(defun parse-columns-2 (filename)
  (with-open-file (input-stream (aoc:input-path filename))
    (loop with columns = nil
          for line = (read-line input-stream nil nil)
          while line
          do (setf columns
                   (if (null columns)
                       (map 'list #'string line)
                       (mapcar (lambda (col ch)
                                 (concatenate 'string col (string ch)))
                               columns
                               (coerce line 'list))))
          finally (return (reverse columns)))))

(defun d6-p1 (filename)
  (loop with columns = (parse-columns-1 filename)
        for col in columns
        sum (eval (mapcar #'read-from-string col))))

(defun d6-p2 (filename)
  (loop with columns = (parse-columns-2 filename)
        with stack = nil
        with result = 0
        for col in columns
        do (if (every (lambda (c) (char= c #\Space)) col)
               (setf stack nil)
               (let ((last-ch (char col (1- (length col))))
                     (n (parse-integer col :junk-allowed t)))
                 (push n stack)
                 (unless (char= last-ch #\Space)
                   (incf result
                         (reduce (if (char= last-ch #\+) #'+ #'*)
                                 stack)))))
        finally (return result)))


(defun day06 (&optional (filename 6))
  (values (d6-p1 filename)
          (d6-p2 filename)))
