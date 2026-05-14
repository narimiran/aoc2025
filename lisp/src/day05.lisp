(in-package #:aoc)


(defun parse-ranges-ingredients (filename)
  (let ((ranges '())
        (ingredients '())
        (seen-blank? nil))
    (with-open-file (input-stream (aoc:input-path filename))
      (loop for line = (read-line input-stream nil nil)
            while line
            do (cond
                 ((and (not seen-blank?) (string= line ""))
                  (setf seen-blank? t))
                 ((not seen-blank?)
                  (let ((dash-pos (position #\- line)))
                    (push (cons (parse-integer line :end dash-pos)
                                (parse-integer line :start (1+ dash-pos)))
                          ranges)))
                 (t (push (parse-integer line) ingredients)))))
    (values (sort ranges #'< :key #'car)
            (sort ingredients #'<))))

(defun d5-p1 (ranges ingredients)
  (loop with count = 0
        with ranges_ = ranges
        for ingredient in ingredients
        do (loop while ranges_
                 for (lo . hi) = (first ranges_)
                 do (cond
                      ((< ingredient lo) (return))
                      ((<= lo ingredient hi)
                       (incf count)
                       (return))
                      (t (pop ranges_))))
        finally (return count)))

(defun d5-p2 (ranges)
  (loop with fresh = 0
        with highest = -1
        for (lo . hi) in ranges
        do (when (> hi highest)
             (let* ((start (max lo (1+ highest)))
                    (size  (- (1+ hi) start)))
               (incf fresh size)
               (setf highest hi)))
        finally (return fresh)))


(defun day05 (&optional (filename 5))
  (multiple-value-bind (ranges ingredients) (parse-ranges-ingredients filename)
    (values (d5-p1 ranges ingredients)
            (d5-p2 ranges))))
