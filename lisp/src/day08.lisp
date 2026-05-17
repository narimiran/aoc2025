(in-package #:aoc)


(defun parse-points (filename)
  (with-open-file (input-stream (aoc:input-path filename))
    (loop for line = (read-line input-stream nil nil)
          while line
          collect (loop with len = (length line)
                        for start = 0 then (1+ end)
                        for end = (or (position #\, line :start start) len)
                        collect (parse-integer line :start start :end end)
                        while (< end len)))))

(defun sq (x)
  (* x x))

(defun distance-squared (a b)
  (destructuring-bind (x1 y1 z1) a
    (destructuring-bind (x2 y2 z2) b
      (+ (sq (- x2 x1))
         (sq (- y2 y1))
         (sq (- z2 z1))))))

(defun sorted-connections (boxes)
  (sort (loop for a in boxes
              append
              (loop for b in boxes
                    while (not (equal a b))
                    collect (list (distance-squared a b) a b)))
        #'<
        :key #'car))

(defun create-circuits (points)
  (mapcar (lambda (pt) (list pt)) points))

(defun current-circuit (circuits pt)
  (dolist (circuit circuits)
    (when (member pt circuit :test #'equal)
      (return circuit))))

(defun connect (circuits a b)
  (let ((circ-a (current-circuit circuits a))
        (circ-b (current-circuit circuits b)))
    (if (equal circ-a circ-b)
        circuits
        (cons (append circ-a circ-b)
              (loop for circ in circuits
                    unless (or (eq circ-a circ)
                               (eq circ-b circ))
                      collect circ)))))

(defun pt1-score (circuits)
  (reduce #'*
          (subseq (sort (mapcar #'length circuits) #'>)
                  0
                  3)))

(defun day08 (&optional (filename 8))
  (loop with points = (parse-points filename)
        with conns = (sorted-connections points)
        with circuits = (create-circuits points)
        with n = 1
        with pt1 = 0
        for (nil a b) in conns
        do
           (setf circuits (connect circuits a b))
           (when (= n 1000)
             (setf pt1 (pt1-score circuits)))
           (when (= 1 (length circuits))
             (return (values pt1 (* (first a) (first b)))))
           (incf n)))
