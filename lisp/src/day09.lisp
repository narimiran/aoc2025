(in-package #:aoc)


(defun parse-polygon (filename)
  (with-open-file (input-stream (aoc:input-path filename))
    (let ((lines (loop for line = (read-line input-stream nil nil)
                       while line
                       collect (aoc:integers line))))
      (coerce lines 'vector))))

(defun create-box (a b)
  (let ((ax (first a))
        (ay (second a))
        (bx (first b))
        (by (second b)))
    (list (if (< ax bx) ax bx)
          (if (< ax bx) bx ax)
          (if (< ay by) ay by)
          (if (< ay by) by ay))))

(defun area (box)
  (destructuring-bind (x1 x2 y1 y2) box
    (* (1+ (- x2 x1))
       (1+ (- y2 y1)))))


(defun largest-rectangles (pts)
  (let ((result '()))
    (loop for a across pts do
      (loop for b across pts
            while (not (eq a b))
            for box = (create-box a b)
            do (push (list (area box) box) result)))
    (sort result #'> :key #'first)))


(defun not-slicingp (box rect)
  (destructuring-bind (min-x max-x min-y max-y) box
    (destructuring-bind (r-min-x r-max-x r-min-y r-max-y) rect
      (or (>= r-min-x max-x)
          (<= r-max-x min-x)
          (>= r-min-y max-y)
          (<= r-max-y min-y)))))

(defun insidep (polygon-lines rect)
  (loop for box across polygon-lines
        always (not-slicingp box rect)))

(defun create-boxes (polygon)
  (let* ((n (length polygon))
         (boxes (make-array n)))
    (loop for i from 0 below n
          for a = (aref polygon i)
          for b = (aref polygon (mod (1+ i) n))
          do (setf (aref boxes i) (create-box a b)))
    boxes))

(defun day09 (&optional (filename 9))
  (let* ((polygon (parse-polygon filename))
         (rectangles (largest-rectangles polygon))
         (boxes (create-boxes polygon)))
    (values (caar rectangles)
            (loop for (area rect) in rectangles
                  thereis (when (insidep boxes rect)
                            area)))))
