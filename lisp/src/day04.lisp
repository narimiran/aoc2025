(in-package #:aoc)


(defparameter *size* 140)
(defparameter *area* (* *size* *size*))
(defparameter *deltas* (list (- (1+ *size*)) (- *size*) (- (1- *size*))
                                    -1                          1
                                (1- *size*)     *size*     (1+ *size*)))

(defun hash (row col)
  (+ (* *size* row) col))

(defun parse-data (filename)
  (let ((rolls (make-array *area* :element-type 'boolean :initial-element nil)))
    (with-open-file (input-stream (aoc:input-path filename))
      (loop for row from 1
            for line = (read-line input-stream nil nil)
            while line
            do (loop for col from 1
                     for c across line
                     when (char= #\@ c)
                       do (setf (aref rolls (hash row col)) t))))
    rolls))

(defun count-neighbours (rolls key)
  (loop for delta in *deltas*
        count (aref rolls (+ key delta))))

(defun count-accessible (rolls)
  (loop for idx from 0 below *area*
        when (aref rolls idx)
          count (< (count-neighbours rolls idx) 4)))

(defun remove-available (rolls)
  (loop for idx from 0 below *area*
        with removed = 0
        when (and (aref rolls idx)
                  (< (count-neighbours rolls idx) 4))
           do (setf (aref rolls idx) nil)
              (incf removed)
        finally (return removed)))

(defun remove-repeatedly (rolls)
  (loop with total = 0
        for removed = (remove-available rolls)
        while (plusp removed) do
          (incf total removed)
        finally (return total)))


(defun day04 (&optional (filename 4))
  (let ((rolls (parse-data filename)))
    (values (count-accessible rolls)
            (remove-repeatedly rolls))))
