(in-package #:aoc)


(defun invalid-2? (id-string half len)
  (declare (type fixnum half len))
  (loop for i from 1 to half
        when (and (zerop (mod len i))
                  (loop for j from i below len by i
                        always (string= id-string id-string
                                        :start1 0 :end1 i
                                        :start2 j :end2 (+ j i))))
          do (return t)))

(defun is-invalid? (id)
  (let* ((id-string (princ-to-string id))
         (len (length id-string)))
    (multiple-value-bind (half rem) (floor len 2)
      (cond
       ((and (zerop rem)
             (string= id-string id-string
                      :start1 0 :end1 half
                      :start2 half :end2 len)) (values id id))
       ((invalid-2? id-string half len) (values 0 id))
       (t (values 0 0))))))


(defun day02 (&optional (filename 2))
  (let ((data (aoc:parse-input (aoc:read-input filename) :nats))
        (p1 0)
        (p2 0))
    (loop for (lo hi) on data by #'cddr do
      (loop for i from lo to hi
            do (multiple-value-bind (n1 n2) (is-invalid? i)
                 (incf p1 n1)
                 (incf p2 n2))))
    (values p1 p2)))
