(in-package #:aoc)


(defun build-graph (filename)
  (let ((graph (make-hash-table :test #'equal)))
    (with-open-file (input-stream (aoc:input-path filename))
      (loop for line = (read-line input-stream nil nil)
            for nodes = (cl-ppcre:split "\\W+" line)
            while line
            do (setf (gethash (first nodes) graph)
                     (rest nodes))))
    graph))


(defparameter *cache* (make-hash-table :test #'equal))

(defun paths (graph curr end)
  (if (equal curr end)
      1
      (let* ((cache-key (cons curr end))
             (val (gethash cache-key *cache*)))
        (or val
            (setf (gethash cache-key *cache*)
                  (let ((nbs (gethash curr graph)))
                    (if nbs
                        (loop for nb in nbs
                              sum (paths graph nb end))
                        0)))))))

(defun d11-p2 (graph)
  (let ((fft-dac (paths graph "fft" "dac")))
    (if (zerop fft-dac)
        (* (paths graph "svr" "dac")
           (paths graph "dac" "fft")
           (paths graph "fft" "out"))
        (* (paths graph "svr" "fft")
           fft-dac
           (paths graph "dac" "out")))))


(defun day11 (&optional (filename 11))
  (let ((graph (build-graph filename)))
    (values (paths graph "you" "out")
            (d11-p2 graph))))
