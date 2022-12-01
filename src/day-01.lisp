(defpackage :aoc2022-01
  (:use :cl :aoc2022-utils)
  (:export
   :*lines*
   :count-calories
   :solve-1
   :solve-2))

(in-package :aoc2022-01)

(defvar *lines* (input-file-lines 1))

(defun count-calories ()
  (labels ((acc (lines counts count)
             (cond
               ((null lines)
                (nreverse (cons count counts)))
               ((string= (car lines) "")
                (acc (cdr lines) (cons count counts) 0))
               (t
                (let ((n (parse-integer (car lines))))
                  (acc (cdr lines) counts (+ count n)))))))
    (acc *lines* nil 0)))

(defun solve-1 ()
  (let ((calories (count-calories)))
    (apply #'max calories)))

(defun solve-2 ()
  (let* ((calories (count-calories))
         (top-3 (subseq (sort calories #'>) 0 3)))
    (apply #'+ top-3)))
