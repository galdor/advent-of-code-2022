(defpackage :aoc2022-04
  (:use :cl :aoc2022-utils)
  (:export
   :*lines*
   :solve-1
   :solve-2))

(in-package :aoc2022-04)

(defvar *lines* (input-file-lines 4))

(defun parse-range-pairs ()
  (mapcar 'parse-range-pair *lines*))

(defun parse-range-pair (string)
  (let ((comma (position #\, string)))
    (cons (parse-range (subseq string 0 comma))
          (parse-range (subseq string (1+ comma))))))

(defun parse-range (string)
  (let ((dash (position #\- string)))
    (cons (parse-integer string :end dash)
          (parse-integer string :start (1+ dash)))))

(defun range-fully-contains-p (range-1 range-2)
  (and (>= (car range-2) (car range-1))
       (<= (cdr range-2) (cdr range-1))))

(defun ranges-overlap-p (range-1 range-2)
  (and (<= (car range-1) (cdr range-2))
       (>= (cdr range-1) (car range-2))))

(defun solve-1 ()
  (count-if (lambda (pair)
              (or (range-fully-contains-p (car pair) (cdr pair))
                  (range-fully-contains-p (cdr pair) (car pair))))
            (parse-range-pairs)))

(defun solve-2 ()
  (count-if (lambda (pair)
              (ranges-overlap-p (car pair) (cdr pair)))
            (parse-range-pairs)))
