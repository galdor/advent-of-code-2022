(defpackage :aoc2022-06
  (:use :cl :aoc2022-utils)
  (:export
   :*lines*
   :solve-1
   :solve-2))

(in-package :aoc2022-06)

(defvar *data* (input-file-data 6))

(defun find-marker (string window-size)
  (do ((i 1 (1+ i))
       (window nil))
      ((>= i (length string)))
    (let ((c (char string i)))
      (cond
        ((member c window)
         (setf window nil))
        (t
         (push c window)
         (when (= (length window) window-size)
           (return-from find-marker (1+ i))))))))

(defun solve-1 ()
  (find-marker *data* 4))

(defun solve-2 ()
  (find-marker *data* 14))
