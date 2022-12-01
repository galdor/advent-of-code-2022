(defpackage :aoc2022-utils
  (:use :cl)
  (:export
   :input-file-path
   :input-file-data
   :input-file-lines))

(in-package :aoc2022-utils)

(defun input-file-path (day)
  "Return the path of an input file in the repository."
  (let ((path (make-pathname :directory '(:relative "data")
                             :name (format nil "day-~2,'0D" day)
                             :type "txt")))
    (asdf:system-relative-pathname "aoc2022" path)))

(defun input-file-data (day)
  "Return the content of an input file as a string."
  (let ((path (input-file-path day))
        (data (make-array 0 :element-type 'character :adjustable t))
        (block-size 4096)
        (offset 0))
    (with-open-file (file path :external-format :utf-8)
      (loop
       (let* ((capacity (array-dimension data 0))
              (nb-left (- capacity offset)))
         (when (< nb-left block-size)
           (setf data
                 (adjust-array data (+ capacity (- block-size nb-left))))))
       (let ((end (read-sequence data file :start offset)))
         (when (= end offset)
           (return-from input-file-data (adjust-array data end)))
         (setf offset end))))))

(defun input-file-lines (day)
  "Return the content of an input files as a list of lines."
  (with-open-file (file (input-file-path day) :external-format :utf-8)
    (do ((path (input-file-path day))
         (lines nil))
        ((eq (car lines) 'eof)
         (nreverse (cdr lines)))
      (push (read-line file nil 'eof) lines))))
