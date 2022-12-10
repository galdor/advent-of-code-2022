(defpackage :aoc2022-10
  (:use :cl :aoc2022-utils)
  (:export
   :*lines*
   :solve-1
   :solve-2))

(in-package :aoc2022-10)

(defvar *data* (input-file-lines 10))

(defun parse-instructions ()
  (mapcar #'parse-instruction *data*))

(defun parse-instruction (string)
  (let* ((space (or (position #\Space string) (length string)))
         (name (subseq string 0 space)))
    (cond
      ((string= name "noop")
       (list 'noop))
      ((string= name "addx")
       (list 'addx (parse-integer string :start (1+ space)))))))

(defun simulate-cpu (instructions)
  (do ((cycle 1)
       (x 1)
       (x-values (make-array 0 :adjustable t :fill-pointer 0)))
      ((null instructions)
       x-values)
    (let ((instruction (pop instructions)))
      (ecase (car instruction)
        (noop
         (vector-push-extend x x-values)
         (incf cycle))
        (addx
         (vector-push-extend x x-values)
         (incf cycle)
         (vector-push-extend x x-values)
         (incf x (cadr instruction))
         (incf cycle))))))

(defun render-crt (instructions)
  (let ((x-values (simulate-cpu instructions)))
    (dotimes (i (length x-values))
      (let ((column (mod i 40)))
        (when (zerop column)
          (terpri))
        (let* ((x (aref x-values i))
               (sprite-visible-p (<= (1- column) x (1+ column))))
          (write-char (if sprite-visible-p #\# #\.)))))))

(defun solve-1 ()
  (let* ((instructions (parse-instructions))
         (x-values (simulate-cpu instructions))
         (sum 0))
    (dolist (c '(20 60 100 140 180 220) sum)
      (incf sum (* c (aref x-values (1- c)))))))

(defun solve-2 ()
  (render-crt (parse-instructions)))
