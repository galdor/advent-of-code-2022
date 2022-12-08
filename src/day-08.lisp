(defpackage :aoc2022-08
  (:use :cl :aoc2022-utils)
  (:export
   :*lines*
   :solve-1
   :solve-2))

(in-package :aoc2022-08)

(defvar *data* (input-file-lines 8))

(defun parse-grid ()
  (let* ((height (length *data*))
         (width (length (car *data*)))
         (grid (make-array `(,height ,width) :element-type '(integer 0 9)))
         (y 0))
    (dolist (line *data* grid)
      (dotimes (x (length line))
        (setf (aref grid y x)
              (- (char-code (aref line x)) #.(char-code #\0))))
      (incf y))))

(defun grid-half-line (grid x y direction)
  (let ((width (array-dimension grid 1))
        (height (array-dimension grid 0))
        (values nil))
    (ecase direction
      (:up
       (do ((i 0 (1+ i))) ((>= i y)) (push (aref grid i x) values)))
      (:down
       (do ((i (1- height) (1- i))) ((<= i y)) (push (aref grid i x) values)))
      (:left
       (do ((i 0 (1+ i))) ((>= i x)) (push (aref grid y i) values)))
      (:right
       (do ((i (1- width) (1- i))) ((<= i x)) (push (aref grid y i) values))))
    values))

(defun tree-visible-p (grid x y)
  (flet ((max-height (direction)
           (apply #'max (or (grid-half-line grid x y direction) '(-1)))))
    (let ((tree (aref grid y x)))
      (or (> tree (max-height :up))
          (> tree (max-height :down))
          (> tree (max-height :left))
          (> tree (max-height :right))))))

(defun view-distance (grid x y direction)
  (let* ((tree (aref grid y x))
         (visible-trees (grid-half-line grid x y direction))
         (index (position-if (lambda (tree2) (>= tree2 tree)) visible-trees)))
    (if index
        (1+ index)
        (length visible-trees))))

(defun scenic-score (grid x y)
  (* (view-distance grid x y :up)
     (view-distance grid x y :down)
     (view-distance grid x y :left)
     (view-distance grid x y :right)))

(defun solve-1 ()
  (let ((grid (parse-grid))
        (count 0))
    (dotimes (y (array-dimension grid 0) count)
      (dotimes (x (array-dimension grid 1))
        (when (tree-visible-p grid y x)
          (incf count))))))

(defun solve-2 ()
  (let ((grid (parse-grid))
        (max-score 0))
    (dotimes (y (array-dimension grid 0) max-score)
      (dotimes (x (array-dimension grid 1))
        (setf max-score (max max-score (scenic-score grid x y)))))))
