(defpackage :aoc2022-09
  (:use :cl :aoc2022-utils)
  (:export
   :*lines*
   :solve-1
   :solve-2))

(in-package :aoc2022-09)

(defvar *data* (input-file-lines 9))

(deftype direction ()
  '(member :up :down :left :right))

(deftype move ()
  '(cons direction (integer 1)))

(deftype point ()
  '(cons integer integer))

(deftype rope ()
  'list)

(defun make-point (x y)
  (cons x y))

(defun make-rope (length)
  (map-into (make-list length) (lambda () (make-point 0 0))))

(defun parse-moves ()
  (mapcar (lambda (line)
            (cons (ecase (char line 0)
                    (#\U :up)
                    (#\D :down)
                    (#\L :left)
                    (#\R :right))
                  (parse-integer line :start 2)))
          *data*))

(defun move-point-toward-direction (point direction)
  (declare (type point point)
           (type direction direction))
  (destructuring-bind (x . y) point
    (ecase direction
      (:up
       (make-point x (1+ y)))
      (:down
       (make-point x (1- y)))
      (:left
       (make-point (1- x) y))
      (:right
       (make-point (1+ x) y)))))

(defun points-adjacent-p (p1 p2)
  (declare (type point p1 p2))
  (and (<= -1 (- (car p1) (car p2)) 1)
       (<= -1 (- (cdr p1) (cdr p2)) 1)))

(defun points-diagonal-p (p1 p2)
  (declare (type point p1 p2))
  (and (/= (car p1) (car p2))
       (/= (cdr p1) (cdr p2))))

(defun move-point (point dx dy)
  (declare (type point point)
           (type integer dx dy))
  (make-point (+ (car point) dx)
              (+ (cdr point) dy)))

(defun distance (p1 p2)
  (declare (type point p1 p2))
  (+ (abs (- (car p2) (car p1)))
     (abs (- (cdr p2) (cdr p1)))))

(defun move-knot (knot old-target new-target)
  (declare (type point knot old-target new-target))
  (if (points-adjacent-p knot new-target)
      knot
      (let* ((dx (- (car new-target) (car old-target)))
             (dy (- (cdr new-target) (cdr old-target)))
             (knot2 (move-point knot dx dy)))
        (when (points-diagonal-p knot2 new-target)
          (cond
            ((zerop dx)
             (setf (car knot2) (car new-target)))
            ((zerop dy)
             (setf (cdr knot2) (cdr new-target)))
            (t
             (let* ((p1 (make-point (car new-target) (cdr knot2)))
                    (p2 (make-point (car knot2) (cdr new-target)))
                    (d1 (distance knot p1))
                    (d2 (distance knot p2)))
               (cond
                 ((< d1 d2)
                  (setf knot2 p1))
                 ((> d1 d2)
                  (setf knot2 p2)))))))
        knot2)))

(defun compute-step (rope direction)
  (declare (type rope rope)
           (type direction direction))
  (do* ((head (car rope))
        (head2 (move-point-toward-direction head direction))
        (rope (cdr rope) (cdr rope))
        (rope2 (list head2))
        (old-target head)
        (new-target head2))
       ((null rope)
        (nreverse rope2))
    (let* ((knot (car rope))
           (knot2 (move-knot knot old-target new-target)))
      (push knot2 rope2)
      (setf old-target knot)
      (setf new-target knot2))))

(defun apply-moves (rope moves)
  (declare (type rope rope)
           (type list moves))
  (let ((visited-points (make-hash-table :test #'equal)))
    (setf (gethash (last rope) visited-points) t)
    (dolist (move moves (hash-table-count visited-points))
      (destructuring-bind (direction . n) move
        (dotimes (i n)
          (setf rope (compute-step rope direction))
          (setf (gethash (last rope) visited-points) t))))))

(defun solve-1 ()
  (apply-moves (make-rope 2) (parse-moves)))

(defun solve-2 ()
  (apply-moves (make-rope 10) (parse-moves)))
