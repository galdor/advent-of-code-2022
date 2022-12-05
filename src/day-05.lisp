(defpackage :aoc2022-05
  (:use :cl :aoc2022-utils)
  (:export
   :*lines*
   :solve-1
   :solve-2))

(in-package :aoc2022-05)

(defvar *lines* (input-file-lines 5))

(defun parse-stacks ()
  (let ((lines (subseq *lines* 0 (1- (position "" *lines* :test #'string=))))
        (stacks (make-array 9 :element-type 'list :initial-element nil)))
    (dolist (line (nreverse lines) stacks)
      (dotimes (i 9)
        (let ((crate (char line (1+ (* i 4)))))
          (unless (char= crate #\Space)
            (push crate (aref stacks i))))))))

(defun parse-moves ()
  (let ((lines (subseq *lines* (1+ (position "" *lines* :test #'string=))))
        (moves nil))
    (dolist (line lines (nreverse moves))
      (let ((fields (split-string line " ")))
        (push (list (parse-integer (nth 1 fields))
                    (1- (parse-integer (nth 3 fields)))
                    (1- (parse-integer (nth 5 fields))))
              moves)))))

(defun stacks-top-crates (stacks)
  (map-into (make-array 9 :element-type 'character) #'car stacks))

(defun solve-1 ()
  (let ((stacks (parse-stacks))
        (moves (parse-moves)))
    (dolist (move moves)
      (destructuring-bind (ncrates stack1 stack2) move
        (dotimes (i ncrates)
          (push (pop (aref stacks stack1))
                (aref stacks stack2)))))
    (stacks-top-crates stacks)))

(defun solve-2 ()
    (let ((stacks (parse-stacks))
          (moves (parse-moves)))
      (dolist (move moves)
        (destructuring-bind (ncrates stack1 stack2) move
          (setf (aref stacks stack2)
                (append (subseq (aref stacks stack1) 0 ncrates)
                        (aref stacks stack2)))
          (setf (aref stacks stack1)
                (subseq (aref stacks stack1) ncrates))))
      (stacks-top-crates stacks)))
