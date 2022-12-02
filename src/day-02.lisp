(defpackage :aoc2022-02
  (:use :cl :aoc2022-utils)
  (:export
   :*lines*
   :solve-1
   :solve-2))

(in-package :aoc2022-02)

(defvar *lines* (input-file-lines 2))

(defun parse-strategy-1 ()
  (mapcar (lambda (line)
            (cons (ecase (char line 0)
                    (#\A :rock)
                    (#\B :paper)
                    (#\C :scissors))
                  (ecase (char line 2)
                    (#\X :rock)
                    (#\Y :paper)
                    (#\Z :scissors))))
          *lines*))

(defun parse-strategy-2 ()
  (mapcar (lambda (line)
            (cons (ecase (char line 0)
                    (#\A :rock)
                    (#\B :paper)
                    (#\C :scissors))
                  (ecase (char line 2)
                    (#\X :loss)
                    (#\Y :draw)
                    (#\Z :win))))
          *lines*))

(defun resolve-expected-round (round)
  (cons (car round)
        (cond
          ((eq (cdr round) :draw)
           (car round))
          ((eq (car round) :rock)
           (if (eq (cdr round) :win) :paper :scissors))
          ((eq (car round) :paper)
           (if (eq (cdr round) :win) :scissors :rock))
          ((eq (car round) :scissors)
           (if (eq (cdr round) :win) :rock :paper)))))

(defun round-result (round)
  (cond
    ((equal (car round) (cdr round))
     :draw)
    ((or (equal round '(:scissors . :rock))
         (equal round '(:rock . :paper))
         (equal round '(:paper . :scissors)))
     :win)
    (t
     :loss)))

(defun round-score (round)
  (let* ((result (round-result round))
         (outcome-score
           (ecase result
             (:loss 0)
             (:draw 3)
             (:win 6)))
         (shape-score
           (ecase (cdr round)
             (:rock 1)
             (:paper 2)
             (:scissors 3))))
    (+ outcome-score shape-score)))

(defun solve-1 ()
  (let ((score 0))
    (dolist (round (parse-strategy-1) score)
      (incf score (round-score round)))))

(defun solve-2 ()
  (let ((score 0))
    (dolist (round (parse-strategy-2) score)
      (incf score (round-score (resolve-expected-round round))))))
