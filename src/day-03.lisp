(defpackage :aoc2022-03
  (:use :cl :aoc2022-utils)
  (:export
   :*lines*
   :solve-1
   :solve-2))

(in-package :aoc2022-03)

(defvar *lines* (input-file-lines 3))

(defun parse-rucksack-compartments (line)
  (let ((nb-items-per-compartment (/ (length line) 2)))
    (cons
     (subseq line 0 nb-items-per-compartment)
     (subseq line nb-items-per-compartment))))

(defun parse-rucksack-groups ()
  (do ((lines *lines* (cdddr lines))
       (groups nil))
      ((endp lines)
       (nreverse groups))
    (push (subseq lines 0 3) groups)))

(defun item-priority (item)
  (cond
    ((char<= #\a item #\z)
     (+ (- (char-code item) (char-code #\a)) 1))
    ((char<= #\A item #\Z)
     (+ (- (char-code item) (char-code #\A)) 27))))

(defun compartments-common-items (compartments)
  (remove-duplicates
   (nintersection (coerce (car compartments) 'list)
                  (coerce (cdr compartments) 'list))))

(defun rucksack-group-common-item (group)
  (car (reduce #'nintersection
               (mapcar (lambda (rucksack)
                         (coerce rucksack 'list))
                       group))))

(defun solve-1 ()
  (let ((rucksacks (mapcar 'parse-rucksack-compartments *lines*))
        (sum 0))
    (dolist (rucksack rucksacks sum)
      (let ((common-item (car (compartments-common-items rucksack))))
        (incf sum (item-priority common-item))))))

(defun solve-2 ()
  (let ((groups (parse-rucksack-groups))
        (sum 0))
    (dolist (group groups sum)
      (let ((common-item (rucksack-group-common-item group)))
        (incf sum (item-priority common-item))))))
