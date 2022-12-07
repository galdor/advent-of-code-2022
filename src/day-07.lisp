(defpackage :aoc2022-07
  (:use :cl :aoc2022-utils)
  (:export
   :*lines*
   :solve-1
   :solve-2))

(in-package :aoc2022-07)

(defvar *data* (input-file-lines 7))

(defclass fs ()
  ((directory-stack
    :type list
    :initform nil
    :accessor fs-directory-stack)
   (root
    :type fs-node
    :accessor fs-root)))

(defclass fs-node ()
  ((name
    :type string
    :initarg :name
    :accessor fs-node-name)))

(defclass fs-directory (fs-node)
  ((children
    :type list
    :initform nil
    :accessor fs-directory-children)))

(defclass fs-file (fs-node)
  ((size
    :type (integer 0)
    :initarg :size
    :accessor fs-file-size)))

(defmethod initialize-instance :after ((fs fs) &key &allow-other-keys)
  (let ((root (make-instance 'fs-directory :name "/")))
    (setf (fs-root fs) root)
    (push root (fs-directory-stack fs))))

(defun fs-current-directory (fs)
  (car (fs-directory-stack fs)))

(defmethod print-object ((node fs-node) stream)
  (print-unreadable-object (node stream :type t)
    (write-string (fs-node-name node) stream)))

(defun fs-directory-has-child (directory name)
  (member name (fs-directory-children directory) :key 'fs-node-name
                                                 :test #'string=))

(defun fs-directory-child (directory name)
  (find name (fs-directory-children directory) :key 'fs-node-name
                                               :test #'string=))

(defun fs-directory-add-child (directory node)
  (push node (fs-directory-children directory)))

(defgeneric fs-node-total-size (node)
  (:method ((node fs-file))
    (fs-file-size node))
  (:method ((node fs-directory))
    (reduce #'+ (fs-directory-children node) :key 'fs-node-total-size)))

(defun walk-fs (function fs)
  (labels ((aux (node depth)
             (funcall function node depth)
             (when (typep node 'fs-directory)
               (dolist (child (fs-directory-children node))
                 (aux child (1+ depth))))))
    (aux (fs-root fs) 0)))

(defun walk-directories (function fs)
  (labels ((aux (node)
             (when (typep node 'fs-directory)
               (funcall function node)
               (dolist (child (fs-directory-children node))
                 (aux child)))))
    (aux (fs-root fs))))

(defun print-fs-tree (fs)
  (walk-fs (lambda (node depth)
             (format t "~V,,,V<~>~A~@[/~*~] ~D~%"
                     (* 2 depth) #\Space
                     (fs-node-name node)
                     (and (typep node 'fs-directory)
                          (string/= (fs-node-name node) "/"))
                     (fs-node-total-size node)))
           fs))

(defun parse-terminal-commands ()
  (do ((lines *data* (cdr lines))
       (commands nil)
       (command nil))
      ((endp lines)
       (nreverse commands))
    (let* ((line (car lines))
           (is-new-command (char= (char line 0) #\$)))
      (unless is-new-command
        (push (mapcar (lambda (part)
                        (if (every #'digit-char-p part)
                            (parse-integer part)
                            part))
                      (split-string line " "))
              command))
      (when (and command (or is-new-command (null (cdr lines))))
        (push (nreverse command) commands))
      (when is-new-command
        (setf command (list (split-string line " " :start 2)))))))

(defun eval-terminal-commands (commands)
  (let ((fs (make-instance 'fs)))
    (dolist (command commands)
      (eval-terminal-command command fs))
    fs))

(defun eval-terminal-command (command fs)
  (destructuring-bind ((op &rest args) &rest output) command
    (cond
      ((string= op "cd")
       (eval-terminal-command/cd (car args) fs))
      ((string= op "ls")
       (eval-terminal-command/ls output fs)
       )
      (t
       (error "unknown command ~S" op)))))

(defun eval-terminal-command/cd (name fs)
  (with-slots (directory-stack) fs
    (cond
      ;; Root directory
      ((string= name "/")
       (setf directory-stack (list (fs-root fs))))
      ;; Parent directory
      ((string= name "..")
       (pop directory-stack))
      ;; Child directory
      (t
       (let* ((current-directory (fs-current-directory fs))
              (child (fs-directory-child current-directory name)))
         (unless child
           (error "unknown child ~S in directory ~S" name current-directory))
         (push child directory-stack))))))

(defun eval-terminal-command/ls (output fs)
  (let ((current-directory (fs-current-directory fs)))
    (dolist (entry output)
      (cond
        ;; Directory
        ((equal (car entry) "dir")
         (let ((name (second entry)))
           (unless (fs-directory-has-child current-directory name)
             (let ((child (make-instance 'fs-directory :name name)))
               (fs-directory-add-child current-directory child)))))
        ;; File
        ((integerp (car entry))
         (let ((name (second entry)))
           (unless (fs-directory-has-child current-directory name)
             (let ((child (make-instance 'fs-file :name name :size
                                         (car entry))))
               (fs-directory-add-child current-directory child)))))
        (t
         (error "invalid ls output entry ~S" entry))))))

(defun solve-1 ()
  (let* ((commands (parse-terminal-commands))
         (fs (eval-terminal-commands commands))
         (sum 0))
    (walk-directories (lambda (node)
                        (let ((size (fs-node-total-size node)))
                          (when (<= size 100000)
                            (incf sum size))))
                      fs)
    sum))

(defun solve-2 ()
  (let* ((commands (parse-terminal-commands))
         (fs (eval-terminal-commands commands))
         (disk-size 70000000)
         (free-space-needed 30000000)
         (fs-size (fs-node-total-size (fs-root fs)))
         (free-space (- disk-size fs-size))
         (missing-space (- free-space-needed free-space))
         (min nil))
    (walk-directories (lambda (node)
                        (let ((size (fs-node-total-size node)))
                          (when (and (>= size missing-space)
                                     (or (null min) (< size min)))
                            (setf min size))))
                      fs)
    min))
