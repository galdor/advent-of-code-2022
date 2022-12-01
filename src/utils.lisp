(defpackage :aoc2022-utils
  (:use :cl)
  (:export
   :*http-user-agent*
   :*firefox-profile-directory*
   :open-day-page
   :default-firefox-profile
   :advent-of-code-cookie
   :download-input-file
   :input-file-path
   :input-file-data
   :input-file-lines))

(in-package :aoc2022-utils)

(defparameter *http-user-agent*
  "https://github.com/galdor/advent-of-code-2022"
  "The user agent used for requests sent to the Advent of Code website.")

(defparameter *firefox-profile-directory*
  (merge-pathnames
   (make-pathname :directory '(:relative ".mozilla" "firefox"))
   (user-homedir-pathname))
  "The path of the directory containing Firefox profiles.")

(defun open-day-page (day)
  "Open the web page for a specific day in Firefox."
  (let* ((uri (format nil "https://adventofcode.com/2022/day/~D" day))
         (command `("firefox" ,uri)))
    (uiop:run-program command :force-shell t :output nil :error-output t)))

(defun default-firefox-profile ()
  "Return the name of the default Firefox profile."
  (let ((profiles-ini-path
          (merge-pathnames (make-pathname :name "profiles" :type "ini")
                           *firefox-profile-directory*))
        (prefix "Default="))
    (handler-case
        (with-open-file (file profiles-ini-path :external-format :utf-8)
          (loop
           (let ((line (read-line file)))
             (when (and (> (length line) (length prefix))
                        (string= line prefix :end1 (length prefix)))
               (return-from default-firefox-profile
                 (subseq line (length prefix)))))))
      (end-of-file (c)
        (declare (ignore c))
        (error "default profile name not found in ~A" profiles-ini-path)))))

(defmacro with-temporary-file-copy ((copy-path path) &body body)
  "Evaluate BODY with COPY-PATH bound to the path of a temporary file created as
a copy of the file at PATH. The temporary file is always deleted after BODY
has been evaluated."
  (let ((input (gensym "INPUT-"))
        (output (gensym "OUTPUT-"))
        (data (gensym "DATA-")))
    `(let ((,copy-path (make-pathname :name (pathname-name ,path) :type "tmp"
                                      :defaults ,path)))
       (unwind-protect
            (progn
              (with-open-file (,input ,path :element-type '(unsigned-byte 8))
                (with-open-file (,output ,copy-path
                                         :direction :output
                                         :if-exists :supersede
                                         :if-does-not-exist :create
                                         :element-type '(unsigned-byte 8))
                  (let ((,data (make-array
                                (file-length ,input)
                                :element-type (stream-element-type ,input))))
                    (read-sequence ,data ,input)
                    (write-sequence ,data ,output))))
              ,@body)
         (delete-file ,copy-path)))))

(defun advent-of-code-cookie ()
  "Return the value of the cookie for the Advent of Code website stored in the
default Firefox profile.

Because Firefox stupidly locks its SQLite databases, we have to copy the
entire database file before reading it. We cannot even use the sqlite .dump
command, because the file is locked. Fingers crossed."
  (let* ((profile (default-firefox-profile))
         (cookie-db-path
           (merge-pathnames (make-pathname :directory `(:relative ,profile)
                                           :name "cookies" :type "sqlite")
                            *firefox-profile-directory*)))
    (with-temporary-file-copy (tmp-path cookie-db-path)
      (let* ((domain ".adventofcode.com")
             (query (format nil "SELECT value FROM moz_cookies WHERE host='~A'"
                            domain))
             (command
               `("sqlite3" "-readonly" "-batch" "-list"
                           ,(namestring tmp-path) ,query)))
        (multiple-value-bind (output error-output status)
            (uiop:run-program command :force-shell t
                                      :output :string
                                      :error-output t)
          (declare (ignore error-output status))
          ;; The first line is the header line
          (let ((second-line-start (position #\Newline output)))
            (unless (and second-line-start
                         (< second-line-start (1- (length output))))
              (error "cannot parse sqlite output~%~%~A" output))
            (let ((end (position #\Newline output
                                 :start (1+ second-line-start))))
              (subseq output (1+ second-line-start) end))))))))

(defun download-input-file (day)
  (let* ((uri (format nil "https://adventofcode.com/2022/day/~D/input" day))
         (output-path (input-file-path day))
         (cookie (advent-of-code-cookie))
         (session (concatenate 'string "Cookie: session=" cookie))
         (user-agent (concatenate 'string "User-Agent: " *http-user-agent*))
         (command `("curl" "--silent"
                           "--show-error"
                           "--fail"
                           "--location"
                           "--header" ,session
                           "--header" ,user-agent
                           "--output" ,(namestring output-path)
                           ,uri)))
    (uiop:run-program command :force-shell t :output nil :error-output t)
    output-path))

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
