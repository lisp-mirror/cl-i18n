;; This software is Copyright (c) Leslie P. Polzer, 2011.
;; Leslie P. Polzer grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL

(in-package :cl-i18n)


(defparameter *debug* nil)

(alexandria:define-constant +pofile-ext+ "po$" :test 'string=)
(alexandria:define-constant +lisp-table-ext+ "lisp$" :test 'string=)

(defmacro when-debug (&body body)
  `(when (not (null *debug*))
     ,@body))


(defun slurp-file (filename)
  "A simple way to slurp a file."
  (with-open-file (stream filename :direction :input :element-type '(unsigned-byte 8))
    (let ((seq (make-array (file-length stream) :element-type '(unsigned-byte 8))))
      (read-sequence seq stream)
      (babel:octets-to-string seq))))
