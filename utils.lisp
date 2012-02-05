;; This software is Copyright (c) Leslie P. Polzer, 2011.
;; Leslie P. Polzer grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL

(in-package :cl-i18n)


(defparameter *debug* t)
(alexandria:define-constant +pofile-ext+ "po$" :test 'string=)
(alexandria:define-constant +lisp-table-ext+ "lisp$" :test 'string=)

(defmacro when-debug (&body body)
  `(when (not (null *debug*))
     ,@body))
