;; This software is Copyright (c) Leslie P. Polzer, 2011.
;; Leslie P. Polzer grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL

(require 'cl-i18n)
(in-package :cl-i18n)

(setf cl-i18n:*translation-file-root* "/examples/locale/");   change
							; accordingly
							; to        the
							; actual
							; location   of
							; the
							; directory
							; cointaining
							; the
							; translation
							; files




;; Or you can use (load-language italian)

(handler-bind ((conditions:no-translation-table-error
		#'(lambda(e)
		    (declare (ignore e))
		    (invoke-restart 'load-language "italian"))))

  (format t "~a ~a~%" 1 #!"apple")
  (format t "~a ~a~%" 1 #!"pie")
  (format t "~a ~a~%~%" 4 (cl-i18n:ntranslate "apple" "apples" 4)))



(cl-i18n:with-translation ((cl-i18n:init-translation-table
			   "/examples/locale/spanish.lisp"
			   :store-results nil :update-translation-table nil)
			   cl-i18n:n/=1-plural-form)

  (format t "~a ~a~%" 1 #!"apple")
  (format t "~a ~a~%" 1 #!"pie")
  (format t "~a ~a~%" 4 (cl-i18n:ntranslate "apple" "apples" 4)))
  
