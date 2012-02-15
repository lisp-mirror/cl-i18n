;; This software is Copyright (c) Leslie P. Polzer, 2011.
;; Leslie P. Polzer grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL

(require 'cl-i18n)
(in-package :cl-i18n)

(setf cl-i18n:*translation-file-root* "/cl-i18n/examples/locale/");   change
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



(setf cl-i18n:*plural-form-function* #'cl-i18n:italian-plural-form)

(handler-bind ((conditions:no-translation-table-error ;; Or just (load-language "italian")
		#'(lambda(e)
		    (declare (ignore e))
		    (invoke-restart 'load-language "italian"))))

  (format t "~a ~a~%" 1 #!"apple")
  (format t "~a ~a~%" 1 #!"pie")
  (format t "~a ~a~%~%" 4 (cl-i18n:ntranslate "apple" "apples" 4)))



(cl-i18n:with-translation ((cl-i18n:init-translation-table ;; or pass
							   ;; a
							   ;; previously
							   ;; loaded
							   ;; hashtable
							   ;; of
							   ;; course
			   "/cl-i18n/examples/locale/it.po"
			   :store-results nil :update-translation-table nil)
			   cl-i18n:*plural-form-function*) ;;loading a PO file
						           ;;also           set
						           ;;*plural-form-function*
  (format t "~a ~a~%" 1 #!"apple")
  (format t "~a ~a~%" 1 #!"pie")
  (format t "~a ~a~%" 4 (cl-i18n:ntranslate "pie" "pies" 4))
  (format t "~a ~a~%" 4 (cl-i18n:ntranslate "apple" "apples" 4))
  (format t "~a~%" #!"Invalid argument"))



