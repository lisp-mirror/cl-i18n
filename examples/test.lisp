;; Cl-i18n example
;; Copyright (C) 2012  cage

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(require 'cl-i18n)
(in-package :cl-i18n)

;;;;  Native file format example ;;;;
;; change accordingly to the actual location of the directory cointaining
;; the translation files
(let ((*translation-file-root* "cl-i18n/examples/locale/"))
  (handler-bind ((i18n-conditions:no-translation-table-error ;; Or just (load-language "italian.lisp")
		  #'(lambda(e)
		      (declare (ignore e))
		      (invoke-restart 'load-language "italian.lisp" nil))))
    (format t "~a ~a~%" 1 #!"apple")
    (format t "~a ~a~%" 1 #!"pie")
    (format t "~a ~a~%~%" 4 (cl-i18n:ntranslate "apple" "apples" 4))))


;;; PO file loading example
(let ((*translation-file-root* "cl-i18n/examples/locale/"))
  (cl-i18n:with-translation ((cl-i18n:init-translation-table ;; or pass
			                                     ;; a
							     ;; previously
							     ;; loaded
							     ;; hashtable
							     ;; of
							     ;; course
			      "it.po"
			      :store-hashtable nil 
			      :store-plural-function t 
			      :update-translation-table nil)
			     cl-i18n:*plural-form-function*) 
    (format t "~a ~a~%" 1 #!"apple")
    (format t "~a ~a~%" 1 #!"pie")
    (format t "~a ~a~%" 4 (cl-i18n:ntranslate "pie" "pies" 4))
    (format t "~a ~a~%" 4 (cl-i18n:ntranslate "apple" "apples" 4))
    (format t "~a~%~%" #!"Invalid argument")))


;; a way similar to GNU gettext style API; please note we are assuming
;; "/usr/share/locale/" as the path where the catalog can be found and
;; also foo.mo is there...
(let ((*translation-file-root* "/usr/share/locale/"))
   (load-language "foo" :locale "it")
   (format t "~a~%"  #!"Browse")
   (format t "~a~%" #!"Save as..."))

(setf *translation-file-root* "cl-i18n/examples/locale/")

(defparameter *italian-from-lisp* (multiple-value-list (load-language "italian.lisp"
								      :locale nil
								      :store-hashtable nil 
								      :store-plural-function nil
								      :update-translation-table nil)))



(defparameter *italian-from-po* (multiple-value-list (load-language "it.po" 
								    :locale nil 
								    :store-hashtable nil 
								    :store-plural-function nil
								    :update-translation-table nil)))




(defparameter *it-from-utx* (multiple-value-list (load-language "it.utx" 
								:locale nil 
								:store-hashtable nil 
								:store-plural-function nil
								:update-translation-table nil)))



;; runtime dictionary switching
(cl-i18n:with-translation ((first *italian-from-lisp*)
			   (second *italian-from-lisp*))

  (format t "translation from lisp~%~a ~a~%" 1 #!"apple")
  (format t "~a ~a~%" 1 #!"pie")
  (format t "~a ~a~%" 4 (cl-i18n:ntranslate "pie" "pies" 4))
  (format t "~a ~a~%" 4 (cl-i18n:ntranslate "apple" "apples" 4))
  (format t "~a~%~%" #!"Invalid argument")

  (cl-i18n:with-translation ((first *italian-from-po*)
			     (second *italian-from-po*))
    
    (format t "switching to po file ~%~a ~a~%" 1 #!"apple")
    (format t "~a ~a~%" 1 #!"pie")
    (format t "~a ~a~%" 4 (cl-i18n:ntranslate "pie" "pies" 4))
    (format t "~a ~a~%" 4 (cl-i18n:ntranslate "apple" "apples" 4))
    (format t "~a~%~%" #!"Invalid argument")))
      

(format t "UTX example~%")
(cl-i18n:with-translation ((first *it-from-utx*)
 			   (second *it-from-utx*))
  (format t "~a ~a~%" 4 (cl-i18n:ntranslate "apple" "apples" 4)))
