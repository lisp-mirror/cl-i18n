;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; vim:set ft=lisp:
;; This software is Copyright (c) Leslie P. Polzer, 2011.
;; Leslie P. Polzer grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL


(defpackage #:cl-i18n-asd
  (:use :cl :asdf))

(in-package :cl-i18n-asd)

(defsystem cl-i18n
  :name "cl-i18n"
  :version "0.4.1"
  :maintainer "cage <cage@katamail.com>"
  :author "Leslie P. Polzer <polzer@stardawn.org>, Vilson Vieira <vilson@void.cc>, cage <cage@katamail.com>"
  :licence "LLGPL"
  :description "A gettext-style internationalisation framework for Common Lisp."
  :depends-on (:alexandria
	       :cl-ppcre
	       :osicat
	       :babel)
  :components ((:file "package")
	       (:file "utils"
		      :depends-on ("package"))
	       (:file "conditions"
		      :depends-on ("package"))
	       (:file "plural-forms"
		      :depends-on ("package"))
	       (:file "translation-class"
		      :depends-on ("plural-forms"))
	       (:file "pofile"
		      :depends-on ("utils"
				   "translation-class"))
	       (:file "mofile"
		      :depends-on ("pofile"))
               (:file "base" 
		      :depends-on ("utils"
				   "conditions"
				   "plural-forms"
				   "translation-class"
				   "mofile"))
               (:file "i18n-utils" 
		      :depends-on ("base"))))

