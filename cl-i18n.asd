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
  :version "0.5.4"
  :encoding :utf-8
  :maintainer "cage"
  :author "Leslie P. Polzer, Vilson Vieira, cage"
  :licence "LLGPL"
  :description "A gettext-style internationalisation framework for Common Lisp."
  :bug-tracker "https://notabug.org/cage/cl-i18n/issues"
  :depends-on (:alexandria
	       #-asdf3 :uiop
	       :cl-ppcre-unicode
	       :babel)
  :components ((:file "package")
	       (:file "function-name"
		:depends-on ("package"))
	       (:file "utils"
		:depends-on ("package"))
	       (:file "conditions"
		:depends-on ("package"))
	       (:file "buffered-input-file"
		:depends-on ("utils"
			     "conditions"
			     "package"))
	       (:file "plural-forms"
		:depends-on ("package"))
	       (:file "translation-class"
		:depends-on ("plural-forms"))
	       (:file "parser"
		:depends-on ("buffered-input-file"))

	       (:file "utx-file"
		:depends-on ("parser"
			     "translation-class"))
	       (:file "pofile"
		:depends-on ("parser"
			     "translation-class"))
	       (:file "mofile"
		:depends-on ("pofile"))
	       (:file "extraction-translatable-strings"
		:depends-on ("parser"))
               (:file "base"
		:depends-on ("plural-forms"
			     "translation-class"
			     "utx-file"
			     "mofile"
			     "extraction-translatable-strings"))
               (:file "i18n-utils"
		:depends-on ("base"))
               (:file "fuzzy-matching"
		:depends-on ("base"))))

(defsystem #:cl-i18n/tests
  :description "Unit tests for cl-i18n."
  :author "cage"
  :license "LLGPL"
  :serial t
  :components ((:file "test"))
  :depends-on (:cl-i18n
               :clunit2))
