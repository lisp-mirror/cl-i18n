;; This software is Copyright (c) Leslie P. Polzer, 2011.
;; Leslie P. Polzer grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL

(defpackage :conditions (:use 
			:cl)
	    (:export
	     :no-translation-table-error
	     :no-translation))

(defpackage :cl-i18n (:use 
		      :cl
		      :alexandria
		      :cl-ppcre)
	    (:export
	     :translation
	     :translated
	     :flag
	     :make-translation
	     :define-plural-form
	     :n/=1-plural-form
	     :english-plural-form
	     :italian-plural-form
	     :translation-list->hash-table
	     :translation-hash-table->list
	     :init-translation-table
	     :load-language 
	     :save-language 
	     :translate
	     :ntranslate
	     :with-translation
	     :*translation-file-root* 
	     :*translation-collect*
	     :random-string
	     :+fuzzy-flag+
	     :+untranslated-flag+
	     :+translated-flag+
	     :+id+
	     :+translation+
	     :+plurals-form+
	     :+status+
	     :+plurals+)
	    (:documentation "An internationalisation framework for Common Lisp"))



(defpackage :cl-i18n-utils (:use 
			    :cl
			    :alexandria
			    :cl-ppcre)
	    (:export 
	     :generate-i18n-file
	     :gen-translation-file
	     :convert-dictionary-format
	     :convert-save-dictionary))