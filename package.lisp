;; This software is Copyright (c) Leslie P. Polzer, 2011.
;; Leslie P. Polzer grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL

(defpackage :i18n-conditions (:use 
			      :cl)
	    (:export
	     :no-translation-table-error
	     :parsing-pofile-error
	     :parsing-mofile-error
	     :parsing-utxfile-error
	     :no-translation
	     :out-of-bounds))


(defpackage :cl-i18n (:use 
		      :cl
		      :alexandria
		      :cl-ppcre)
	    (:export
	     :define-tokenizer
	     :define-parser-skeleton
	     :parsed-file
	     :defnocfun
	     :with-error
	     :with-no-errors
	     :peek-valid-stream
	     :is-comment-line-p
	     :next-token
	     :comment-line
	     :*file*
	     :*has-errors*
	     :*parsing-errors*
	     :peek-token
	     :parse-comment-line
	     :seek
	     :get-line

	     :translation
	     :translated
	     :flag
	     :make-translation
	     :define-plural-form
	     :n/=1-plural-form
	     :english-plural-form 
	     :german-plural-form
	     :dutch-plural-form 
	     :swedish-plural-form 
	     :danish-plural-form 
	     :norwegian-plural-form 
	     :faroese-plural-form 
	     :spanish-plural-form 
	     :portuguese-plural-form 
	     :italian-plural-form 
	     :bulgarian-plural-form 
	     :greek-plural-form 
	     :finnish-plural-form 
	     :estonian-plural-form 
	     :hebrew-plural-form 
	     :esperanto-plural-form 
	     :hungarian-plural-form 
	     :turkish-plural-form 
	     :polish-plural-form 
	     :slovenian-plural-form 
	     :czech-plural-form 
	     :slovak-plural-form 
	     :russian-plural-form 
	     :ukrainian-plural-form 
	     :serbian-plural-form 
	     :croatian-plural-form 
	     :brazilian-plural-form 
	     :portuguese-plural-form 
	     :french-plural-form 
	     :latvian-plural-form 
	     :gaeilge-plural-form 
	     :irish-plural-form 
	     :romanian-plural-form 
	     :lithuanian-plural-form
	     :slurp-file
	     :translation-list->hash-table
	     :translation-hash-table->list
	     :init-translation-table
	     :load-language 
	     :save-language 
	     :translate
	     :ntranslate
	     :with-translation
	     :with-po-file
	     :with-mo-file
	     :*translation-file-root* 
	     :*translation-collect*
	     :*plural-form-function*
	     :*has-error*
	     :*parsing-errors*
	     :random-string
	     :+fuzzy-flag+
	     :+untranslated-flag+
	     :+translated-flag+
	     :+id+
	     :+translation+
	     :+plurals-form+
	     :+status+
	     :+plurals+
	     :*directory-sep-regexp*
	     :*directory-sep*)
	    (:documentation "An internationalisation framework for Common Lisp"))



(defpackage :utx-file (:use 
		       :cl
		       :alexandria
		       :cl-ppcre)
	    (:import-from :cl-i18n :peek-token)
	    (:import-from :cl-i18n :parse-comment-line)
	    (:import-from :cl-i18n :comment-line)
	    (:export
	     :utx-parsed-file
	     :is-comment-line-p
	     :comment-line
	     :next-token
	     :parse-utx-file
	     :with-utx-file))


(defpackage :cl-i18n-utils (:use 
			    :cl
			    :alexandria
			    :cl-ppcre)
	    (:export
	     :generate-i18n-file
	     :gen-translation-file
	     :convert-dictionary-format
	     :convert-save-dictionary))