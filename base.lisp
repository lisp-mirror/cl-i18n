;; This software is Copyright (c) Leslie P. Polzer, 2011.
;; Leslie P. Polzer grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL

(in-package :cl-i18n)



(defparameter *translation-file-root* "."
  "The directory where translation files are stored.
  Defaults to current directory.")

(defparameter *plural-form-function* #'n/=1-plural-form
  "This is the function used by the library to figure out the right plural form")

(defparameter *translation-table* (make-hash-table :test 'equal)
  "The actual translation table used, it is an hashtable with the original (untranslated) string as key and an instance of the class translation as value")

(defun random-string (strings)
  (nth (random (list-length strings)) strings))

(defparameter *translation-collect* nil)


(defun save-language (lang &optional destination)
  "Save a translation table to a file, default path is *translation-file-root* \"/\" lang"
  (with-open-file (file 
		    (or destination
                        (concatenate 'string *translation-file-root* "/" lang ".lisp"))
		    :if-does-not-exist :create
		    :if-exists :supersede
		    :direction :output)
    (format file "~a" (translation-hash-table->list *translation-table*))))


(defun init-translation-table (filename &key (store-results t) (update-translation-table t))
  "Load translations from a file, storing them in a hash table.
   if store-results is t *translation-table* is setf'd to the loaded table
   If the file is in gettext po file format the *plural-form-function* is setf'd too"
  (let ((t-table (make-hash-table :test 'equal)))
    (cond
      ((scan +pofile-ext+ filename)
       (with-po-file ((slurp-file filename))
	 (multiple-value-bind (hashtable plural-function errorsp errors)
	     (parse-po-file)
	   (if errorsp
	       (error 'conditions:parsing-pofile-error :text (format nil "~{~a~}" errors))
	       (progn
		 (setf *plural-form-function* plural-function)
		 (setf t-table hashtable))))))
      ((scan +lisp-table-ext+ filename)
       (with-open-file (file filename)
	 (setf t-table (translation-list->hash-table (read file) 
						     (make-hash-table :test 'equal)))))
      (t ;;maybe a MO file?
       (with-mo-file (stream mofile filename)
	 (parse-mofile mofile stream)
	 (if (not (null (parsing-errors mofile)))
	     (error 'conditions:parsing-mofile-error :text (format nil "~{~a~}" (parsing-errors mofile)))
	     (progn
	       (mofile->pofile mofile)
	       (with-po-file ((pofile mofile))
		 (multiple-value-bind (hashtable plural-function)
		     (parse-po-file)
		   (setf *plural-form-function* plural-function)
		   (setf t-table hashtable))))))))
    
    (when update-translation-table
      (maphash #'(lambda (k v) (setf (gethash k *translation-table*) v)) 
	       *translation-table*))
    
    (if store-results
	(setf *translation-table* t-table)
	t-table)))



(defun load-language (lang &key (file-format "lisp"))
  "Load a language that will be used for all subsequent translations."
  (init-translation-table (concatenate 'string *translation-file-root* "/"
                                       (etypecase lang
                                         (string lang)
                                         (symbol (string-downcase (symbol-name lang))))
                                       "." file-format)
			  :store-results t
			  :update-translation-table t))


(defun translate (str)
  "Translate a string. This will raise an error if the translation table has not been
   initialized beforehand. If the string doesn't have a translation a warning
   is emitted as well and the original string returned."
  (restart-case
    (when (and (eql (hash-table-count *translation-table*) 0) (not *translation-collect*))
      (error 'conditions:no-translation-table-error :text "cl-i18n: translation table not initialized! Call \"load-language\" first."))
    (load-language (value) (load-language value))
    (use-value (value) (setf *translation-table* value)))
  (multiple-value-bind (translation found) (gethash str *translation-table*)
    (if (or (not found) (string= (translated translation) ""))
	(if *translation-collect*
	    (setf (gethash str *translation-table*) (make-translation str))
	    (progn
	      (warn 'conditions:no-translation 
		    :text (format nil "cl-i18n: no translation for ~S defined!" str))
	      str))
	(typecase translation
	  (translation (translated translation))
	  (string translation)
	  (cons (apply (first translation) (rest translation)))
	  (t (format nil "~A" translation))))))


(defun ntranslate (str1 str2 n)
  "Translate a string guessing a plural form.
   str1 is the string to be translated
   str2 is the fallback plural form
   n is the number of the objects
   First str1 is checked to get the translated object, if found 
   the nth element (as computed by the function *plural-form-function*) 
   of its plural-translated slot is used as plural form.
   If this index is less than 0 or more than the length of plural-translated
   ntranslate return str2.
   If the translation object does not exists str2 is returned"
  (let ((translation (gethash str1 *translation-table*)))
    (if (not (null translation))
	(let ((index (funcall *plural-form-function* n)))
	  (if (> index 0)
	      (let ((plural-translation (nth (1- index)
					     (plural-translated translation))))
		(if (not (null plural-translation))
		    plural-translation
		    str2))
	      (translated translation)))
	(if (= n 1)
	    str1
	    str2))))

	   
(defun read-lisp-string (input)
  "Parse a Lisp string. Expects \"input\" to point to the
  first character after the leading double quote.
  Slick version by Xach."
  (with-output-to-string (output)
    (loop
      (let ((char (read-char input)))
        (case char
          (#\\
           (setf char (read-char input)))
          (#\"
           (return)))
        (write-char char output)))))

;(when (get-dispatch-macro-character #\# #\@)
;    (error "Someone is already using #@"))

(defmacro with-translation ((translations plural-function) &body body)
  "Macro to switch between language at runtime"
  `(let ((*translation-table* ,translations)
	 (*plural-form-function* ,plural-function))
     ,@body))

(set-dispatch-macro-character #\# #\!
  #'(lambda (stream char1 char2)
      (declare (ignore char1 char2))
      (if (char= (read-char stream) #\")
        `(translate ,(read-lisp-string stream))
        (error "cl-i18n: the read macro `#!' must precede a double-quoted string!"))))

(set-dispatch-macro-character #\# #\§ 
  #'(lambda (stream char1 num)
      (declare (ignore char1))
      `(cl-i18n:ntranslate ,(read stream) ,(read stream) ,num)))
      

