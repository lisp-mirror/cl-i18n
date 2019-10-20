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

(defparameter *locale* nil)

(defparameter *categories* "LC_MESSAGES")

(defparameter *plural-form-function* #'n/=1-plural-form
  "This is the function used by the library to figure out the right plural form")

(defparameter *translation-table* (make-hash-table :test 'equal)
  "The actual translation table used, it is an hashtable with the original (untranslated) string as key and an instance of the class translation as value")

(defun translation-table ()
  *translation-table*)

(defun random-string (strings)
  (nth (random (list-length strings)) strings))

(defparameter *translation-collect* nil)

(defclass locale-definition ()
  ((language
    :initform nil
    :initarg  :language
    :accessor language)
   (territory
    :initform nil
    :initarg  :territory
    :accessor territory)
   (codeset
    :initform nil
    :initarg  :codeset
    :accessor codeset)
   (modifier
    :initform nil
    :initarg  :modifier
    :accessor modifier)))

(defmethod print-object ((object locale-definition) stream)
  (with-accessors ((language language) (territory territory)
		   (codeset codeset) (modifier modifier)) object
      (print-unreadable-object (object stream :type t :identity t)
	(format stream
		"lang: ~s terr: ~s codeset ~s modifier ~s"
		language territory codeset modifier))))

(defgeneric all-possible-locale-dir (object))

(defgeneric fittest-actual-locale-dir (object))

(defgeneric fittest-actual-locale-file (object filename))

(defmethod all-possible-locale-dir ((object locale-definition))
  (with-accessors ((language language) (territory territory)
		   (codeset codeset) (modifier modifier)) object
    (remove-duplicates
     (vector
      (format nil "~a_~a.~a~@[@~a~]" language territory codeset modifier)
      (format nil "~a_~a~@[.~a~]" language territory codeset)
      (format nil "~a~@[_~a~]" language territory)
      (format nil "~a" language))
     :test #'string=)))

(defmethod fittest-actual-locale-dir ((object locale-definition))
  (loop for p across (all-possible-locale-dir object) do
       (let ((path (format nil "~a~a~a~a~a"
			   *translation-file-root*
			   *directory-sep*
			   p
			   *directory-sep*
			   *categories*)))
	 (when (uiop:directory-exists-p path)
	   (return-from fittest-actual-locale-dir path))))
  nil)

(defmethod fittest-actual-locale-file ((object locale-definition) filename)
  (loop for p across (all-possible-locale-dir object) do
       (let ((path (format nil "~a~a~a~a~a~a~a.mo"
			   *translation-file-root*
			   *directory-sep*
			   p
			   *directory-sep*
			   *categories*
			   *directory-sep*
			   filename)))
	 (when (uiop:file-exists-p path)
	   (return-from fittest-actual-locale-file path))))
  nil)

(defun find-locale ()
  (let ((raw (or (uiop:getenvp "LC_ALL")
		 (uiop:getenvp *categories*)
		 (uiop:getenvp "LANG"))))
    (if raw
	(multiple-value-bind (match registers)
	    (cl-ppcre:scan-to-strings "([^_]+)_?([^\\.]+)?\\.?([^@]+)?@?(.+)?" raw)
	  (if match
	      (make-instance 'locale-definition
			     :language  (elt registers 0)
			     :territory (elt registers 1)
			     :codeset   (elt registers 2)
			     :modifier  (elt registers 3))
	      nil))
	nil)))

(defun save-language (lang &optional (destination nil)
		      (translation-table nil) (plural-function nil))
  "Save a translation table to a file, default path is *translation-file-root* \"/\" lang"
  (let ((output-file (or destination
			 (concatenate 'string
                                      *translation-file-root*
                                      *directory-sep* lang
                                      ".lisp"))))
    (create-brand-new-file output-file)
    (with-open-file (file output-file
			  :if-does-not-exist :create
			  :if-exists :supersede
			  :direction :output)
      (format file "~(~s~)~%~a"
	      (if plural-function
		  (symbol-name (get-function-name plural-function))
		  *plural-form-function*)
	      (translation-hash-table->list
	       (if translation-table
		   translation-table
		   *translation-table*))))))

(defmacro if-not-utf8-read-whole ((filename) &body body)
  `(if (utf8-encoded-p ,filename)
       (with-po-file (:filename ,filename)
	 ,@body)
       (with-po-file (:filename nil :buffer (slurp-file ,filename
							:convert-to-string t))
	 ,@body)))

(defun init-translation-table (filename &key
			                  (store-hashtable t)
			                  (store-plural-function t)
			                  (update-translation-table t))
  "Load translations from a file (*translation-file-root* is used as a
   prefix  for the actual  path), storing  them in  a hash  table.  if
   store-hashtable is  t *translation-table*  is setf'd to  the loaded
   table,  if  store-plural-function  is t  *plural-form-function*  is
   setf'd too. The *plural-form-function* is setf'd too"
  (let ((t-table (make-hash-table :test 'equal))
	(local-plural-function nil))
    (restart-case
        (let ((actual-filename (cond
			         ((typep *locale* 'locale-definition)
				  (or
				   (fittest-actual-locale-file *locale* filename)
				   ""))
			         ((typep *locale* 'string)
				  (format nil "~a~a~a~a~a~a~a.mo"
					  *translation-file-root*
					  *directory-sep*
					  *locale*
					  *directory-sep*
					  *categories*
					  *directory-sep*
					  filename))
			         (t
				  (format nil "~a~a~a"
					  *translation-file-root*
					  *directory-sep*
					  filename)))))
          (flet ((to-sexp-table ()
                   (handler-case
	               (with-open-file (file actual-filename :if-does-not-exist :error)
	                 (setf local-plural-function (symbol-function
					              (alexandria:format-symbol 'cl-i18n
								                "~@:(~a~)"
								                (read file))))
	                 (setf t-table (translation-list->hash-table (read file)
							             (make-hash-table
								      :test 'equal))))
	             (end-of-file () (setf local-plural-function #'english-plural-form
				           t-table (make-hash-table :test 'equal)))
	             (file-error () (progn
			              (create-brand-new-file actual-filename)
			              (setf local-plural-function #'english-plural-form
				            t-table (make-hash-table :test 'equal)))))))
	    (cond
	      ((scan +pofile-ext+ actual-filename)
	       (if-not-utf8-read-whole (actual-filename)
	         (multiple-value-bind (hashtable plural-function errorsp errors)
		     (parse-po-file)
	           (if errorsp
		       (error 'i18n-conditions:parsing-pofile-error
			      :text (format nil "~{~a~}" errors))
		       (progn
		         (setf local-plural-function plural-function)
		         (setf t-table hashtable))))))
	      ((scan +utx-ext+ actual-filename)
	       (utx-file:with-utx-file (:filename actual-filename)
	         (multiple-value-bind (hashtable plural-function errorsp errors)
		     (utx-file:parse-utx-file)
	           (if errorsp
		       (error 'i18n-conditions:parsing-utxfile-error
			      :text (format nil "~{~a~}" errors))
		       (progn
		         (setf local-plural-function plural-function)
		         (setf t-table hashtable))))))
	      ((scan +lisp-table-ext+  actual-filename)
               (to-sexp-table))
	    ((is-mo-file-p actual-filename :ext ".*" :test-magic-number t);;maybe a MO file?
	     (with-mo-file (stream mofile actual-filename)
	       (parse-mofile mofile stream)
	       (if (not (null (parsing-errors mofile)))
		   (error 'i18n-conditions:parsing-mofile-error
			  :text (format nil "~{~a~}" (parsing-errors mofile)))
		   (multiple-value-bind (hashtable plural-function)
		       (mofile->translation mofile)
		     (setf t-table hashtable)
		     (setf local-plural-function plural-function)))))
            (t
             (to-sexp-table)))
	    (when update-translation-table
	      (maphash #'(lambda (k v) (setf (gethash k *translation-table*) v))
		       *translation-table*))
	    (when store-hashtable
	      (setf *translation-table* t-table))
	    (when store-plural-function
	      (setf *plural-form-function* local-plural-function))
	    (values t-table local-plural-function)))
      (return-empty-translation-table ()
        (setf t-table (make-hash-table :test 'equal))))))

(defun load-language (catalog &key (locale *locale*) (categories *categories*)
		      (store-plural-function t) (store-hashtable t)
		      (update-translation-table t))
  "Load a language that will be used for all subsequent translations.
   Pass the evaluation results of (find-locale) to let the library guess the current locale.
   Use a locale string to explicitly set a locale instead."
  (let ((*locale* locale)
	(*categories* categories))
    (init-translation-table catalog
			    :store-hashtable store-hashtable
			    :store-plural-function store-plural-function
			    :update-translation-table update-translation-table)))

(defun translate (str)
  "Translate a string. This will raise an error if the translation table has not been
   initialized beforehand. If the string doesn't have a translation a warning
   is emitted as well and the original string returned."
  (restart-case
      (when (and (= (hash-table-count *translation-table*) 0)
                 (not *translation-collect*))
        (error 'i18n-conditions:no-translation-table-error
               :text "cl-i18n: translation table not initialized! Call \"load-language\" first."))
    (load-language (value &optional (store-plural t))
      (load-language value :store-plural-function store-plural))
    (use-value (value)
      (setf *translation-table* value))
    (return-untranslated ()
      str))
  (multiple-value-bind (translation found) (gethash str *translation-table*)
    (if (or (not found) (string= (translated translation) ""))
	(if *translation-collect*
	    (setf (gethash str *translation-table*) (make-translation str))
	    (progn
	      (warn 'i18n-conditions:no-translation
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
        (error "cl-i18n: the read macro '#!' must precede a double-quoted string!"))))

(set-dispatch-macro-character #\# #-lispworks #\§
                                  #+lispworks #\SECTION-SIGN
  #'(lambda (stream char1 num)
      (declare (ignore char1))
      `(cl-i18n:ntranslate ,(read stream) ,(read stream) ,num)))
