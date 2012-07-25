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


(defmacro if-not-utf8-read-whole ((filename) &body body)
  `(if (utf8-encoded-p ,filename)
       (with-po-file (:filename ,filename)
	 ,@body)
       (with-po-file (:filename nil :buffer (slurp-file ,filename 
							:convert-to-string nil))
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
	(local-plural-function nil)
	(actual-filename (if *locale*
			     (format nil "~a~a~a~a~a~a~a.mo" 
				     *translation-file-root*
				     *directory-sep*
				     *locale*
				     *directory-sep*
				     *categories*
				     *directory-sep*
				     filename)
			     (format nil "~a~a~a" 
				     *translation-file-root*  
				     *directory-sep*
				     filename))))
    (cond
      ((scan +pofile-ext+ actual-filename)
       (if-not-utf8-read-whole (actual-filename)
	 (multiple-value-bind (hashtable plural-function errorsp errors)
	     (parse-po-file)
	   (if errorsp
	       (error 'conditions:parsing-pofile-error 
		      :text (format nil "~{~a~}" errors))
	       (progn
		 (setf local-plural-function plural-function)
		 (setf t-table hashtable))))))
      ((scan +lisp-table-ext+ actual-filename)
       (with-open-file (file actual-filename)
	 (setf local-plural-function (symbol-function (alexandria:format-symbol 'cl-i18n "~@:(~a~)"
										(read file))))
	 (setf t-table (translation-list->hash-table (read file) 
						     (make-hash-table :test 'equal)))))
      (t ;;maybe a MO file?
       (with-mo-file (stream mofile actual-filename)
	 (parse-mofile mofile stream)
	 (if (not (null (parsing-errors mofile)))
	     (error 'conditions:parsing-mofile-error 
		    :text (format nil "~{~a~}" (parsing-errors mofile)))
	     (multiple-value-bind (hashtable plural-function)
		 (mofile->translation mofile)
	       (setf t-table hashtable)
	       (setf local-plural-function plural-function))))))
    
    (when update-translation-table
      (maphash #'(lambda (k v) (setf (gethash k *translation-table*) v)) 
	       *translation-table*))
    
    (when store-hashtable
      (setf *translation-table* t-table))
    (when store-plural-function
      (setf *plural-form-function* local-plural-function))
    (values t-table local-plural-function)))



(defun load-language (catalog &key (locale *locale*) (categories *categories*) (store-plural-function t))
  "Load a language that will be used for all subsequent translations."
  (let ((*locale* locale)
	(*categories* categories))
    (init-translation-table catalog :store-hashtable t 
			    :store-plural-function store-plural-function
			    :update-translation-table t)))


(defun translate (str)
  "Translate a string. This will raise an error if the translation table has not been
   initialized beforehand. If the string doesn't have a translation a warning
   is emitted as well and the original string returned."
  (restart-case
    (when (and (eql (hash-table-count *translation-table*) 0) (not *translation-collect*))
      (error 'conditions:no-translation-table-error :text "cl-i18n: translation table not initialized! Call \"load-language\" first."))
    (load-language (value &optional (store-plural t)) (load-language value :store-plural-function store-plural))
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
