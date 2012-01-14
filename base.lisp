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

(alexandria:define-constant +fuzzy-flag+ :fuzzy :test 'eq)
(alexandria:define-constant +untranslated-flag+ :untranslated :test 'eq)
(alexandria:define-constant +translated-flag+ :translated :test 'eq)


(alexandria:define-constant +id+ "id" :test 'string=)
(alexandria:define-constant +translation+ "translation" :test 'string=)
(alexandria:define-constant +plurals-form+ "plurals-form" :test 'string=)
(alexandria:define-constant +status+ "status" :test 'string=)
(alexandria:define-constant +plurals+ "plurals" :test 'string=)


(defclass translation ()
  ((translated
    :initform ""
    :initarg  :translated
    :accessor translated
    :type 'string
    :documentation "The translated string")
   (plural-form
    :initform ""
    :initarg  :plural-form
    :accessor plural-form
    :type 'string)
   (plural-translated
    :initform ""
    :initarg  :plural-translated
    :accessor plural-translated
    :type 'list
    :documentation "a list of string for each valid plural form")
   (flag
    :initform +untranslated-flag+
    :initarg  :flag
    :accessor flag
    :documentation "The status of the translation, can be one of +fuzzy-flag+ +untranslated-flag+ or +translation+"))
  (:documentation "The class that holds a translated string, its plural form and the translation status"))

(defmethod print-object ((object translation) stream)
  (format nil "~a ~s~%~a ~s~%~a ~s~%~a ~s~%"
	  +translation+ (translated object) 
	  +plurals-form+ (plural-form object)
	  +status+       (flag object)
	  +plurals+ (plural-translated object)))

(defmethod make-load-form ((object translation) &optional environment)
  (make-load-form-saving-slots object
			       :slot-names '(translated plural-form plural-translated flag)
			       :environment environment))

(defgeneric copy-translation (object old)
  (:documentation "Copy an instance of translation class from old to object"))

(defmethod copy-translation ((object translation) (old translation))
  (setf (translated object) (translated old))
  (setf (plural-form object) (plural-form old))
  (setf (plural-translated object) (copy-list (plural-translated old)))
  (setf (flag object) (flag old))
  object)
  

(defun make-translation (translation &optional (flag +untranslated-flag+)
			 (plural-form "") (plural-translated '()))
  "Create an instance of a translation class"
  (make-instance 'translation 
		 :translated translation
		 :flag       flag
		 :plural-form plural-form
		 :plural-translated plural-translated))

(defun translation-hash-table->list (ht)
  "Convert a translation table to a list with the format used to store the table in a file"
  (loop for key being the hash-keys of ht
	and value being the hash-values of ht
	collect (format nil "~a ~s~%~a ~s~%~a ~s~%~a ~s~%~a ~s~%"
			+id+ key 
			+translation+ (translated value) 
			+plurals-form+ (plural-form value)
			+status+       (flag value)
			+plurals+ (plural-translated value))))

(defun save-language (lang &optional destination)
  "Save a translation table to a file, default path is *translation-file-root* \"/\" lang"
  (with-open-file (file 
		    (or destination
                        (concatenate 'string *translation-file-root* "/" lang ".lisp"))
		    :if-does-not-exist :create
		    :if-exists :supersede
		    :direction :output)
    (format file "~a" (translation-hash-table->list *translation-table*))))

(defgeneric translation-list->hash-table (source dest))

(defmethod translation-list->hash-table ((list list) (ht hash-table))
  "Parse a list into a translation table."
  (when (and (> (length list) 0)
	 (= (mod (length list) 10) 0))
    (loop 
       for str = (nth 1 list)
       and translation = (nth 3 list)
       and plural-form = (nth 5 list)
       and flag        = (nth 7 list)
       and plurals     = (nth 9 list)
       do (progn
	    (setf list (subseq list 10))
	    (setf (gethash str ht) (make-translation translation flag plural-form plurals)))
       until (equal list nil)))
  ht)

(defun init-translation-table (filename &key (store-results t) (update-translation-table t))
  "Load translations from a file, storing them in a hash table.
   if store-results is t *translation-table* is setf'd to the loaded table"
  (with-open-file (file filename)
    (let ((t-table (translation-list->hash-table (read file)
						 (if update-translation-table
						     *translation-table*
						     (make-hash-table :test 'equal)))))
      (if store-results
	  (setf *translation-table* t-table)
	  t-table))))



(defun load-language (lang)
  "Load a language that will be used for all subsequent translations."
  (init-translation-table (concatenate 'string *translation-file-root* "/"
                                       (etypecase lang
                                         (string lang)
                                         (symbol (string-downcase (symbol-name lang))))
                                       ".lisp")
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
	 (*plural-form-function* #',plural-function))
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
      

