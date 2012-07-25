;; This software is Copyright (c) Leslie P. Polzer, 2011.
;; Leslie P. Polzer grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL

(in-package :cl-i18n)


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
    :initform '()
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
  (format stream "~a ~s~%~a ~s~%~a ~s~%~a ~s~%"
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
