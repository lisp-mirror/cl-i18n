;; This software is Copyright (c) cage, 2012.
;; cage grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL

(in-package :cl-i18n)

(alexandria:define-constant +stream-element-type+ '(unsigned-byte 8) :test 'equalp)

(alexandria:define-constant +string-num-byte-version-number+ 4 :test '=)

(alexandria:define-constant +string-num-byte-size+ 4 :test '=)

(alexandria:define-constant +offset-original-byte-size+ 4 :test '=)

(alexandria:define-constant +offset-translation-byte-size+ 4 :test '=)

(alexandria:define-constant +hashing-table-size-byte-size+ 4 :test '=)

(alexandria:define-constant +hashing-table-offset-byte-size+ 4 :test '=)

(alexandria:define-constant +original-strings-offset-size-chunk-size+ 8 :test '=)

(alexandria:define-constant +original-strings-offset-chunk-size+ 4 :test '=)

(alexandria:define-constant +original-strings-length-chunk-size+ 4 :test '=)

(alexandria:define-constant +translated-strings-offset-size-chunk-size+ 8 :test '=)

(alexandria:define-constant +translated-strings-offset-chunk-size+ 4 :test '=)

(alexandria:define-constant +translated-strings-length-chunk-size+ 4 :test '=)

(defun mo-magic-number-p (seq)
  (or (equalp seq +mo-file-magic-number+)
      (equalp seq (reverse +mo-file-magic-number+))))

(defun 2byte->word (byte1 byte2)
  (let ((res #x00000000))
    (boole boole-ior
	   (ash (boole boole-ior byte1 res) 8)
	   byte2)))

(defun 2word->int (word1 word2)
  (let ((res #x00000000))
    (boole boole-ior
	   (ash (boole boole-ior word1 res) 16)
	   word2)))

(defun byte->int (bytes)
  (let ((res #x0000000000000000))
    (loop
       for i in bytes and
       ct = 0 then (+ ct 8) do
	 (setf res
	       (boole boole-ior
		      (ash i ct)
		      res)))

    res))

(defclass mofile ()
  ((mofile
    :initform nil
    :accessor mofile)
   (magic-number
    :initform -1
    :accessor magic-number)
   (version-number
    :initform -1
    :accessor version-number)
   (string-number
    :initform -1
    :accessor string-number)
   (offset-original
    :initform -1
    :accessor offset-original)
   (offset-translations
    :initform -1
    :accessor offset-translations)
   (hashing-table-size
    :initform -1
    :accessor hashing-table-size)
   (hashing-table-offset
    :initform -1
    :accessor hashing-table-offset)
   (parsing-errors
    :initform nil
    :accessor parsing-errors)
   (original-strings
    :initform nil
    :accessor original-strings)
   (translated-strings
    :initform nil
    :accessor translated-strings)
   (pofile
    :initform nil
    :accessor pofile)))

(defmacro with-mo-file ((stream moclass mofile) &rest body)
  `(let ((,moclass (make-instance 'mofile)))
     (with-open-file (,stream ,mofile :direction :input :element-type +stream-element-type+ :if-does-not-exist :error)
       ,@body)))

(defgeneric parse-magic-number (object stream))

(defgeneric parse-original-strings (object stream))

(defgeneric parse-translated-strings (object stream))

(defgeneric parse-mofile (object stream))

(defgeneric mofile->pofile (object))

(defgeneric mofile->translation (object &optional originals translated
					plural-function translations))

(defmethod print-object :after ((object mofile) stream)
  (with-slots (magic-number version-number string-number offset-original offset-translations hashing-table-size hashing-table-offset original-strings translated-strings parsing-errors) object
    (print-unreadable-object (object stream :type nil :identity nil)
      (format stream "Magic number #x~x~%Version ~d~%string number ~d~%offset original #x~x~%offset translation #x~x~%hashing-table-size ~d~%hashing-table-offset #x~x~%original strings: ~s~%translated strings ~s~%errors ~s" magic-number version-number string-number offset-original offset-translations hashing-table-size hashing-table-offset original-strings translated-strings parsing-errors))))

(defmethod parse-magic-number ((object mofile) stream)
  (let* ((bytes (loop for i from 0 below (length +mo-file-magic-number+)
                   collect (read-byte stream)))
	 (magic-number (2word->int (2byte->word (fourth bytes) (third bytes))
				   (2byte->word (second bytes) (first bytes)))))
    (if (mo-magic-number-p bytes)
	(progn
	  (setf (magic-number object) magic-number)
	  (values magic-number object))
	(progn
	  (push (format nil "Invalid magic-number ~x instead of ~a"
                        magic-number
                        +mo-file-magic-number+)
		(parsing-errors object))
	  nil))))


(defmacro define-parse-header-chunk ((name size &optional (slot name)))
  (alexandria:with-gensyms (bytes res)
    `(progn
       (defgeneric ,(alexandria:format-symbol t "PARSE-~:@(~a~)" name) (mofile stream))
       (defmethod ,(alexandria:format-symbol t "PARSE-~:@(~a~)" name) ((object mofile) stream)
	 (let* ((,bytes (loop for i from 0 below ,size collect (read-byte stream)))
		(,res (byte->int ,bytes)))
	   ,(when (not (null slot))
		  `(setf (,slot object) ,res))
	   (values ,res object))))))

(define-parse-header-chunk (version-number +string-num-byte-version-number+))

(define-parse-header-chunk (string-number +string-num-byte-size+))

(define-parse-header-chunk (offset-original +offset-original-byte-size+))

(define-parse-header-chunk (offset-translations +offset-translation-byte-size+))

(define-parse-header-chunk (hashing-table-size +hashing-table-size-byte-size+))

(define-parse-header-chunk (hashing-table-offset +hashing-table-offset-byte-size+))

(define-parse-header-chunk (original-string-length +original-strings-length-chunk-size+ nil))

(define-parse-header-chunk (original-string-offset +original-strings-offset-chunk-size+ nil))

(define-parse-header-chunk (translated-string-length +translated-strings-length-chunk-size+ nil))

(define-parse-header-chunk (translated-string-offset +translated-strings-offset-chunk-size+ nil))

(defmacro with-parse-strings-chunks ((stream start-offset chunk-size whole-chunk-size parse-length-fun parse-offset-func) mofile)
  (alexandria:with-gensyms (pos strings str-len str-offset str-bytes orig-strings)
    `(let ((,strings '()))
       (if (file-position ,stream ,start-offset)
	   (progn
	     (do ((,pos ,start-offset (+ ,pos ,chunk-size)))
		 ((not (< ,pos ,whole-chunk-size))
		  (reverse ,strings))
	       (file-position ,stream ,pos)
	       (let* ((,str-len (,parse-length-fun ,mofile ,stream))
		      (,str-offset (,parse-offset-func ,mofile ,stream))
		      (,str-bytes (make-array ,str-len :element-type +stream-element-type+)))
		 (when-debug
		  (format t "string @ ~d length ~x offset ~x " ,pos ,str-len ,str-offset))
		 (file-position ,stream ,str-offset)
		 (read-sequence ,str-bytes ,stream)
		 (let ((,orig-strings (cl-ppcre:split "\\x0" (babel:octets-to-string ,str-bytes))))
		   (when-debug
		    (format t "val: ~s~%" ,orig-strings))
		   (push ,orig-strings ,strings)))))
	   (push (format nil "Invalid offset (~a) for original strings offset" ,start-offset)
		 (parsing-errors ,mofile))))))

(defmethod parse-original-strings ((object mofile) stream)
  (with-accessors ((offset-original offset-original)
		   (original-strings original-strings)) object

    (setf original-strings
	  (with-parse-strings-chunks (stream offset-original
					     +original-strings-offset-size-chunk-size+
					     (+ offset-original (* (string-number object) +original-strings-offset-size-chunk-size+))
					     parse-original-string-length
					     parse-original-string-offset) object))
    (values original-strings object)))


(defmethod parse-translated-strings ((object mofile) stream)
  (with-accessors ((offset-original offset-original)
		   (offset-translations offset-translations)
		   (translated-strings translated-strings)) object
    (let ((end-chunk (+ offset-translations
			(* (string-number object)
			   +original-strings-offset-size-chunk-size+))))
      (setf translated-strings
	  (with-parse-strings-chunks (stream offset-translations
					     +translated-strings-offset-size-chunk-size+
					     end-chunk
					     parse-translated-string-length
					     parse-translated-string-offset) object))
      (values translated-strings object))))

(defmethod parse-mofile ((object mofile) stream)
  (parse-magic-number object stream)
  (parse-version-number object stream)
  (parse-string-number object stream)
  (parse-offset-original object stream)
  (parse-offset-translations object stream)
  (parse-hashing-table-size object stream)
  (parse-hashing-table-offset object stream)
  (parse-original-strings object stream)
  (parse-translated-strings object stream))


(defun split-escape (msg)
  (if (null msg)
      (format nil "\"\"~%")
      (let ((splitted (cl-ppcre:split "\\n" msg)))
	(if (> (length splitted) 1)
	    (format nil "~{\"~a\\n\"~%~}" splitted)
	    (format nil "~{\"~a\"~%~}" splitted)))))


(defmethod mofile->pofile ((object mofile))
  (with-accessors ((pofile pofile)
		   (original-strings original-strings)
		   (translated-strings translated-strings)) object

    (labels ((concat (str)
	       (setf pofile
		     (concatenate 'string pofile str))))
      (let ((ct 0))
	(mapcar #'(lambda (orig)
		    (concat (format nil "~a ~a" +msgid+ (split-escape (first orig))))
		    (when (> (length orig) 1)
		      (mapcar #'(lambda (plur)
				  (concat (format nil "~a ~a" +msgid-plural+ (split-escape plur))))
			      (rest orig)))
		    (let ((ct-pl 0))
		      (if (> (length (nth ct translated-strings)) 1)
			  (mapcar #'(lambda(plur)
				      (concat (format nil "~a[~a] ~a" +msgstr+ ct-pl (split-escape plur)))
				      (incf ct-pl))
				  (nth ct translated-strings))
			  (concat (format nil "~a ~a~%" +msgstr+ (split-escape (first (nth ct translated-strings)))))))
		    (concat (format nil "~%"))
		    (incf ct))
		original-strings)
	(values pofile object)))))

(defmethod mofile->translation ((object mofile) &optional
				(original (original-strings object))
				(translated (translated-strings object))
				(plural-function (extract-plural-function (first (first translated))))
				(translations (make-hash-table :test 'equal)))
  (if (null original)
      (values translations plural-function)
      (let ((translation (make-instance 'translation))
	    (orig (first original))
	    (transl (first translated)))

	(setf (translated translation) (first transl))
	(if (> (length orig) 1)
	    (setf (plural-form translation) (split-escape (second orig))))

	(do ((plural-form (rest transl) (rest plural-form)))
	    ((null plural-form) (setf (plural-translated translation)
				      (reverse (plural-translated translation))))
	  (push (first plural-form) (plural-translated translation)))
	(setf (gethash (first orig) translations) translation)
	(mofile->translation object (rest original) (rest translated) plural-function translations))))
