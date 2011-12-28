;; This software is Copyright (c) Leslie P. Polzer, 2011.
;; Leslie P. Polzer grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL

(in-package :cl-i18n)

(export '(load-language save-language translate
          *translation-file-root* *translation-collect*
          random-string))

(defvar *translation-file-root* "."
  "The directory where translation files are stored.
  Defaults to current directory.")

(defvar *translation-table* (make-hash-table :test 'equal))

(defun random-string (strings)
  (nth (random (list-length strings)) strings))

(defvar *translation-collect* nil)

(defun translation-hash-table->list (ht)
  (loop for key being the hash-keys of ht
	and value being the hash-values of ht
	collect (format nil "~s -> ~s~%" key value)))

(defun save-language (lang &optional destination)
  (with-open-file (file 
		    (or destination
                        (concatenate 'string *translation-file-root* "/" lang ".lisp"))
		    :if-does-not-exist :create
		    :if-exists :supersede
		    :direction :output)
    (format file "~a" (translation-hash-table->list *translation-table*))))

(defun translation-list->hash-table (list ht)
  "Parse a list of the form (string delim translation) into a hash table,
  ignoring delim."
  (loop for str = (first list)
        and translation = (third list)
        do (progn
             (setf list (cdddr list))
             (setf (gethash str ht) translation))
        until (equal list nil))
  ht)

(defun init-translation-table (filename)
  "Load translations from a file, storing them in a hash table."
  (with-open-file (file filename)
    (setf *translation-table* (translation-list->hash-table (read file) *translation-table*))))

(defun load-language (lang)
  "Load a language that will be used for all subsequent translations."
  (init-translation-table (concatenate 'string *translation-file-root* "/"
                                       (etypecase lang
                                         (string lang)
                                         (symbol (string-downcase (symbol-name lang))))
                                       ".lisp")))

(defun translate (str)
  "Translate a string. This will warn if the translation table has not been
  initialized beforehand. If the string doesn't have a translation a warning
  is emitted as well and the original string returned."
  (if (and (eql (hash-table-count *translation-table*) 0) (not *translation-collect*))
    (progn #+?(warn "cl-i18n: translation table not initialized! Call “load-language” first.")))
  (multiple-value-bind (translation found) (gethash str *translation-table*)
    (if (or (not found) (equal translation ""))
      (if *translation-collect*
        (setf (gethash str *translation-table*) str)
        (progn #+?(warn "cl-i18n: no translation for ~S defined!" str) str))
      (typecase translation
        (string translation)
        (cons (apply (first translation) (rest translation)))
        (t (format nil "~A" translation))))))

(defun read-lisp-string (input)
  "Parse a Lisp string. Expects “input” to point to the
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

(set-dispatch-macro-character #\# #\!
  #'(lambda (stream char1 char2)
      (declare (ignore char1 char2))
      (if (char= (read-char stream) #\")
        `(translate ,(read-lisp-string stream))
        (error "cl-i18n: the read macro `#!' must precede a double-quoted string!"))))

