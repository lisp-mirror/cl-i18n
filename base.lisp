
(in-package :cl-i18n)

(export '(load-language translate *translation-file-root*))

(defvar *translation-file-root* "."
  "The directory where translation files are stored.
  Defaults to current directory.")

(defvar *translation-table* (make-hash-table :test 'equal))

(defun translation-list->hash-table (list ht)
  "Parse a list of the form (string delim translation) into a hash table,
  ignoring delim."
  (loop for str = (first list)
        and translation = (third list)
        do (progn
             (setf list (cdddr list))
             (setf (gethash str ht) translation)
             (format t "inserted ~A:~A~%" str translation))
        until (equal list nil))
  ht)

(defun init-translation-table (filename)
  "Load translations from a file, storing them in a hash table."
  (with-open-file (file filename)
    (setf *translation-table* (translation-list->hash-table (read file) *translation-table*))))

(defun load-language (lang)
  "Load a language that will be used for all subsequent translations."
  (init-translation-table (concatenate 'string
                                       *translation-file-root* "/" lang ".lisp")))

(defun translate (str)
  "Translate a string. This will warn if the translation table has not been
  initialized beforehand. If the string doesn't have a translation a warning
  is emitted as well and the original string returned."
  (if (eql (hash-table-count *translation-table*) 0)
    (warn "cl-i18n: translation table not initialized! Call “load-language” first."))
  (multiple-value-bind (translation found) (gethash str *translation-table*)
    (if (not found)
      (progn (warn "cl-i18n: no translation for ~S defined!" str) str)
      translation)))

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

(set-dispatch-macro-character #\# #\!
  #'(lambda (stream char1 char2)
      (declare (ignore char1 char2))
      (if (char= (read-char stream) #\")
        `(translate ,(read-lisp-string stream))
        (error "cl-i18n: the read macro `#!' must precede a double-quoted string!"))))

