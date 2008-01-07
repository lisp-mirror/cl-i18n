;;;; i18n-util.lisp
;;;; to use: (generate-i18n-file "foo.lisp" "your-language.lisp")

(defpackage :i18n-util
  (:use :cl :cl-ppcre))

(in-package :i18n-util)

(export 'generate-i18n-file)

(defun generate-i18n-file (source-filename localization-filename)
  "Read a Lisp source file, get all #! strings and generate the localization file."
  (let ((strings (get-strings source-filename)))
    (with-open-file (stream localization-filename :direction :output :if-exists :supersede)
      (format stream "(~%~{   ~S -> \"\"~%~})" strings))))

(defun slurp-file (filename)
  "A simple way to slurp a file."
  (with-open-file (stream filename :direction :input)
    (let ((seq (make-string (file-length stream))))
      (read-sequence seq stream)
      seq)))

(defun get-strings (file)
  "Uses CL-PPCRE to get all strings on the form #!\"foo\", and collect them on a list without duplication."
  (loop for each in (remove-duplicates (cl-ppcre:all-matches-as-strings "#!\".*\"" (slurp-file file)) :test #'string=)
     collect (cl-ppcre:regex-replace-all "(#!|\")" each "")))

