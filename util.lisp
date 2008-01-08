
(in-package :cl-i18n)

(export 'generate-i18n-file)

(defun generate-i18n-file (source-filename localization-filename)
  "Reads a Lisp source file, get all #! strings and generate the translation
  resource, or merge with it if the translation resource already exists."
  (let* ((i18n-table (read-i18n-file localization-filename))
         (strings (if (null i18n-table)
                      (get-strings source-filename)
                      (loop for each in (get-strings source-filename)
                         for hash-value = (gethash (car each) i18n-table)
                         collect (list (car each)
                                       (if (null hash-value)
                                           ""
                                           hash-value))))))
    (with-open-file (stream localization-filename :direction :output :if-exists :supersede)
      (format stream "(~%~{  ~{~S -> ~S~%~}~})~%" strings))))

(defun slurp-file (filename)
  "A simple way to slurp a file."
  (with-open-file (stream filename :direction :input)
    (let ((seq (make-string (file-length stream))))
      (read-sequence seq stream)
      seq)))

(defun get-strings (filename)
  "Uses CL-PPCRE to get all strings on the form #!\"foo\",
  and collect them uniquely in a list."
  (loop for each in (remove-duplicates (cl-ppcre:all-matches-as-strings "#!\".*\"" (slurp-file filename)) :test #'string=)
     collect (list (cl-ppcre:regex-replace-all "(#!|\")" each "") "")))

(defun read-i18n-file (filename)
  "Reads the i18n file, if it exists, and put the strings into a hash table"
  (when (probe-file filename)
    (with-open-file (stream filename)
      (translation-list->hash-table (read stream) (make-hash-table :test 'equal)))))

