;; This software is Copyright (c) Leslie P. Polzer, 2011.
;; Leslie P. Polzer grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL

(in-package :cl-i18n-utils)



(defun similar-phrase (phrase dict &key (threshold 3))
  "Scan the translation table looking for the best matching string of \"phrase\""
  (let ((min (list nil 1000)))
    (maphash #'(lambda (k v) 
		 (let ((dist (levenshtein-distance phrase k))
		       (trsl (cl-i18n:translated v)))
			 (if (and
			      (string/= trsl "")
			      (< dist threshold)
			      (< dist (second min)))
			     (setf min (list trsl dist)))))
	     dict)
    (first min)))
				      

(defun generate-i18n-file (source-filename localization-filename &key (fuzziness 3)
			   (plural-function #'cl-i18n:english-plural-form))
  "Reads a Lisp source file, get all #! strings and generate the translation
  resource, or merge with it if the translation resource already exists.
  Untranslated strings that show levenshtein distance less than :fuzziness 
  with a translated one get the translation of the latter; such translation 
  are marked as \"fuzzy\" in the output file"
  (let* ((path-splitted (cl-ppcre:split cl-i18n:*directory-sep-regexp* localization-filename))
	 (root (if (> (length path-splitted) 1)
		   (reduce #'(lambda (a b) (concatenate 'string a cl-i18n:*directory-sep* b))
			   (subseq path-splitted 0 (1- (length path-splitted))))
		   "."))
	 (output-filename (car (last path-splitted)))
	 (output-filename-noext (if (cl-ppcre:scan-to-strings ".*\\." output-filename)
				    (subseq (cl-ppcre:scan-to-strings ".*\\." output-filename)
					    0 (1- (length 
						   (cl-ppcre:scan-to-strings ".*\\." output-filename))))
				    output-filename))
	 (cl-i18n:*translation-file-root* root))

    (multiple-value-bind (i18n-table readed-plural-function)
	(cl-i18n:init-translation-table output-filename
					:store-hashtable nil
					:store-plural-function nil)
      (when (null readed-plural-function)
	(setf readed-plural-function plural-function))
      (let ((new-strings (get-strings source-filename)))
	(mapc #'(lambda (s) (when (not (gethash s i18n-table))
			      (let ((similar (similar-phrase s i18n-table :threshold fuzziness)))
				(setf (gethash s i18n-table)
				      (cl-i18n:make-translation (if (not (null similar))
								    similar
								    "")
								(if (not (null similar))
								    cl-i18n:+fuzzy-flag+
								    cl-i18n:+untranslated-flag+))))))
	      new-strings)
	(cl-i18n:save-language output-filename-noext nil i18n-table readed-plural-function)))))
	  



(defun get-strings (filename)
  "Uses CL-PPCRE to get all strings on the form #!\"foo\",
  and collect them uniquely in a list."
  (let* ((sfile (cl-i18n:slurp-file filename))
	 (matched (remove-duplicates (cl-ppcre:all-matches-as-strings "#!\".*?[^\\x5C]\"" sfile) :test #'string=))
	 (new-strings (mapcar #'(lambda (s) (let ((replaced (cl-ppcre:regex-replace-all "(#!|\\x5C)" s "")))
					      (subseq replaced 1 (1- (length replaced)))))
			      matched)))
    
    new-strings))


(defun read-i18n-file (filename)
  "Reads the i18n file, if it exists, and put the strings into a hash table"
  (if (probe-file filename)
    (with-open-file (stream filename)
      (cl-i18n:translation-list->hash-table (read stream) (make-hash-table :test 'equal)))
    (make-hash-table :test 'equal)))


(defun levenshtein-distance (string1 string2)
  "Compute the levenshtein distance (i. e. how much are similars) between two strings"
  (macrolet ((matrix-elt (mat i j)
	       `(nth ,j (nth ,i ,mat))))
    (labels ((gen-matrix (l1 l2)
	       (let ((mat (copy-tree (make-list (1+ l2) :initial-element (make-list (1+ l1) :initial-element -1)))))
		 (loop for i from 0 below (1+ l1) do (setf (matrix-elt mat 0 i) i))
		 (loop for i from 0 below (1+ l2) do (setf (matrix-elt mat i 0) i))
		 mat)))
      (let* ((l1 (length string1))
	     (l2 (length string2))
	     (mat (gen-matrix l1 l2)))
	(loop for i from 0 below l2 do
	     (loop for j from 0 below l1 do
		  (if (char= (char string2 i) (char string1 j))
		      (setf (matrix-elt mat (1+ i) (1+ j)) (matrix-elt mat i j))
		      (setf (matrix-elt mat (1+ i) (1+ j))
			    (min
			     (1+ (matrix-elt mat i (1+ j))) ; a deletion
			     (1+ (matrix-elt mat (1+ i) j)) ; an insertion
			     (1+ (matrix-elt mat i j))))))) ; a substitution
	
	(values (matrix-elt mat l2 l1) mat)))))

(defun gen-translation-file (path output &key (ext "lisp$"))
  "Scan a directory for sources files and collect all translatable strings. 
   The strings are merged with a translation file (if exists)"
  (mapc #'(lambda (f) (generate-i18n-file (namestring f) output))
	(remove-if-not #'(lambda (p) (cl-ppcre:scan ext (file-namestring p)))
		       (osicat:list-directory path))))


(defun convert-dictionary-format (old &key (plural-function 'cl-i18n:n/=1-plural-form))
  "Convert an 0.4 translation table file format to the new one"
  (with-open-file (istream old :direction :input :if-does-not-exist :error)
    (let ((old-format (read istream)))
      (format nil "~s~%(~%~a)~%"
	      (symbol-name plural-function)
	      (with-output-to-string (ostream)
		(loop 
		   for ct = 0 then (+ ct 3) while (< ct (length old-format)) do
		     (progn
		       (format ostream "~a ~s~%~a ~s~%~a ~s~%~a ~s~%~a ~s~%"
			       cl-i18n:+id+ (nth ct old-format)
			       cl-i18n:+translation+ (nth (+ 2 ct) old-format)
			       cl-i18n:+plurals-form+ (nth ct old-format)
			       cl-i18n:+status+ cl-i18n:+translated-flag+
			       cl-i18n:+plurals+ '()))))))))


(defun convert-save-dictionary (old new)
  "Convert an 0.4 translation table file format to the new one and save in a new file"
  (with-open-file (stream new :direction :output :if-exists :error)
    (princ (convert-dictionary-format old) stream)))