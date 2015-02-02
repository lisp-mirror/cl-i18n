;; This software is Copyright (c) Leslie P. Polzer, 2011.
;; Leslie P. Polzer grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL

(in-package :cl-i18n)

(defparameter *debug* nil)

(defparameter *directory-sep-regexp*
  #+windows "\\"
  #-windows "\\/")

(defparameter *directory-sep*
  #+windows "\\"
  #-windows "/")

(defparameter *valid-dir-mofile-repo* "(?i)^(([a-z][a-z])|([a-z][a-z]_[a-z][a-z]))$")

(defparameter *valid-dir-rate-mofile-repo* 0.7)

(defparameter *valid-dir-average-mofile-repo* 40)

(defparameter *mofile-repo-exclude-path* '("^\\/proc" "^\\/sys" "^\\/dev" "^\\/run"))

(defparameter *well-known-mofile-path* '("/usr/share/locale/" "/usr/local/share/locale/"))

(alexandria:define-constant +utx-ext+ "utx$" :test 'string=)

(alexandria:define-constant +pofile-ext+ "po$" :test 'string=)

(alexandria:define-constant +lisp-table-ext+ "lisp$" :test 'string=)

(defmacro when-debug (&body body)
  `(when (not (null *debug*))
     ,@body))

(defun slurp-file (filename &key (convert-to-string t))
  "A simple way to slurp a file."
  (with-open-file (stream filename :direction :input :element-type '(unsigned-byte 8))
    (let ((seq (make-array (file-length stream) :element-type '(unsigned-byte 8))))
      (read-sequence seq stream)
      (if convert-to-string
	  (babel:octets-to-string seq)
	  seq))))

(defun create-brand-new-file (file)
  (open file :direction :probe :if-does-not-exist :create))

(defun uchar-length (leading-byte)
  (let ((ones (do* ((ct 7 (1- ct))
		    (bit (ldb (byte 1 ct) leading-byte) 
			 (ldb (byte 1 ct) leading-byte))
		    (ones-ct 0))
		   ((= bit 0) ones-ct)
		(incf ones-ct))))
    (cond
      ((= ones 0)
       1)
      ((= ones 1)
       0)
      (t 
       ones))))

(defun utf8-encoded-p (file)
  (with-open-file (stream file :direction :input 
			  :if-does-not-exist :error 
			  ::element-type '(unsigned-byte 8))
    (let* ((leading-byte (read-byte stream))
	   (leading-byte-length (uchar-length leading-byte)))
      (cond 
	((= leading-byte-length 0)
	 nil)
	((> leading-byte-length 6)
	 nil)
	(t
	 (loop for i from 0 below (1- leading-byte-length) do
	      (let* ((ch (read-byte stream))
		     (ll (uchar-length ch)))
		(when (> ll 0)
		  (return-from utf8-encoded-p nil))))
	 t)))))

(defun pathname->string (path)
  (uiop/filesystem:native-namestring path))

(defun directoryp (d)
  (uiop/filesystem:directory-exists-p d))

(defun list-directory-entries (d)
  "Does not resolve symlinks to avoid loop"
  (and (directoryp d)
       (nconc
	(uiop/filesystem:subdirectories d)
	(uiop/filesystem:directory-files d))))

(defun remove-regular-files (entries)
  (remove-if #'(lambda (a) (not (directoryp a))) entries))

(defun is-mo-file-p (path &optional (ext "mo"))
    (and (uiop/filesystem:file-exists-p path)
	 (cl-ppcre:scan (concatenate 'string "\\." ext "$") 
			(pathname->string path))))

(defun cat-parent-dir (parent direntry)
  (format nil "~a~a~a" parent *directory-sep* direntry))

(defmacro do-directory ((var) root &body body)
  `(and (directoryp ,root)
	(loop for ,var in (list-directory-entries root) do ,@body)))

(defun excluded-path-p (dir)
  (loop for i in *mofile-repo-exclude-path* do
       (if (cl-ppcre:scan i (pathname->string dir))
	   (return-from excluded-path-p t))))

(defun count-mo-files-direct-children (root)
  (let ((count 0))
    (do-directory (file) root
      (when (is-mo-file-p file)
	(incf count)))
    count))

(defun mo-repository-p (db)
  (if db
      (let* ((dircount (/ (length db) 2))
	     (mofile-count (loop for i from 1 below (length db) by 2 collect (nth i db)))
	     (mofile-dir (loop for i from 0 below (length db) by 2 collect (symbol-name (nth i db))))
	     (mofile-last-dir (mapcar 
			       #'(lambda (dir) (car (last (cl-ppcre:split *directory-sep-regexp* dir))))
			       mofile-dir))
	     (mofile-valid-dir (remove-if #'null
					  (mapcar #'(lambda (dir) 
						      (cl-ppcre:scan-to-strings 
						       *valid-dir-mofile-repo* dir))
						  mofile-last-dir)))
	     (average-mofile-count (/ (reduce #'+ mofile-count :initial-value 0) dircount)))
	(and (> (/ (length mofile-valid-dir) dircount) *valid-dir-rate-mofile-repo*)
	     (> average-mofile-count *valid-dir-average-mofile-repo*)))
      nil))

(defun scan-mo-files (root &optional (db '()) (max-depth 10))
  (let ((dirs (list-directory-entries root))
	(direct-child-count 0))
    (if (and dirs
	     (> max-depth 0))
	(progn
	  (incf direct-child-count (count-mo-files-direct-children root))
	  (loop for ent in dirs do
	       (when (directoryp ent)
		 (let ((children-count (scan-mo-files ent db (1- max-depth))))
		   (setf (getf db (alexandria:make-keyword (pathname->string ent)))
			 children-count)
		   (incf direct-child-count children-count))))
	  (values direct-child-count db))
	(values 0 db))))

(defun equals-path-fn ()
  #'(lambda (a b) (string= (pathname->string a) (pathname->string b))))

(defun count-mo-files (root)
  (let ((seen nil))
  (do ((stack (list root))
       (count 0))
      ((null stack) count)
    (let* ((dirname (uiop/filesystem:truenamize (pop stack)))
	   (dirs (remove-regular-files (list-directory-entries dirname))))
      (incf count (count-mo-files-direct-children dirname))
      (loop for ent in dirs do
	   (when (not (find ent seen :test (equals-path-fn)))
	     (push ent seen) 
	     (when (and (directoryp ent)
			(not (excluded-path-p ent)))
	       (push ent stack))))))))
    
(defun search-mo-repository (root &key (max-path-depth 10))
  (let ((seen nil)) 
    (labels ((get-max-count-dir (root)
	       (let ((max-count (list "" 0)))
		 (do-directory (dir) root
		   (when (and (directoryp dir)
			      (not (excluded-path-p dir))
			      (not (find dir seen :test (equals-path-fn))))
		     (push dir seen)
		     (let ((count (count-mo-files dir)))
		       (when (> count (second max-count))
			 (setf max-count (list dir count))))))
		 max-count)))
    (let ((catalog (find-if #'(lambda (p)
				(mo-repository-p
				 (second
				  (multiple-value-list (scan-mo-files p
								      '()
								      max-path-depth)))))
			    *well-known-mofile-path*)))
      (if (not catalog)
      	  (do ((dir (first (get-max-count-dir root)) (first (get-max-count-dir dir))))
      	      ((mo-repository-p (second (multiple-value-list (scan-mo-files dir
									    '()
									    max-path-depth))))
	       dir))
      	  catalog)))))


