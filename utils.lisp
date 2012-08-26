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
(defparameter *valid-dir-rate-mofile-repo* 0.8)
(defparameter *valid-dir-average-mofile-repo* 40)
(defparameter *mofile-repo-exclude-path* '("\\/proc$" "\\/sys$" "\\/dev$"))
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

(defun is-mo-file-p (path &optional (ext "mo"))
  (handler-bind  ((nix:enoent
		    #'(lambda(e) 
			(declare (ignore e))
			(return-from is-mo-file-p nil))))
    (let ((st (nix:stat path)))
      (and st
	   (nix:s-isreg (nix:stat-mode st))
	   (cl-ppcre:scan (concatenate 'string "\\." ext "$") path)))))



(defun cat-parent-dir (parent direntry)
  (format nil "~a~a~a" parent *directory-sep* direntry))


(defmacro do-directory ((var) root &body body)
  (alexandria:with-gensyms (dir)

    `(let ((,dir (nix:opendir ,root)))
       (unwind-protect
	    (handler-case	    
		(do ((,var (cat-parent-dir ,root (nix:readdir ,dir)) 
			   (cat-parent-dir ,root (nix:readdir ,dir))))
		    ((cl-ppcre:scan "NIL$" ,var))
		  ,@body)
	      (nix::enotdir () 0)
	      (nix:eacces () 0)
	      (nix:eloop () 0))

       (nix:closedir ,dir)))))


(defun excluded-path-p (dir)
  (loop for i in *mofile-repo-exclude-path* do
       (if (cl-ppcre:scan i dir)
	   (return-from excluded-path-p t)))
  nil)

(defun count-mo-files-direct-children (root)
  (let ((count 0))
    (do-directory (file) root
      (when (is-mo-file-p file)
	(incf count)))
    count))




(defun count-mo-files-descend (root)
  (labels ((count-mo (parent dir-handle &optional (count 0))
	     (let ((ent (nix:readdir dir-handle)))
	       (if ent
		   (if (is-mo-file-p (cat-parent-dir parent ent))
		       (count-mo parent dir-handle (1+ count))
		       (count-mo parent dir-handle count))
		   count))))
		       
    (let ((dir (nix:opendir root)))
      (unwind-protect
	   (count-mo root dir)
	(nix:closedir dir)))))



(defun mo-repository-p (db)
  (let* ((dircount (/ (length db) 2))
	 (mofile-count (loop for i from 1 below (length db) by 2 collect (nth i db)))
	 (mofile-dir (loop for i from 0 below (length db) by 2 collect (symbol-name (nth i db))))
	 (mofile-last-dir (mapcar 
	  		   #'(lambda (dir) (car (last (cl-ppcre:split *directory-sep-regexp* dir))))
	  		   mofile-dir))
	 (mofile-valid-dir (remove-if #'null
				      (mapcar #'(lambda (dir) (cl-ppcre:scan-to-strings *valid-dir-mofile-repo* dir))
					      mofile-last-dir)))
	 (average-mofile-count (/ (reduce #'+ mofile-count :initial-value 0) dircount)))
    
    
    (and (> (/ (length mofile-valid-dir) dircount) *valid-dir-rate-mofile-repo*)
	 (> average-mofile-count *valid-dir-average-mofile-repo*))))
	 


(defun scan-mo-files (root &optional (db '()))
  (handler-case
      (let ((dir (nix:opendir root))
	    (direct-child-count 0))
	(unwind-protect
	     (progn
	       (do ((ent (cat-parent-dir root (nix:readdir dir)) 
			 (cat-parent-dir root (nix:readdir dir))))
		   ((cl-ppcre:scan "NIL$" ent))
		 (cond
		   ((cl-ppcre:scan "\\/\\.$" ent)
		    (incf direct-child-count (count-mo-files-direct-children root)))
		   ((not (cl-ppcre:scan "\\.\\.$" ent))
		    (handler-case
		    (let ((st (nix:stat ent)))
		      (when (nix:s-isdir (nix:stat-mode st))
			(let ((children-count (scan-mo-files ent db)))
			  (setf (getf db (alexandria:make-keyword ent)) children-count)
			  (incf direct-child-count children-count))))
		      (nix:enoent () nil)))))
	       (values direct-child-count db))
	  (nix:closedir dir)))
    (nix:enoent () (values 0 db))
    (nix:eacces () (values 0 db))
    (nix:eloop () (values 0 db))))


(defun count-mo-files (root)
  (do ((stack (list root))
       (count 0))
      ((null stack) count)
    (handler-case
	(let* ((dirname (pop stack))
	       (dir (nix:opendir dirname)))
	(unwind-protect
	     (do ((ent (cat-parent-dir dirname (nix:readdir dir)) 
		       (cat-parent-dir dirname (nix:readdir dir))))
		 ((cl-ppcre:scan "NIL$" ent))
	       (cond
		 ((cl-ppcre:scan "\\/\\.$" ent)
		  (incf count (count-mo-files-direct-children dirname)))
		 ((not (cl-ppcre:scan "\\.\\.$" ent))
		  (handler-case
		      (let ((st (nix:stat ent)))
			(when (and (nix:s-isdir (nix:stat-mode st))
				   (not (excluded-path-p ent)))
			  (push ent stack)))
		    (nix:enoent () nil)))))
	  (nix:closedir dir)))
      (nix:enotdir () 0)
      (nix:eacces () 0)
      (nix:eloop () 0))))
    

(defun search-mo-repository (root)
  (labels ((get-max-count-dir (root)
	     (let ((max-count (list "" 0)))
	       (do-directory (dir) root
		 (when (and (not (cl-ppcre:scan "\\/\\.$" dir))
			    (not (cl-ppcre:scan "\\.\\.$" dir))
			    (not (excluded-path-p dir)))
		   (let ((count (count-mo-files dir)))
		     (when (> count (second max-count))
		       (setf max-count (list dir count))))))
	       max-count)))

    (let ((catalog  (find-if #'(lambda (p) 
				 (mo-repository-p (second 
						   (multiple-value-list (scan-mo-files p)))))
			     *well-known-mofile-path*)))
      (if (not catalog)
	  (do ((dir (first (get-max-count-dir root)) (first (get-max-count-dir dir))))
	      ((mo-repository-p (second (multiple-value-list (scan-mo-files dir)))) dir))
	  catalog))))
      
      
      


