;; This software is Copyright (c) cage, 2012.
;; cage grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL

(in-package :cl-i18n)

(alexandria:define-constant +peek-length-tokenizer-on-error+ 6 :test 'equal)

(defparameter *file* "")

(defparameter *string-pos* 0)

(defparameter *has-errors* nil)

(defparameter *parsing-errors* '())

(defparameter *blank-space* '(#\space #\newline))

(defclass parsed-file (buffered-input-file)
  ((comment-line
    :initform nil
    :initarg :comment-line
    :accessor comment-line)))

(defgeneric peek-token (object &optional test))

(defgeneric peek-token-suppress-errors (object &optional test))

(defgeneric parse-comment-line (object))

(defgeneric is-comment-line-p (object line))

(defgeneric next-token (object &key hook-to-stringpos
				 return-first-match
				 predicate-sort-tokens
				 no-more-token-error))

(defgeneric next-token-simple (object &key no-more-token-error))

(defmacro with-error ((predicate msg &rest arg-predicate) &body body)
  `(if (apply ,predicate (list ,@arg-predicate))
       (progn ,@body)
       (progn
	 (setf *has-errors* t)
	 (push ,msg *parsing-errors*)
	 nil)))

(defmacro with-no-errors (&body body)
  `(when (not *has-errors*)
     ,@body))

(defmacro with-no-errors* (&body forms)
  (when forms
    `(with-no-errors
       ,(first forms)
       (with-no-errors* ,@(rest forms)))))

(defmacro let-noerr (forms &body body)
  (if (not (null forms))
      `(with-no-errors
	 (let (,(first forms))
	   (let-noerr ,(rest forms) ,@body)))
      `(progn ,@body)))

(defmacro let-noerr* (forms &body body)
  (if (not (null forms))
      `(with-no-errors
	 (let* (,(first forms))
	   (let-noerr ,(rest forms) ,@body)))
      `(progn ,@body)))

(defmacro with-valid-stream (&body body)
  `(with-error (#'peek-valid-stream "Attempt to read an empty stream")
     ,@body))

(defmethod peek-token ((object parsed-file) &optional (test #'identity))
  (with-valid-stream
    (multiple-value-bind (token start-token)
	(next-token object)
      (prog1
	  (funcall test token)
	(seek *file* start-token)))))

(defmethod peek-token-suppress-errors ((object parsed-file) &optional (test #'identity))
  (with-no-errors
    (multiple-value-bind (token start-token)
	(next-token object)
      (prog1
	  (funcall test token)
	(if token
	    (seek *file* start-token))))))

(defmethod parse-comment-line ((object parsed-file))
  (let-noerr ((peek (peek-token *file*)))
    (when (is-comment-line-p object peek)
      (next-token *file*)
      (parse-comment-line object))))

(defmethod is-comment-line-p ((object parsed-file) line)
   (if (comment-line object)
       (scan (comment-line object) line)
       nil))

(defmacro define-parser-skeleton (name classname &rest other-vars)
  (let ((macro-name (alexandria:format-symbol t "~:@(with-~a-file~)" name))
	(other-v other-vars))
    `(defmacro ,macro-name ((&key (buffer (make-buffer 2)) (filename nil)) &rest body)
       `(let ((*file* (make-instance ',',classname :buffer ,buffer :filename ,filename))
	      (*parsing-errors* '())
	      (*has-errors* nil)
	      ,@',other-v)

	  (unwind-protect
	       (progn ,@body)
	    (close-file *file*))))))

(defmacro define-parser-skeleton* (package name classname &rest other-vars)
  "does not close the stream"
  (let ((macro-name (alexandria:format-symbol package "~:@(with-~a-file~)" name))
	(other-v other-vars))
    `(defmacro ,macro-name ((&key (buffer (make-buffer 2)) (filename nil)) &rest body)
       `(let ((*file* (make-instance ',',classname :buffer ,buffer :filename ,filename))
	      (*parsing-errors* '())
	      (*has-errors* nil)
	      ,@',other-v)
	  (progn ,@body)))))

(defmacro define-is-stuff-p (test &rest operators)
  (alexandria:with-gensyms (str)
    `(progn
       ,@(mapcar #'(lambda (op)
		    `(defun ,(alexandria:format-symbol t "IS-~:@(~a~)-P"
						       (cl-ppcre:regex-replace-all "\\+|\\*"
										   (symbol-name op)
										   ""))
			 (,str)
		       (,test ,op ,str)))
		operators))))

(defun char@ ()
  (restart-case
      (let ((char (get-char *file*)))
	(if (not (null char))
	    (string char)
	    (error 'i18n-conditions:out-of-bounds :seq *file* :idx *string-pos*)))
    (ignore-error () ())
    (use-value (e) e)))

(defun char@1+ ()
  (restart-case
      (let ((char (get-char *file*)))
	(if (not (null char))
	    (progn
	      (increment-pointer *file*)
	      (string char))
	    (error 'i18n-conditions:out-of-bounds :seq *file* :idx *string-pos*)))
    (ignore-error () ())
    (use-value (e) e)))

(defun 1+char@ (&key (go-back t))
  (restart-case
      (progn
	(increment-pointer *file*)
	(let ((char (get-char *file*)))
	  (if (not (null char))
	      (progn
		(when go-back
		  (decrement-pointer *file*))
		(string char))
	      (error 'i18n-conditions:out-of-bounds :seq *file* :idx *string-pos*))))
    (ignore-error () ())
    (use-value (e) e)))

(defun peek-end-stream (&key (pos-offset 0))
  (let ((saved-pos (logical-file-position *file*)))
    (loop for i from 0 below (1- pos-offset) do (increment-pointer *file*))
    (prog1
	(not (increment-pointer *file*))
      (seek *file* saved-pos))))

(defun peek-valid-stream ()
  (not (peek-end-stream)))

(defmacro multiple-increment (times)
  `(progn
     ,@(loop for i from 0 below times collect
	    `(increment-pointer *file*))))

(defmacro scanner-re (re)
  `(cl-ppcre:create-scanner ,re))

(defun concatenate-regexps (regexps)
  (format nil "~{(~a)~^|~}" regexps))

(defmacro define-tokenizer-simple (classname &rest regexps)
  (alexandria:with-gensyms (scanner register-number match start-re end-re line-length line-start)
    (let ((class-name (alexandria:format-symbol t "~@:(~a~)" classname))
	  (no-more-token-error (alexandria:format-symbol t "NO-MORE-TOKEN-ERROR")))
      `(let ((,scanner (cl-ppcre:create-scanner ,(concatenate-regexps regexps))))
	 (defmethod next-token-simple ((object ,class-name) &key (,no-more-token-error t))
	   (declare (optimize (speed 3) (debug 0) (safety 0)))
	   (multiple-value-bind (,register-number ,line-start ,line-length ,match ,start-re
						  ,end-re)
	       (regex-scan-line-simple object ,scanner)
	     (declare (ignore ,line-start))
	     (declare ((signed-byte 64) ,register-number ,line-start ,line-length
	      	       ,start-re ,end-re))
	     (declare (simple-string ,match))
	       (if (>= ,register-number 0)
		   (progn
		     (seek *file* ,end-re)
		     (values ,match ,start-re))
		   (if (peek-end-stream :pos-offset 0)
		       (if ,no-more-token-error
			   (progn
			     (setf *has-errors* t)
			     (push "Error: stream ended without valid token found"
				   *parsing-errors*))
			   nil)
		       (handler-bind ((i18n-conditions:out-of-bounds
				       #'(lambda (c)
					   (declare (ignore c))
					   (invoke-restart 'ignore-error))))
			 (seek *file* ,line-length)
			 (char@1+)
			 (next-token-simple object
					    :no-more-token-error ,no-more-token-error))))))))))

(defmacro define-tokenizer ((classname &rest regexps) &body other-cond-clause)
  (alexandria:with-gensyms (scan tokens sorted-matches max-match)
    (let ((class-name (alexandria:format-symbol t "~@:(~a~)" classname)))
      `(defmethod next-token ((object ,class-name) &key
						     (hook-to-stringpos t)
						     (return-first-match nil)
						     (predicate-sort-tokens #'(lambda (a b)
										(> (length (first a))
										   (length (first b)))))
						     (no-more-token-error t))
	 (if (peek-valid-stream)
	     (let ((,tokens nil))
	       (cond
		 ,@other-cond-clause
		 (t
		  (block token-matching
		    ,@(loop for r in regexps collect
			   `(let ((,scan (multiple-value-list
					  (regex-scan *file*
						      ,r
						      hook-to-stringpos))))
			      (when (first ,scan)
				(if return-first-match
				    (progn
				      (setf ,tokens
					    (list
					     (first ,scan)  ; the token
					     (second ,scan) ; where the token starts
					     (third ,scan))) ; where the token ends
				      (return-from token-matching))
				    (push (list
					   (first ,scan)  ; the token
					   (second ,scan) ; where the token starts
					   (third ,scan)) ; where the token ends
					  ,tokens))))))
		  (if (not (null ,tokens))
		      (let* ((,sorted-matches (sort ,tokens predicate-sort-tokens))
			     (,max-match (first ,sorted-matches)))
			(seek *file* (third ,max-match))
			(values (first ,max-match) (second ,max-match)))
		      (if no-more-token-error
			  (if (peek-end-stream :pos-offset +peek-length-tokenizer-on-error+)
			      (progn
				(setf *has-errors* t)
				(push "Error: stream ended without valid token found" *parsing-errors*)
				(string (char@))
				nil)
			      (progn
				(setf *has-errors* t)
				(push (format nil
					      "Error: stream ended without valid token found starting from ~s"
					      (regex-scan *file*
							  ,(format nil "(?s).{~a}"
								   +peek-length-tokenizer-on-error+)
							  :sticky t))
				      *parsing-errors*)
				nil))
			  nil)))))
	     nil)))))

(defmacro defnocfun (name args &body body)
  `(defun ,(alexandria:format-symbol t "~:@(~a~)" name) (,@args)
     ,@(let ((user-rest (eq (caar body) 'declare)))
	    (append
	     (if user-rest
		 (list (car body))
		 nil)
	     (list
	      `(when (peek-valid-stream)
		 (parse-comment-line *file*)))
	     (list
	      (if user-rest
		  `(progn ,@(rest body))
		  `(progn ,@body)))))))
