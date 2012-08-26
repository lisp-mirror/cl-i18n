;; This software is Copyright (c) cage, 2012.
;; cage grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL

(in-package :utx-file)

(alexandria:define-constant +column+ "[^\\t\\n]+(\\t|\\n)" :test 'equalp)
(alexandria:define-constant +column-id+ "[0-9]+\\t" :test 'equalp)
(alexandria:define-constant +utx-ignored-line+ "(#.*\\r\\n)|(^\\p{white_space}+\\r\\n)|(^\\r\\n)" :test 'string=)
(alexandria:define-constant +line-stopper+ #\NewLine :test 'char=)
(alexandria:define-constant +status-forbidden+ "forbidden" :test 'string=)
(alexandria:define-constant +plural-field+ "src:plural" :test 'string=)
(alexandria:define-constant +status-field+ "term status" :test 'string=)


(defparameter *fields-position* (make-hash-table :test 'equal))

(defclass utx-parsed-file (cl-i18n:parsed-file) ())

(defgeneric last-comment-line-p (object))

(defmethod initialize-instance :after ((object utx-parsed-file) &key &allow-other-keys)
  (with-slots (comment-line) object
    (setf utx-file:comment-line +utx-ignored-line+)))


(defmethod peek-token ((object utx-parsed-file) &optional (test #'identity))
  (if (cl-i18n:peek-valid-stream)
      (multiple-value-bind (token start-token)
	  (cl-i18n:next-token object)
	(prog1
	    (funcall test token)
	  (cl-i18n:seek cl-i18n:*file* start-token)))
      nil))

(defmethod last-comment-line-p ((object utx-parsed-file))
  (cl-i18n:with-no-errors
    (multiple-value-bind (line length start)
	(cl-i18n:get-line cl-i18n:*file*)
      (declare (ignore length))
    (unwind-protect
	 (not (cl-i18n:is-comment-line-p object line))
      (cl-i18n:seek cl-i18n:*file* start)))))

(defmethod parse-comment-line ((object utx-parsed-file))
  (cl-i18n:with-no-errors
    (multiple-value-bind (line length start)
	(cl-i18n:get-line cl-i18n:*file*)
      (declare (ignore length))
      (if (and
	   (cl-i18n:is-comment-line-p object line)
	   (not (last-comment-line-p object)))
	  (progn
	    (parse-comment-line object))
	  (progn
	    (cl-i18n:seek cl-i18n:*file* start)
	    nil)))))
	
    


(cl-i18n:define-parser-skeleton utx utx-parsed-file 
   (*fields-position* (make-hash-table :test 'equal)))


(cl-i18n:define-tokenizer (utx-file:utx-parsed-file +column+))

(defun last-column-p (col)
  (char= (char col (1- (length col))) +line-stopper+))


(defun row-src (row)
  (first row))

(defun row-target (row)
  (second row))

(defmacro get-field (key row)
  (alexandria:with-gensyms (pos)
    `(let ((,pos (gethash ,key *fields-position*)))
       (if ,pos
	   (nth ,pos ,row)
	   nil))))

    
(defun row-status (row)
  (get-field +status-field+ row))

(defun row-plural (row)
  (get-field +plural-field+ row))

(defun status-forbidden-p (row)
  (string= +status-forbidden+ (row-status row)))


(cl-i18n:defnocfun parse-utx-file ()
  (cl-i18n:with-no-errors
    (if (cl-i18n:peek-valid-stream)
	(progn
	  (parse-utx-column-description)
	  (values (parse-utx-lines)
		  #'cl-i18n:english-plural-form 
		  cl-i18n:*has-errors* 
		  cl-i18n:*parsing-errors*))
	(values nil
		#'cl-i18n:english-plural-form 
		cl-i18n:*has-errors* 
		cl-i18n:*parsing-errors*))))


(defun parse-utx-column-description ()
  (let ((fields (trim-rows (parse-utx-line))))
    (loop for i from 0 below (length fields) do
	 (setf (gethash (nth i fields) *fields-position*) i))))


(defun min-column-number-p (row)
  (if (and row
	   (< (length row) 3))
      (progn
	(setf cl-i18n:*has-errors* t)
	(push (format nil "Error: utx row has less than 3 field: ~{~s~}" row)
	      cl-i18n:*parsing-errors*)
	nil)
      row))


(defun trim-rows (rows &optional (bag (format nil "~a~a~a" #\Tab #\Newline #\Return)))
  (mapcar #'(lambda (c) (string-trim bag c))
	  rows))

(defun parse-utx-lines (&optional (current-line '()) (entries (make-hash-table :test 'equal)))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
   (labels ((get-row ()
	      (cl-i18n:with-no-errors
		(let ((row (parse-utx-line)))
		  (if (min-column-number-p row)
		      (trim-rows row)
		      nil)))))
     #+sbcl
     (if (cl-i18n:peek-valid-stream)
	 (progn
	   (setf current-line (get-row))
	   (when current-line
	     (setf (gethash (row-src current-line) entries)
		   (cl-i18n:make-translation (row-target current-line)
					     cl-i18n:+translated-flag+
					     ""
					     (list (row-plural current-line)))))
	   (parse-utx-lines current-line entries))
	 entries)
     #-sbcl
     (do ((row (get-row) (get-row)))
	((not (cl-i18n:peek-valid-stream)) entries)
       (setf (gethash (row-src row) entries)
	    (cl-i18n:make-translation (row-target row)
				      cl-i18n:+translated-flag+
				      ""
				      (list (row-plural row)))))))

(defun parse-utx-line (&key (look-for-comment t))
  (when (and look-for-comment
	     (cl-i18n:peek-valid-stream))
    (parse-comment-line cl-i18n:*file*))
  (cl-i18n:with-no-errors
    (if (cl-i18n:peek-valid-stream)
	(let ((col (parse-utx-column)))
	  (if col 
	      (if (not (last-column-p col))
		  (append (list col) (parse-utx-line :look-for-comment nil))
		  (list col))))
	nil)))


(defun parse-utx-column ()
  (cl-i18n:with-no-errors
    (if (cl-i18n:peek-valid-stream)
	(cl-i18n:next-token cl-i18n:*file*)
	nil)))

