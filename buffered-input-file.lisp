;; This software is Copyright (c) cage, 2012.
;; cage grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL

(in-package :cl-i18n)

(alexandria:define-constant +default-buffer-size+ 1000 :test #'=)
(alexandria:define-constant +default-element-type+ '(unsigned-byte 8) :test #'equal)


(defun make-buffer (&optional (length +default-buffer-size+))
  (make-array length :element-type +default-element-type+ 
	      :fill-pointer length :initial-element 0 :adjustable t))
  

(defun buffer-uchar-length (buffer)
  (babel:vector-size-in-chars buffer))



; file                      |b b b b b b b b b b b b b b b b b b b |
; buffer                            |c c c c c c c|
; buffer-position                   |c c c c c ^
; logical-file-position     |c c c c c c c c c ^
; file-position             |b b b b b b b b b b b ^
; uchars-count              |c c c c c c c c c c c ^


(defclass buffered-input-file ()
  ((filename
    :initarg :filename
    :initform nil
    :accessor filename)
   (buffer
    :initarg :buffer
    :initform (make-buffer)
    :accessor buffer)
   (logical-file-position
    :initarg :logical-file-position
    :initform 0
    :accessor logical-file-position)
   (uchars-count
    :initarg :uchars-count
    :initform 0
    :accessor uchars-count)
   (buffer-position
    :initarg :buffer-position
    :initform 0
    :accessor buffer-position)
   (inner-stream
    :initarg :inner-stream
    :initform nil
    :accessor inner-stream)
   (cached-string
    :initarg :cached-string
    :initform nil
    :accessor cached-string)
   (line-mode
    :initform nil
    :initarg :line-mode
    :accessor line-mode)
   (statistics
    :initarg :statistics
    :initform nil
    :accessor statistics)))




(defmethod print-object ((object buffered-input-file) stream)
  (format stream "buffer ~s bin ~a string ~s (length ~a) physical file position ~a buffer position ~a logical file-position ~a char-count ~a"

	  (map 'vector #'(lambda (v) (format nil "~x" v)) (buffer object))
	  (map 'vector #'(lambda (v) (format nil "~b" v)) (buffer object))
	  (concatenate 'string
		       (subseq (babel:octets-to-string (buffer object)) 0 
			       (buffer-position object))
		       #+sbcl "¶"
		       #-sbcl "*"
		       (subseq (babel:octets-to-string (buffer object)) 
			       (buffer-position object)))
	  
	    
	  (length (babel:octets-to-string (buffer object))) 
	  (actual-file-position object)
	  (buffer-position object)
	  (logical-file-position object)
	  (uchars-count object)))
  

(defmethod initialize-instance :after ((object buffered-input-file) &key &allow-other-keys)
  (with-accessors ((stream inner-stream) (buffer buffer) (filename filename)
		   (cached-string cached-string)
		   (inner-file-position inner-file-position)
		   (uchars-count uchars-count)) object
    (if filename
	(progn
	  (setf filename filename)
	  (setf stream (open filename :direction :input :element-type +default-element-type+
			     :if-does-not-exist :error))
	  (when (<= (stream-length object) (length buffer))
	    (setf buffer (make-buffer (stream-length object))))
	  (read-adjust-buffer object))
	(progn
	  (setf buffer (babel:string-to-octets buffer)
		cached-string (babel:octets-to-string buffer))))
    (setf uchars-count (length cached-string))))
	



(defgeneric stream-length (object))

(defgeneric actual-file-position (object &optional pos))

(defgeneric close-file (object))

(defgeneric valid-stream-p (object))

(defgeneric inside-buffer-p (object pos &key as-char))

(defgeneric outside-buffer-p (object pos))

(defgeneric replace-buffer (object &key direction))
(defgeneric replace-buffer-forward (object))

(defgeneric replace-buffer-backward (object))

(defgeneric truncate-buffer (object pos))

(defgeneric enlarge-buffer (object))

(defgeneric adjust-buffer (object))
(defgeneric adjust-buffer-backward (object))

(defgeneric read-adjust-buffer (object))

  
(defgeneric regex-scan (object regex &optional sticky last-start last-end))

(defgeneric regex-scan-line-mode (object regex &optional sticky last-start last-end))

(defgeneric increment-pointer-then-get-char (object))
(defgeneric get-char-then-increment-pointer (object))
(defgeneric get-char (object))
(defgeneric get-line (object &key line-separator))
(defgeneric unget-char (object &optional position))

(defgeneric increment-pointer (object))
(defgeneric decrement-pointer (object))

(defgeneric seek (object pos))

(defmethod adjust-buffer ((object buffered-input-file))
  (with-accessors ((stream inner-stream) (buffer buffer)
		   (buffer-position buffer-position)
		   (cached-string cached-string)
		   (uchars-count uchars-count)
		   (logical-file-position logical-file-position)) object
    (let* ((last-leading (do ((ct (1- (length buffer)) (1- ct)))
			     ((> (uchar-length (elt buffer ct)) 0)
			      ct)))
	   (uchar-size (uchar-length (elt buffer last-leading))))
      
      (loop for i from 0 below (- uchar-size (length (subseq buffer last-leading))) do
	   (vector-push-extend (read-byte stream) buffer))
      (setf cached-string (babel:octets-to-string buffer)))))


(defmethod adjust-buffer-backward ((object buffered-input-file))
  (with-accessors ((stream inner-stream) (buffer buffer)
		   (buffer-position buffer-position)
		   (uchars-count uchars-count)
		   (cached-string cached-string)
		   (logical-file-position logical-file-position)) object
    (let ((old-file-pos (actual-file-position object)))
      (actual-file-position object (- (actual-file-position object) (length buffer)))
      (do ((ct 0 (1+ ct)))
	  ((or
	    (> (uchar-length (elt buffer 0)) 0)
	    (> ct  10)))
	(if (> (file-position stream) 0)
	    (progn
	      (actual-file-position object (1- (actual-file-position object)))
	      (let* ((new-byte (read-byte stream))
		     (new-vector (make-buffer 1)))
		(setf (aref new-vector 0) new-byte)
		(setf buffer (concatenate '(vector (unsigned-byte 8)) new-vector buffer))
		(actual-file-position object (1- (actual-file-position object)))))
	    (error 'i18n-conditions:out-of-bounds :seq buffer :idx (actual-file-position object))))
      (setf cached-string (babel:octets-to-string buffer))
      (actual-file-position object old-file-pos))))



(defmethod read-adjust-buffer ((object buffered-input-file))
  (with-accessors ((stream inner-stream) (buffer buffer)
		   (buffer-position buffer-position)
		   (uchars-count uchars-count)
		   (logical-file-position logical-file-position)) object
    (read-sequence buffer stream)
    (adjust-buffer object)))
  

(defmethod close-file ((object buffered-input-file))
  (with-accessors ((stream inner-stream)) object
    (when stream
      (close stream))))


(defmacro with-ustring ((var object) &body body)
  `(let ((,var (if (null (cached-string ,object))
		   (babel:octets-to-string (buffer ,object))
		   (cached-string ,object))))
     ,@body))



(defmethod stream-length ((object buffered-input-file))
  (with-accessors ((stream inner-stream)
		   (buffer buffer)) object
    (if stream
	(file-length stream)
	(with-ustring (ustring object)
	  (length ustring)))))


(defmethod actual-file-position ((object buffered-input-file) &optional (pos nil))
  (with-accessors ((stream inner-stream)
		   (buffer buffer)) object
    (if stream
	(file-position stream pos)
	(length buffer))))

(defmethod valid-stream-p ((object buffered-input-file))
  (with-accessors ((stream inner-stream)) object
    (if stream
	(< (logical-file-position object) (uchars-count object))
	(< (logical-file-position object) (stream-length object)))))

(defmethod inside-buffer-p ((object buffered-input-file) pos &key (as-char nil))
  (with-accessors ((buffer buffer)) object
    (and (>= pos 0)
	 (< pos (length (if as-char 
			    (cached-string object)
			    buffer))))))

(defmethod outside-buffer-p ((object buffered-input-file) pos)
  (not (inside-buffer-p object pos)))


(defmethod replace-buffer ((object buffered-input-file) &key (direction :forward))
  (ecase direction
    (:forward
     (replace-buffer-forward object))
    (:backward
     (replace-buffer-backward object))))


(defmacro with-file-position ((var object) &body body)
  `(let ((,var (actual-file-position ,object)))
     ,@body))




(defmethod replace-buffer-forward ((object buffered-input-file))
  (with-accessors ((stream inner-stream) (buffer buffer)
		   (buffer-position buffer-position)
		   (cached-string cached-string)
		   (uchars-count uchars-count)
		   (logical-file-position logical-file-position)) object
    (with-file-position (inner-file-position object)
      (if (< inner-file-position (stream-length object))
	  (progn
	    (if (< (+ inner-file-position +default-buffer-size+)
		   (stream-length object))
		(setf buffer (make-buffer))
		(setf buffer (make-buffer (- (stream-length object) inner-file-position))))
	    (setf buffer-position 0)
	    (read-adjust-buffer object)
	    (incf uchars-count (length cached-string)))
	  nil))))


(defmethod truncate-buffer ((object buffered-input-file) pos)
  (with-accessors ((stream inner-stream) (buffer buffer)
		   (buffer-position buffer-position)
		   (cached-string cached-string)
		   (uchars-count uchars-count)
		   (logical-file-position logical-file-position)) object
    (with-file-position (inner-file-position object)
      (if (< (+ inner-file-position +default-buffer-size+)
	     (stream-length object))
	  (setf buffer (make-buffer))
	  (setf buffer (make-buffer (- (stream-length object) inner-file-position))))
	  (setf buffer-position 0)
	  (read-adjust-buffer object)
	  (setf uchars-count (+ pos (length cached-string))))))
	  
  


(defmethod replace-buffer-backward ((object buffered-input-file))
  (with-accessors ((stream inner-stream) (buffer buffer)
		   (buffer-position buffer-position)
		   (uchars-count uchars-count)
		   (logical-file-position logical-file-position)) object
    (with-file-position (inner-file-position object)

      (if (and stream
	       (> logical-file-position 0)
	       (> inner-file-position 0))
	  (let* ((old-buffer-length (length buffer))
		 (old-buffer-length-char (buffer-uchar-length buffer))
		 (new-buffer-length
		  (if (> (- inner-file-position old-buffer-length +default-buffer-size+) 0)
		      +default-buffer-size+
		      (- inner-file-position old-buffer-length))))
	    (actual-file-position object (- inner-file-position old-buffer-length new-buffer-length))

	    (setf buffer (make-buffer new-buffer-length))
	    (read-sequence buffer stream)
	    (adjust-buffer-backward object)
	    (decf uchars-count old-buffer-length-char)
	    (setf new-buffer-length (length (cached-string object)))
	    (setf buffer-position (1- new-buffer-length))
	    logical-file-position)
	  (progn 
	    (setf buffer-position 0)
	    nil)))))




(defmethod enlarge-buffer ((object buffered-input-file))
  (with-accessors ((stream inner-stream) (buffer buffer) 
		   (cached-string cached-string)
		   (buffer-position buffer-position)
		   (uchars-count uchars-count)) object

      (with-file-position (inner-file-position object)
	(if (< inner-file-position (stream-length object))
	    (with-ustring (old-string object)
	      (let* ((old-buffer (alexandria:copy-array buffer))
		     (file-pos-inc
		      (if (< (+ inner-file-position +default-buffer-size+)
			     (stream-length object))
			  +default-buffer-size+
			  (- (stream-length object) inner-file-position)))
		     
		     (actual-length
		      (+ (length old-buffer) file-pos-inc)))
		(file-position stream (- (file-position stream)
					 (length old-buffer)))
		(decf uchars-count (length old-string))
		(setf buffer (make-buffer actual-length))
		(read-adjust-buffer object) ;; also set cached-string
		
		(incf uchars-count (length cached-string))
		buffer))
	    nil))))

      

(defmethod regex-scan ((object buffered-input-file)
		       regex &optional (sticky t)
		       (last-start nil) (last-end nil))
  (if (line-mode object)
      (regex-scan-line-mode object regex sticky last-start last-end)
      (with-accessors ((stream stream) (buffer buffer)
		       (logical-file-position logical-file-position) 
		       (buffer-position buffer-position)
		       (uchars-count uchars-count)) object
	(with-ustring (ustring object)
	  (multiple-value-bind (start end)
	      (cl-ppcre:scan regex ustring 
			     :start buffer-position)
	    (let ((all-buffer-length (- uchars-count (length ustring))))
	      (if (not start) ; match not found
		  (if (enlarge-buffer object)
		      (regex-scan object regex sticky last-start last-end)
		      (progn
			(replace-buffer object)
			(values nil nil nil)))
		  (if (or (not sticky)
			  (equal start buffer-position))
		      (if (and last-start
			       last-end
			       (= start last-start)
			       (= end last-end))
			  (values (subseq ustring start end) 
				  (+ start all-buffer-length)
				  (+ end all-buffer-length))

			  (if (enlarge-buffer object)
			      (regex-scan object regex sticky start end)
			      (values (subseq ustring start end) 
				      (+ start all-buffer-length)
				      (+ end all-buffer-length))))
		      (values nil nil nil)))))))))



(defmethod regex-scan-line-mode ((object buffered-input-file)
 				 regex &optional (sticky t)
 				 (last-start nil) (last-end nil))
  (declare (ignore last-start last-end))
  (multiple-value-bind (line line-length line-start)
      (cl-i18n:get-line object)
    (declare (ignore line-length))
    (unwind-protect
	 (multiple-value-bind (start end)
	     (cl-ppcre:scan regex line)
	   (if (not start) ; match not found
	       (values nil nil nil)
	       (if sticky
		   (if (= start 0) 
		       (progn
			 (values (subseq line start end) 
				 (+ line-start start)
				 (+ line-start start end)))
		       (values nil nil nil))
		   (progn
		     (values (subseq line start end) 
			     (+ line-start start)
			     (+ line-start start end))))))
      (cl-i18n:seek object line-start))))

	   



(defmethod get-char-then-increment-pointer ((object buffered-input-file))
  (let ((char (get-char object)))
    (when char
      (increment-pointer object))
    char))
	

(defmethod increment-pointer-then-get-char ((object buffered-input-file))
  (when (increment-pointer object)
    (get-char object)))


    

(defmethod get-char ((object buffered-input-file))
  (with-accessors ((buffer buffer)
		   (buffer-position buffer-position)) object
    (with-ustring (uchar-buff object)
      (if (valid-stream-p object)
	  (elt uchar-buff buffer-position)
	  nil))))


(defmethod get-line (object &key (line-separator #\newline))
  (do* ((start-pos (logical-file-position object))
	(count 0 (1+ count))
	(read (get-char-then-increment-pointer object)
	      (get-char-then-increment-pointer object))
	(line (string read) (concatenate 'string line (string read))))
      ((or (not read) 
	   (char= read line-separator)) 
       (values line count start-pos))))
  


(defmethod unget-char ((object buffered-input-file)
		       &optional (position (1- (buffer-position object))))
  (with-accessors ((stream stream) (buffer buffer)
		   (logical-file-position logical-file-position) 
		   (buffer-position buffer-position)) object
    (with-ustring (ubuffer object)
      (cond 
	((inside-buffer-p object position)
	 (prog1
	     (elt ubuffer position)
	   (decf logical-file-position)
	   (setf buffer-position position)))
	(t
	 (if (replace-buffer object :direction :backward)
	     (unget-char object (buffer-position object))
	     nil))))))


(defmethod increment-pointer ((object buffered-input-file))
  (with-accessors ((logical-file-position logical-file-position)
		   (buffer buffer)
		   (uchars-count uchars-count)
		   (buffer-position buffer-position)) object
    (let ((saved-bufferpos buffer-position)
	  (saved-filepos logical-file-position)
	  (buffer-length (length (cached-string object))))
      (if (valid-stream-p object)
	  (progn
	    (incf logical-file-position)
	    (incf buffer-position)
	    (when (not (< buffer-position buffer-length))
	      (replace-buffer object :direction :forward))
	    logical-file-position)
	  (progn
	    (setf buffer-position saved-bufferpos
		  logical-file-position saved-filepos)
	    nil)))))


(defmethod decrement-pointer ((object buffered-input-file))
  (with-accessors ((logical-file-position logical-file-position)
		   (buffer buffer)
		   (uchars-count uchars-count)
		   (buffer-position buffer-position)) object
    (let ((saved-bufferpos buffer-position)
	  (saved-filepos logical-file-position))
      (if (> logical-file-position 0)
	  (progn
	    (if (<= buffer-position 0)
		(when (replace-buffer object :direction :backward)
		  (decf logical-file-position))
		(progn
		  (decf logical-file-position)
		  (decf buffer-position)))
		
	    logical-file-position)
	  (progn
	    (setf buffer-position saved-bufferpos
		  logical-file-position saved-filepos)
	    nil)))))



(defmethod seek ((object buffered-input-file) pos)
  (with-accessors ((logical-file-position logical-file-position)
		   (uchars-count uchars-count)
		   (buffer buffer)
		   (buffer-position buffer-position)) object
    (if (filename object) ;; slow...
	(let ((uchar-diff (- pos logical-file-position)))
	  (when (/= uchar-diff 0)
	    (loop for i from 0 below (abs uchar-diff) do
		 (if (plusp uchar-diff)
		     (increment-pointer object)
		     (decrement-pointer object)))))
	(when (and (>= pos 0)
		   (< pos (length buffer)))
	  (setf buffer-position pos
		logical-file-position pos)))))
