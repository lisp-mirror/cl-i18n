;; This software is Copyright (c) cage, 2012.
;; cage grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL

(in-package :cl-i18n)

;; expression := epsilon             |
;;               string   expression |
;;               function expression

;; string     := "^[^\\\"]+\""

;; function   := function-prefix string

;; function-prefix := "\\(_\\p{white_space}+\""  string

(defparameter *extr-function-re*                  "\\(_\\p{white_space}+\"")

(alexandria:define-constant +extr-string-re+      "^[^\\\"]*\""             :test #'string=)

(alexandria:define-constant +extr-escaped-stopper "\\\""                    :test #'string=)

(defclass extract-parsed-file (parsed-file) ())

(define-parser-skeleton extract-parsed extract-parsed-file)

;; (scan tokens sorted-matches max-match)

(defmethod next-token
           ((object extract-parsed-file)
            &key (hook-to-stringpos t) (return-first-match nil)
            (predicate-sort-tokens
             #'(lambda (a b) (< (length (first a)) (length (first b)))))
            (no-more-token-error t))
  (if (peek-valid-stream)
      (let ((tokens nil))
        (cond
         (t
          (block token-matching
            (let ((scan
                   (multiple-value-list
                    (regex-scan *file* *extr-function-re* hook-to-stringpos))))
              (when (first scan)
                (if return-first-match
                    (progn
                     (setf tokens
                             (list (first scan) (second scan)
                                   (third scan)))
                     (return-from token-matching))
                    (push
                     (list (first scan) (second scan)
                           (third scan))
                     tokens))))
            (let ((scan
                   (multiple-value-list
                    (regex-scan *file* +extr-string-re+ hook-to-stringpos))))
              (when (first scan)
                (if return-first-match
                    (progn
                     (setf tokens
			   (list (first scan) (second scan) (third scan)))
                     (return-from token-matching))
                    (push
                     (list (first scan) (second scan)
                           (third scan))
                     tokens)))))
          (if (not (null tokens))
              (let* ((sorted-matches (sort tokens predicate-sort-tokens))
                     (max-match      (first sorted-matches)))
                (seek *file* (third max-match))
                (values (first max-match) (second max-match)))
              (if no-more-token-error
                  (if (peek-end-stream :pos-offset
                                       +peek-length-tokenizer-on-error+)
                      (progn
                       (setf *has-errors* t)
                       (push "error: stream ended without valid token found"
                             *parsing-errors*)
                       (string (char@))
                       nil)
                      (progn
                       (setf *has-errors* t)
                       (push
                        (format nil
                                "error: stream ended without valid token found starting from ~s"
                                (regex-scan *file* "(?s).{6}" :sticky t))
                        *parsing-errors*)
                       nil))
                  nil)))))
      nil))

(defun %next-token ()
  (next-token *file* :no-more-token-error nil))

(defun just-peek-token ()
  (with-no-errors
    (multiple-value-bind (token start-token)
	(%next-token)
      (if token
	  (progn
	    (seek *file* start-token)
	    token)
	  nil))))

(define-is-stuff-p cl-ppcre:scan *extr-function-re* +extr-string-re+)

(defun escaped-stopper-p (str)
  (and (> (length str) 1)
       (string= +extr-escaped-stopper str
		:start1 0
		:end1   (length +extr-escaped-stopper)
		:start2 (- (length str) 2)
		:end2   (length str))))

(defun parse-delimited-string ()
  (labels ((%cat-string ()
	     (cl-i18n:with-no-errors
	       (let ((token (%next-token)))
		 (if (is-extr-string-re-p token)
		     (if (escaped-stopper-p token)
			 (concatenate 'string token (%cat-string))
			 token)
		     (progn
		       (push (format nil
				     "Error: expected delimited string (re: '.*\"') ~a found instead"
				     token)
			     *parsing-errors*)
		       nil))))))
    (%cat-string)))

(defun parse-function ()
  (cl-i18n:with-no-errors
    (parse-function-prefix)
    (parse-delimited-string)))

(defun parse-function-prefix ()
  (cl-i18n:with-no-errors
    (let ((token (%next-token)))
      (when (not (is-extr-function-re-p token))
	(push (format nil "Error: expected trandslation function name ~a found instead" token)
	      *parsing-errors*)))))

(defun parse-extract-parsed-file (&optional (accum '()))
  (cl-i18n:with-no-errors
    (let ((token (just-peek-token))
	  (translatable '()))
      (if (not token)
	  accum
	  (progn
	    (if (is-extr-function-re-p token)
		(let ((possible-string (parse-function)))
		  (when possible-string
		    (setf translatable (subseq possible-string 0 (1- (length possible-string))))))
		(parse-delimited-string))
	    (parse-extract-parsed-file (remove-if #'(lambda (a) (null a))
						  (push translatable accum))))))))
