;; This software is Copyright (c) cage, 2012.
;; cage grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL

(in-package :cl-i18n)

(alexandria:define-constant +po-comment-line+ "#\\n|^#[^,].*\\n|\\n" :test 'string=)

(defclass po-parsed-file (parsed-file) ())


(defmethod initialize-instance :after ((object po-parsed-file) &key &allow-other-keys)
  (with-slots (comment-line) object
    (setf comment-line +po-comment-line+)))


(define-parser-skeleton po po-parsed-file)

(alexandria:define-constant +number+ "0|[1-9][0-9]+|[1-9]" :test 'string=)

(alexandria:define-constant +and-op+ "&&" :test 'string=)

(alexandria:define-constant +or-op+ "||" :test 'string=)

(alexandria:define-constant +or-op-regex+ "\\|\\|" :test 'string=)

(alexandria:define-constant +<+  "<" :test 'string=)

(alexandria:define-constant +>+  ">" :test 'string=)

(alexandria:define-constant +<=+  "<=" :test 'string=)

(alexandria:define-constant +>=+  ">=" :test 'string=)

(alexandria:define-constant +!=+  "!=" :test 'string=)

(alexandria:define-constant +==+  "==" :test 'string=)

(alexandria:define-constant +%+  "%" :test 'string=)

(alexandria:define-constant +?+  "?" :test 'string=)

(alexandria:define-constant +?-regex+  "\\?" :test 'string=)

(alexandria:define-constant +colon+  ":" :test 'string=)

(alexandria:define-constant +open-paren+  "(" :test 'string=)

(alexandria:define-constant +close-paren+  ")" :test 'string=)

(alexandria:define-constant +end-expression+  ";" :test 'string=)

(alexandria:define-constant +open-paren-regex+  "\\(" :test 'string=)

(alexandria:define-constant +close-paren-regex+  "\\)" :test 'string=)

(alexandria:define-constant +var+  "n" :test 'string=)

(alexandria:define-constant +escape-newline+ "\\" :test 'equalp)

(alexandria:define-constant +escaped-string-delim+ "\"" :test 'string=)

(alexandria:define-constant +escape-string-escape-char+ "\\" :test 'equalp)

(alexandria:define-constant +escape-string-escaped-newline+ "n" :test 'equalp)

(alexandria:define-constant +newline+ #\NewLine :test 'equalp)

(alexandria:define-constant +agnostic-comment+ "#" :test 'equalp)

(alexandria:define-constant +flag-line+ "#,[ ]*" :test 'equalp)

(alexandria:define-constant +flag-fuzzy+ "fuzzy" :test 'equalp)

(alexandria:define-constant +msgid+ "msgid" :test 'equalp)

(alexandria:define-constant +msgstr+ "msgstr" :test 'equalp)

(alexandria:define-constant +msgid-regexp+ "msgid[ ]+" :test 'equalp)

(alexandria:define-constant +msgstr-regexp+ "msgstr[ ]+" :test 'equalp)

(alexandria:define-constant +msgstr[]+ "msgstr\\[[0-9]\\]" :test 'equalp)

(alexandria:define-constant +msgstr[]-regexp+ "msgstr\\[[0-9]\\]" :test 'equalp)

(alexandria:define-constant +msgid-plural+ "msgid_plural" :test 'equalp)

(alexandria:define-constant +plural-form-label+ "Plural-Forms:" :test 'equalp)

(alexandria:define-constant +nplurals-label+ "nplurals=" :test 'equalp)

(alexandria:define-constant +plural-expression-label+ "plural=" :test 'equalp)

(defparameter *boolean-op* (list +and-op+ +or-op+))

(defparameter *compar-op* (list +>+ +<+ +>=+ +<=+ +==+ +!=+))

(defparameter *aritm-op* (list +%+))

(defparameter *n* 0)

(defun == (a b)
  (if (= a b)
      1
      0))

(defun != (a b)
  (if (/= a b)
      1
      0))

(defun % (a b)
  (mod a b))

(defun && (a b)
  (if (and (> a 0)
	   (> b 0))
      1
      0))

(defun bool-or (a b)
  (if (or (> a 0)
	  (> b 0))
      1
      0))

(defun >* (a b)
  (if (> a b)
      1
      0))

(defun >=* (a b)
  (if (>= a b)
      1
      0))

(defun <* (a b)
  (if (< a b)
      1
      0))

(defun <=* (a b)
  (if (<= a b)
      1
      0))

(defun stack-if (&rest args)
  (let ((if-term (first args))
	(then (second args))
	(else (third args)))
    (cond
      ((numberp if-term)
       (if (/= 0 if-term)
	   then
	   else))
      (t
       (if if-term then else)))))

(defun string->function (fun)
  (cond
    ((string= fun "||")
     #'bool-or)
    ((string= fun "?")
     #'stack-if)
    ((string= fun ">")
     #'>*)
    ((string= fun ">=")
     #'>=*)
    ((string= fun "<")
     #'<*)
    ((string= fun "<=")
     #'<=*)
    (t
     (symbol-function (intern fun :cl-i18n)))))

(defun unescaped-char (char)
  (cond
    ((string= char +escape-string-escaped-newline+)
     (format nil "~%"))
    ((string= char #\newline)
     "")
    ((string= char #\")
     "\"")))

(define-tokenizer (po-parsed-file +po-comment-line+ +open-paren-regex+ +close-paren-regex+ +number+ +and-op+ +or-op-regex+ +<+ +>+ +<=+ +>=+ +!=+ +==+ +%+ +?-regex+ +colon+ +var+ +end-expression+ +plural-expression-label+ +msgid-regexp+ +msgstr-regexp+ +flag-line+ +flag-fuzzy+ +msgstr[]-regexp+ +msgid-plural+)
    ((string= (char@) +escape-newline+)
     (multiple-increment 2)
     (next-token *file*))
  ((member (char@) *blank-space* :test #'string=)
   (increment-pointer *file*)
   (next-token *file*)))

(define-is-stuff-p string= +and-op+ +or-op+ +<+ +>+  +<=+  +>=+  +!=+  +==+  +%+  +?+  +colon+  +open-paren+  +close-paren+ +var+ +end-expression+ +fuzzy-flag+)

(define-is-stuff-p cl-ppcre:scan +msgid-regexp+ +msgstr-regexp+ +flag-line+ +msgstr[]-regexp+ +msgid-plural+)

(defun is-number-p (str)
  (cl-ppcre:scan +number+ str))

(defmacro parse-token ((var predicate msg &rest predicate-arg) &body body)
  `(let-noerr ((,var (next-token *file*)))
       (with-error (,predicate ,msg ,@predicate-arg) ,@body)))

(defun parse-any-operator (oper-list &key (test #'string=))
  (parse-token (operator (lambda (v) (member v
					     oper-list
					     :test test))
			 (format nil "Error: expected operator like ~{~a ~} got ~s instead." oper-list operator)
			 operator)
    operator))

(defmacro define-parse-operators (names test &rest operators-list)
  `(progn
     ,@(mapcar #'(lambda (name operator)
		   `(defun ,(alexandria:format-symbol t "~:@(~a~)" name) ()
			(parse-any-operator ,operator :test ,test)))
	       names operators-list)))

(define-parse-operators (parse-comparision-operator parse-arithmetic-operator
						    parse-boolean-operator)
    #'string=
    *compar-op* *aritm-op* *boolean-op*)

(define-parse-operators (parse-open-parent parse-close-parent
					   parse-if-symbol parse-then-symbol)
    #'string=
    (list +open-paren+) (list +close-paren+) (list +?+) (list +colon+))

(define-parse-operators (parse-msgid parse-msgstr
				     parse-msgid-plural
				     parse-msgstr-plural)
    #'(lambda (a b) (cl-ppcre:scan b a))
  (list +msgid-regexp+) (list +msgstr-regexp+) (list +msgid-plural+)
  (list +msgstr[]-regexp+))

(defun is-bool-op-p (str)
  (or (is-and-op-p str)
      (is-or-op-p str)))

(defun is-binary-operator (op)
  (or
   (eq #'bool-or op)
   (eq #'&& op)
   (eq #'== op)
   (eq #'!= op)
   (eq #'% op)
   (eq #'>* op)
   (eq #'<* op)
   (eq #'>=* op)
   (eq #'<=* op)))

(defun is-ternary-operator (op)
  (eq #'stack-if op))

(defnocfun parse-msgid-group ()
  (let-noerr ((msgid (parse-msgid))
	      (string (parse-escaped-string)))
    (values string msgid)))

(defnocfun parse-msgid-plural-group ()
  (let-noerr ((msgid (parse-msgid-plural))
	      (string (parse-escaped-string)))
    (values string msgid)))


(defmacro with-line-mode (&body body)
  `(progn
     (setf (line-mode *file*) t)
     ,@body))

(defnocfun parse-po-file ()
  (with-line-mode
      (let-noerr ((plural-function (parse-header))
		  (entries (parse-entries)))
	(values entries plural-function *has-errors* *parsing-errors*))))

(defnocfun parse-entries (&optional (res (make-hash-table :test 'equal)))
  (with-no-errors
    (if (peek-valid-stream)
	(let-noerr ((peek (peek-token *file*))
		    (flag :untranslated))
	  (when (is-flag-line-p peek)
	    (next-token *file*)
	    (setf flag (alexandria:make-keyword (format nil "~:@(~a~)" (next-token *file*)))))
	  (when-debug
	    (format t "flag ~s~%errors ~s~%" flag *parsing-errors*))

	  (let-noerr ((msgid (parse-msgid-group)))
	    (when-debug
	      (format t "msgid ~s errors ~s~%" msgid *parsing-errors*))
	    (multiple-value-bind (first-translation plural-forms)
		(parse-msgstr-group)
	      (when-debug
		(format t "msgsrt ~s ~s~%errors ~s~%" first-translation plural-forms *parsing-errors*))
	      (let ((translation (make-translation
				  (if (not (null plural-forms))
				      (first plural-forms)
				      first-translation)
				  flag
				  first-translation
				  (if (not (null plural-forms))
				      (rest plural-forms)
				      '()))))
		(setf (gethash msgid res) translation)
		(parse-entries res)))))
	res)))

(defun parse-msgstr-group ()
  (let-noerr ((peek (peek-token *file*)))
    (with-no-errors
      (cond
	((is-msgid-plural-p peek)
	 (let-noerr ((plural (parse-msgid-plural-group))
		     (plural-forms (parse-msgstr-plural-group)))
	   (values plural plural-forms)))
	((is-msgstr-regexp-p peek)
	 (with-no-errors
	   (parse-msgstr)
	   (let-noerr ((string (parse-escaped-string)))
	     (values string nil))))
	(t
	 (setf *has-errors* t)
	 (push "Junk found while parsing-for entries" *parsing-errors*))))))

(defun parse-msgstr-plural-group (&optional (res '()))
  (with-no-errors
    (parse-msgstr-plural)
    (let-noerr ((string (parse-escaped-string)))
      (if (and (peek-valid-stream)
	       (is-msgstr[]-regexp-p (peek-token *file*)))
	  (progn
	    (parse-msgstr-plural-group (push string res)))
	  (reverse (push string res))))))

(defnocfun parse-header ()
  (parse-msgid-group)
  (parse-msgstr)
  (let-noerr ((header (parse-escaped-string)))
    (when-debug
      (format t "header~%~s~%" header))
    (extract-plural-function header)))

(defun extract-plural-function (header)
  (when-debug
    (format t "header~%~s~%" header))
  (with-po-file (:buffer (cl-ppcre:regex-replace-all "(?m)\\n" header " "))
    (with-no-errors
      (next-token *file* :hook-to-stringpos nil);; the plural expression starts here
      (when-debug
	(format t "plural-expr: ****~a***~%" (peek-token *file*)))
      (multiple-value-bind (fun stack)
	  (parse-plural-expression)
	(when-debug
	  (format t "stack (~%~{~s~%~})~% fun ~a 1 -> ~a~%" stack fun (funcall fun 1)))
	fun))))

(defun parse-escaped-string (&optional (res "") (delimc nil))
  (if (peek-valid-stream)
      (handler-bind ((i18n-conditions:out-of-bounds
		      #'(lambda(e)
			  (declare (ignore e))
			  (invoke-restart 'use-value ""))))
	(let-noerr ((char (char@1+)))
	  (cond
	    ((string= char +escape-string-escape-char+)
	     (let-noerr ((char-esc (char@1+)))
	       (parse-escaped-string
		(concatenate 'string res (unescaped-char char-esc)) delimc)))
	    ((string= char +escaped-string-delim+)
	     (parse-escaped-string res (not delimc)))
	    ((string= char " ")
	     (if delimc
		 (parse-escaped-string (concatenate 'string res char) delimc)
		 (parse-escaped-string res delimc)))
	    ((string= char +newline+)
	     (cond
	       ((or
		 (string= (char@) " ")
		 (string= (char@) +escaped-string-delim+)
		 (string= (char@) +newline+))
		(parse-escaped-string res delimc))
	       (t
		res)))
	    (t
	     (parse-escaped-string
	      (concatenate 'string res char) delimc)))))
      res))

(defun parse-plural-expression ()
  (let-noerr* ((peek (peek-token *file*))
	       (stack (if (is-number-p peek)
			  (list (parse-integer (next-token *file*)))
			  (parse-ternary-expression))))

    (values #'(lambda (n) (let ((*n* n))
			    (execute-expression stack)))
	    stack)))

(defun parse-ternary-expression ()
  (let-noerr ((first-term (parse-boolean-expression)))
    (if (not (is-end-expression-p (peek-token *file*)))
	(let-noerr ((if-symbol (parse-if-symbol))
		    (then-term (parse-expression)))
	  (with-no-errors
	    (parse-then-symbol)
	    (let-noerr ((else-term (parse-expression)))
	      (list (string->function if-symbol)
		    first-term
		    then-term
		    else-term))))
	(list first-term))))

(defun parse-expression ()
  (let-noerr ((peek (peek-token *file*)))
    (cond
      ((is-number-p peek)
       (parse-integer (next-token *file*)))
      (t
       (parse-ternary-expression)))))

(defun parse-boolean-expression (&optional (stack '()))
  (if (peek-valid-stream)
      (with-no-errors
	(let ((peek (peek-token *file*)))
	  (cond
	    ((is-close-paren-p peek)
	     (parse-close-parent)
	     stack)
	    ((is-open-paren-p peek)
	     (parse-open-parent)
	     (setf stack (parse-boolean-expression stack)) ;; parse the subexpression
	     (setf stack (parse-boolean-expression stack)) ;; parse the close parent
	     (setf stack (parse-boolean-expression stack))) ;; parse the rest of the expression
	    ((is-var-p peek)
	     (setf stack (parse-arithmetic-expression stack))
	     (setf stack (parse-boolean-expression stack)))
	    ((is-bool-op-p (peek-token *file*))
	     (let-noerr ((boolean-op (parse-boolean-operator)))
	       (setf stack (parse-boolean-expression stack))
	       (push (string->function boolean-op) stack)))
	    ((is-end-expression-p peek)
	     stack)
	    ((is-?-p peek)
	     stack)
	    ((is-colon-p peek)
	     stack)
	    (t
	     (push (format nil "Error: ~s, ~s or ~s expected, got ~s instead." +var+ +open-paren+ +close-paren+ peek)
		   *parsing-errors*)
	     (setf *has-errors* t)))))
      stack))

(defun parse-arithmetic-expression (&optional stack)
  (let ((local-stack '()))
    (parse-token (var (lambda (v) (string= v +var+))
		      (format nil "Error: expected ~s got ~s instead." +var+ var)
		      var)
      (let-noerr ((operator (parse-any-operator (concatenate 'list *aritm-op* *compar-op*))))
	(cond
	  ((member operator *compar-op* :test #'string=)
	   (let-noerr ((number (parse-number)))
	     (push (parse-integer number) local-stack)
	     (push (quote *n*) local-stack)
	     (push (string->function operator) local-stack)))
	  ((member operator *aritm-op* :test #'string=) ; %
	   (let-noerr ((number1 (parse-number)) ; n%100==1 -> (== % n 100 1) -> (1 100 n % ==)
		       (compare-op (parse-comparision-operator))
		       (number2 (parse-number)))
	     (with-no-errors
	       (push (parse-integer number2) local-stack)
	       (push (parse-integer number1) local-stack)
	       (push (quote *n*) local-stack)
	       (push (string->function operator) local-stack)
	       (push (string->function compare-op) local-stack))))))
      (push local-stack stack))))

(defun parse-number ()
  (parse-token (number (lambda (v) (cl-ppcre:scan +number+ v))
		       (format nil "Error: Number expected got ~s instead." number)
		       number)
    number))

(defmacro pop-apply-binary-operator (stack operator)
  (alexandria:with-gensyms (op1 op2)
    `(let ((,op1 (pop ,stack))
	   (,op2 (pop ,stack)))
       (push (funcall ,operator ,op1 ,op2) ,stack))))

(defmacro pop-apply-ternary-operator (stack operator)
  (alexandria:with-gensyms (if-term then else)
    `(let ((,if-term (pop ,stack))
	   (,then (pop ,stack))
	   (,else (pop ,stack)))
       (push (funcall ,operator ,if-term ,then ,else) ,stack))))

(defun execute-expression (stack)
  (let ((exec-stack '()))
    (macrolet ((pop-stack (s) (pop s)))
      (labels ((execute ()
		 (if (> (length stack) 0)
		     (progn
		       (let ((elem (pop stack)))
			 (cond
			   ((is-binary-operator elem)
			    (pop-apply-binary-operator exec-stack elem))
			   ((is-ternary-operator elem)
			    (pop-apply-ternary-operator exec-stack elem))
			   ((null elem)
			    (push elem exec-stack))
			   ((listp elem)
			    (push (execute-expression elem) stack))
			   ((symbolp elem)
			    (push (symbol-value elem) exec-stack))
			   (t
			    (push elem exec-stack))))
		       (execute))
		     (pop exec-stack))))
	(setf stack (reverse stack))
	(execute)))))
