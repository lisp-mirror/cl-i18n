;; This software is Copyright (c) Leslie P. Polzer, 2011.
;; Leslie P. Polzer grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL

(in-package :cl-i18n)

(defmacro define-plural-form ((lang) &body body)
  (if (and
       (listp lang)
       (not (null lang)))
      (progn
	`(progn
	   ,@(mapcar (lambda (name) `(define-plural-form (,name)  ,@body)) lang)))
      (progn
	`(defun
	     ,(alexandria:format-symbol t "~:@(~a~)-PLURAL-FORM" lang)
	     (n) ;anaphora
	   ,@body))))

(define-plural-form ((n/=1 english german dutch swedish danish norwegian faroese spanish italian bulgarian greek finnish estonian hebrew esperanto hungarian turkish))
  (if (/= n 1)
      1
      0))

(define-plural-form (polish)
  (cond
    ((or (= n 0)
	 (= n 1))
     0)
    ((and (>= (mod n 10) 2)
	  (<= (mod n 10) 4)
	  (or (< (mod n 100) 10)
	      (>= (mod n 100) 20)))
     1)
    (t 2)))

(define-plural-form (slovenian)
    (cond
      ((or (= n 0)
	   (= (mod n 100) 1))
       0)
      ((= (mod n 100) 2)
       1)
      ((or (= (mod n 100) 3)
	   (= (mod n 100) 4))
       2)
      (t 3)))

(define-plural-form ((czech slovak))
  (cond
    ((or (= n 0)
	 (= n 1))
     0)
    ((and (>= n 2)
	  (<= n 4))
     1)
    (t 2)))

(define-plural-form ((russian ukrainian serbian croatian))
  (cond
    ((or
      (= n 0)
      (and
       (= (mod n 10) 1)
       (/= (mod n 100) 11)))
     0)
    ((and
      (>= (mod n 10) 2)
      (<= (mod n 10) 4)
      (or (< (mod n 100) 10)
	  (>= (mod n 100) 20)))
     1)
     (t 2)))

(define-plural-form ((brazilian portuguese french))
  (cond
    ((or (= n 0)
	 (= n 1))
     0)
    (t 1)))

(define-plural-form (latvian)
  (cond
    ((and
      (= (mod n 10) 1)
      (/= (mod n 100) 11))
     0)
    ((/= n 0)
     1)
    (t 2)))

(define-plural-form ((gaeilge irish))
  (cond
    ((or (= n 0)
	 (= n 1))
     0)
    ((= n 2)
     1)
    (t 2)))

(define-plural-form (romanian)
  (cond
    ((= n 1)
     0)
    ((or
      (= n 0)
      (and
       (> (mod n 100) 0)
       (< (mod n 100) 20)))
     1)
    (t 2)))

(define-plural-form (lithuanian)
  (cond
    ((or (= n 0)
	 (= n 1)
	 (and (= (mod n 10) 1)
	      (/= (mod n 100) 11)))
     0)
    ((and
      (>= (mod n 10) 2)
      (or (< (mod n 100) 10)
	  (>= (mod n 100) 20)))
     1)
    (t 2)))
