;; This software is Copyright (c) Leslie P. Polzer, 2011.
;; Leslie P. Polzer grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL

(in-package :cl-i18n)

(defmacro define-plural-form ((lang) &body body)
  `(defun
       ,(alexandria:format-symbol t "~a-PLURAL-FORM" lang)
       (n) ;anaphora
     ,@body))


(define-plural-form (n/=1)
  (if (/= n 1)
      1
      0))
    

(define-plural-form (english)
  (if (/= n 1)
      1
      0))


(define-plural-form (italian)
  (if (/= n 1)
      1
      0))

(define-plural-form (spanish)
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
