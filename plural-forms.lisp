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
