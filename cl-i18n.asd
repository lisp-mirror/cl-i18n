;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; vim:set ft=lisp:

(defpackage #:cl-i18n-asd
  (:use :cl :asdf))

(in-package :cl-i18n-asd)

(defsystem cl-i18n
  :name "cl-i18n"
  :version "0.3"
  :maintainer "Leslie P. Polzer <polzer@stardawn.org>"
  :author "Leslie P. Polzer <polzer@stardawn.org>"
  :licence "LLGPL"
  :description "A gettext-style internationalisation framework for Common Lisp."
  :depends-on (:cl-ppcre)
  :components ((:file "cl-i18n")
               (:file "i18n-util")))

