;; Copyright (c) 2004 Sean Ross
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. The names of the authors and contributors may not be used to endorse 
;;    or promote products derived from this software without specific prior 
;;    written permission.

;; THIS SOFTWARE IS PROVIDED BY THE AUTHORS AND CONTRIBUTORS ``AS IS'' AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
;; OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;; SUCH DAMAGE.



(in-package :cl-i18n)


;; Function storing hack.
;; This just stores the function name if we can find it
;; or signal a store-error.
(defun parse-name (name)
  (let ((name (subseq name 21)))
    (declare (type simple-string name))
    (if (search name "SB!" :end1 3)
        (replace name "SB-" :end1 3)
        name)))

#+sbcl
(defvar *sbcl-readtable* (copy-readtable nil))
#+sbcl
(set-macro-character #\# #'(lambda (c s) 
                             (declare (ignore c s))
                              "Invalid character in function name.")
                     nil
                     *sbcl-readtable*)

(defun get-function-name (obj)
  (multiple-value-bind (l cp name) (function-lambda-expression obj)
    (declare (ignore l cp))
    (cond #+sbcl
          ;; handle (SB-C::&OPTIONAL-DISPATCH MAKE-FOO) names introduced around 1.0.15
          ((and name (consp name) (not (cddr name))  (eql (first name) (find-symbol "&OPTIONAL-DISPATCH" :sb-c)))
           (second name))
          ;; normal names and (setf name)
          ((and name (or (symbolp name) (consp name))) name)
          ;;  Try to deal with sbcl's naming convention
          ;; of built in functions (pre 0.9)
          #+sbcl
          ((and name (stringp name)
                (search "top level local call " (the simple-string name)))
           (let ((new-name (parse-name name))
                 (*readtable* *sbcl-readtable*))
             (unless (string= new-name "")
               (handler-case (read-from-string new-name)
                 (error (c)
                   (declare (ignore c))
                   "Unable to determine function name for ~A.")))))
          (t "Unable to determine function name for ~A."))))


