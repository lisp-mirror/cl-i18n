(in-package #:cl-user)

(defpackage #:cl-i18n-tests
  (:use
   #:alexandria
   #:common-lisp
   #:cl-i18n
   #:clunit)
  (:export #:run))

(in-package #:cl-i18n-tests)

(defsuite cl-i18n-suite ())

(defun package-path ()
  (uiop:pathname-parent-directory-pathname
   (asdf:component-pathname
    (asdf:find-component (symbolicate (string-upcase "cl-i18n"))
                         nil))))

(defun file-in-package (name)
  (concatenate 'string (namestring (package-path)) name))

(setf *translation-file-root* (file-in-package "cl-i18n/examples/locale/"))

(defmacro with-italian-native-translation-table (&body body)
  ;; Native file format example ;;;;
  ;; change accordingly to the actual location of the directory cointaining
  ;; the translation files
  `(handler-bind ((i18n-conditions:no-translation-table-error ;; Or just (load-language "italian.lisp")
                    #'(lambda(e)
                        (declare (ignore e))
                        (invoke-restart 'load-language "italian.lisp" t))))
     ,@body))

(defmacro standard-singular-tests ()
  `(progn
     (assert-equality #'string= "mela"  (translate "apple"))
     (assert-equality #'string= "torta" (translate "pie"))))

(defmacro standard-plural-tests ()
  `(progn
     (assert-equality #'string=
         "mele"
         (ntranslate "apple" "apples" 4))
     (assert-equality #'string=
         "mela"
         (ntranslate "apple" "apples" 1))))

(deftest native-translation (cl-i18n-suite)
  (with-italian-native-translation-table
    (standard-singular-tests)))

(deftest native-translation-plural ((cl-i18n-suite) (native-translation))
  (standard-plural-tests))

(defmacro with-italian-po-translation-table (&body body)
  `(cl-i18n:with-translation ((cl-i18n:init-translation-table  "it.po"
                                                               :store-hashtable nil
                                                               :store-plural-function t
                                                               :update-translation-table nil)
                              cl-i18n:*plural-form-function*)
     ;; or pass a previously loaded hashtable of course
     ,@body))

(deftest po-translation (cl-i18n-suite)
  (with-italian-po-translation-table
    (standard-singular-tests)))

(deftest po-translation-plural (cl-i18n-suite)
  (with-italian-po-translation-table
    (standard-plural-tests)))

(defparameter *italian-from-lisp*
  (multiple-value-list (load-language "italian.lisp"
                                      :locale                   nil
                                      :store-hashtable          nil
                                      :store-plural-function    nil
                                      :update-translation-table nil)))

(defparameter *italian-from-po*
  (multiple-value-list (load-language "it.po"
                                      :locale                   nil
                                      :store-hashtable          nil
                                      :store-plural-function    nil
                                      :update-translation-table nil)))

(defparameter *it-from-utx*
  (multiple-value-list (load-language "it.utx"
                                      :locale                   nil
                                      :store-hashtable          nil
                                      :store-plural-function    nil
                                      :update-translation-table nil)))

(deftest runtime-dictionary-switching (cl-i18n-suite)
 (with-translation ((first *italian-from-lisp*)
                    (second *italian-from-lisp*))
   (standard-singular-tests)
   (standard-plural-tests)
   (with-translation ((first *italian-from-po*)
                      (second *italian-from-po*))
     (standard-singular-tests)
     (standard-plural-tests)
     (with-translation ((first *it-from-utx*)
                        (second *it-from-utx*))
       (standard-singular-tests)
       (standard-plural-tests)))))

(defun run-all-tests (&key (use-debugger nil))
  "Run all the tests."
  (clunit:run-suite 'cl-i18n-suite :use-debugger use-debugger))
