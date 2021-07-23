;; This software is Copyright (c) cage 2021
;; cage grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL

(in-package :cl-i18n-utils)

(defun fuzzy-match (template sequence
                    &key
                      (similarity-match     5)
                      (similarity-mismatch -5)
                      (penalty-weight       1))
  "Performs a Smith-Waterman affinity search.
   See: https://en.wikipedia.org/wiki/Smith%E2%80%93Waterman_algorithm

   Returns multiple values,  first a list of index  in `sequence' that
   matches `template' (even if some  deletion must be performed to get
   a perfect match.

   The other three return values  are matrix cost and traceback costs,
   useful to perform additional searches.

   The list is sorted in ascending order and null values means a gap is
   present to match `template'

   Example:

   (fuzzy-match \"TAB\" \"TUUUAB\") -> (0 NIL NIL NIL 4 5)

   T   AB
   |   ||
   T---AB


   Note: length of `template' must be shorter or equal to `sequence'.
"
  (declare (simple-string sequence template))
  (labels ((initialize-cost-matrix ()
             (make-array (list (1+ (length sequence))
                               (1+ (length template)))
                         :element-type     'fixnum
                         :initial-element  0
                         :adjustable       nil))
           (initialize-trace-matrix ()
             (initialize-cost-matrix))
           (similarity-value (i j)
             (if (char= (elt sequence (1- i))
                        (elt template (1- j)))
                 similarity-match
                 similarity-mismatch))
           (find-max (matrix)
             (let ((max-i -1)
                   (max-j -1)
                   (max    0))
               (loop for i from 0 below (array-dimension matrix 0) do
                 (loop for j from 0 below (array-dimension matrix 1) do
                   (when (> (aref matrix i j)
                            max)
                     (setf max (aref matrix i j))
                     (setf max-i i)
                     (setf max-j j))))
               (values max-i max-j max)))
           (traceback (costs trace-col trace-row i j &optional (res '()))
             (let ((new-i    (aref trace-col i j))
                   (new-j    (aref trace-row i j))
                   (new-cost (aref costs     i j)))
               (cond
                 ((= new-cost 0)
                  (mapcar (lambda (a)
                            (if (third a)
                                nil
                                (first a)))
                          res))
                 ((= j new-j)
                  (traceback costs
                             trace-col trace-row
                             new-i new-j
                             (push (list new-i new-j t) res)))
                 (t
                  (traceback costs
                             trace-col trace-row
                             new-i new-j
                             (push (list new-i new-j) res)))))))
    (let ((costs      (initialize-cost-matrix))
          (trace-col  (initialize-trace-matrix))
          (trace-row  (initialize-trace-matrix)))
      (loop for ct-sequence from 0 below (length sequence) do
        (loop for ct-template from 0 below (length template) do
          (let* ((i                 (1+ ct-sequence)) ; y
                 (j                 (1+ ct-template)) ; x
                 (cost-similarity   (+ (aref costs (1- i) (1- j))
                                       (similarity-value i j)))
                 (cost-deletion-col (- (aref costs (1- i) j)
                                       penalty-weight))
                 (cost-deletion-row (- (aref costs i (1- j))
                                       penalty-weight))
                 (all-costs         (list cost-similarity
                                          cost-deletion-row
                                          cost-deletion-col))
                 (max               -1e10)
                 (max-pos           -1))
            (loop for ct from 0 below 3 do
              (when (> (elt all-costs ct)
                       max)
                (setf max (elt all-costs ct))
                (incf max-pos)))
            (setf max (max 0 max))
            (setf (aref costs i j) max)
              (cond
                ((= max-pos 0)
                 (setf (aref trace-col i j) (1- i))
                 (setf (aref trace-row i j) (1- j)))
                ((= max-pos 1)
                 (setf (aref trace-col i j) i)
                 (setf (aref trace-row i j) (1- j)))
                ((= max-pos 2)
                 (setf (aref trace-col i j) (1- i))
                 (setf (aref trace-row i j) j))))))
      (multiple-value-bind (start-i start-j)
          (find-max costs)
        (when (and (> start-i 0)
                   (> start-j 0))
          (let ((trace (traceback costs trace-col trace-row start-i start-j)))
            (values trace
                    costs trace-col trace-row)))))))
