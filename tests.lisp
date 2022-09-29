(defpackage #:seqlist-tests
  (:use #:cl #:seqlist #:1am)
  (:shadowing-import-from
   #:seqlist

   #:first
   #:second
   #:third
   #:fourth
   #:fifth
   #:sixth
   #:seventh
   #:eighth
   #:ninth
   #:tenth

   #:push
   #:pushnew
   #:pop))

(in-package #:seqlist-tests)

(test nth-accessors
  (let* ((seq (copy-seq "abc"))
         (box (list seq)))
    (is (equal #\a (first seq)))
    (is (equal #\A (setf (first seq) #\A)))
    (is (equal "Abc" seq))
    (is (equal #\A (first seq)))
    (is (eq seq (first box)))
    (is (equal #\b (second (first box))))
    (is (equal #\B (setf (second (first box)) #\B)))
    (is (equal "ABc" seq))
    (is (equal #\B (second (first box)))))
  (signals type-error
    (first (make-array '(10 10)))))

(test push-pushnew-pop
  (let ((seq '()))
    (is (equal '(1) (push 1 seq)))
    (is (equal '(2 1) (push 2 seq)))
    (is (equal '(2 1) (pushnew 2 seq)))
    (is (equal '(3 2 1) (pushnew 3 seq)))
    (is (equal 3 (pop seq))))
  (let ((seq 'what))
    (is (equal '(1 . what) (push 1 seq))))
  (let ((seq 'what))
    (signals error (pushnew 1 seq)))
  (let ((seq (make-array 1 :fill-pointer 0)))
    (is (equalp #(1) (push 1 seq)))
    (is (equal 1 (fill-pointer seq)))
    (signals error (push 2 seq)))
  (let ((seq (make-array 1 :fill-pointer 0 :adjustable t)))
    (is (equalp #(1) (push 1 seq)))
    (is (equalp #(1 2) (push 2 seq)))
    (is (equal 2 (pop seq))))
  (let ((seq (make-array 1)))
    (signals error (push 1 seq)))
  (let ((seq #()))
    (signals error (push 1 seq))))
