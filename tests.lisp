(defpackage #:seqlist-tests
  (:use #:cl #:seqlist #:1am)
  (:shadowing-import-from
   #:seqlist

   #:null
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
   #:rest
   #:append
   #:push
   #:pushnew
   #:pop))

(in-package #:seqlist-tests)

(test null-test
  (is (eql t (null '())))
  (is (eql t (null #())))
  (is (eql t (null #*)))
  (is (eql t (null "")))
  (is (eql t (null (make-array 0))))
  ;;
  (is (eql nil (null (make-array '()))))
  (is (eql nil (null (make-array '(0 0)))))
  (is (eql nil (null '(1))))
  (is (eql nil (null '(1 . 2))))
  (is (eql nil (null #(0))))
  (is (eql nil (null " ")))
  (is (eql nil (null #*0))))

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

(test rest-test
  (let ((tail '(2 3 . 4)))
    (is (eq tail (rest (cons 1 tail))))
    (let ((seq (list 1 2 3 4)))
      (setf (rest seq) tail)
      (is (eq tail (rest seq)))))
  (let ((seq (vector 1 2 3)))
    (is (equalp #(4 5) (setf (rest seq) #(4 5))))
    (is (equalp #(4 5) (rest seq)))
    (is (equalp #(1 4 5) seq)))
  (let ((seq (make-array 3 :initial-contents '(1 2 3) :adjustable t)))
    (is (equalp #(2 3 4 5) (setf (rest seq) #(2 3 4 5))))
    (is (equalp #(2 3 4 5) (rest seq)))
    (is (equalp #(1 2 3 4 5) seq)))
  (let ((seq "wait"))
    (is (equal "ait" (rest seq)))
    (is (equal "hat" (setf (rest seq) "hat")))
    (is (equal "what" seq))))

(test append-test
  (is (equal '() (append)))
  (is (equal '(1 2 3 4 5) (append '(1 2 3) '(4 5))))
  (is (equal '(1 2 3 4 5) (append '() '(1 2 3) '() '(4 5) '())))
  (is (equal '(1 2 3 4 5) (append '() '(1 2 3) #() #(4 5) "")))
  (is (equalp #(1 2 3 4 5) (append #() '(1 2 3) #() #(4 5) "")))
  (is (equal "abcdef" (append "abc" "def")))
  (signals type-error (append "123" '(4 5)))
  (is (equal '(1 2 3 1 2 3 #\1 #\2 #\3) (append '(1 2 3) #(1 2 3) "123")))
  (is (equal "aabbccdd" (append "aa" "bb" #(#\c #\c) '(#\d #\d)))))

(test append-dotted-list-test
  (let ((tail '(3 4 . 5)))
    (is (eq tail (nthcdr 2 (append '(1 2) tail)))))
  (let ((tail 'what))
    (is (eq 'what (nthcdr 2 (append '(1 2) tail)))))
  (is (equal '(1 2 3 4 . 5) (append '(1 2) '(3 4) 5)))
  (is (equal '(1 2 3 4 . 5) (append '(1 2) '(3 4 . 5))))
  (signals type-error (append '(1 2) '(3 4 . 5) '()))
  (signals type-error (append #(1 2) '(3 4 . 5))))

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
    (if (adjustable-array-p seq)
        (is (equalp #(1 2) (push 2 seq)))
        (signals error (push 2 seq))))
  (let ((seq (make-array 1 :fill-pointer 0 :adjustable t)))
    (is (equalp #(1) (push 1 seq)))
    (is (equalp #(1 2) (push 2 seq)))
    (is (equal 2 (pop seq))))
  (let ((seq (make-array 1)))
    (signals error (push 1 seq)))
  (let ((seq #()))
    (signals error (push 1 seq))))
