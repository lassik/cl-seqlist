(defpackage #:seqlist
  (:use #:cl)
  (:shadow #:null
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
           #:butlast
           #:append
           #:push
           #:pushnew
           #:pop)
  (:export #:null
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
           #:butlast
           #:append
           #:push
           #:pushnew
           #:pop))

(in-package #:seqlist)

(defun null (seq)
  (or (cl:null seq)
      (and (vectorp seq)
           (zerop (length seq)))))

(defmacro define-nth-accessors (&rest names)
  (let ((seq (gensym "SEQ-"))
        (elt-var (gensym "ELT-"))
        (getters '())
        (setters '()))
    (do ((names names (cl:rest names))
         (n 0 (1+ n)))
        ((cl:null names)
         `(progn ,@(reverse getters)
                 ,@(reverse setters)))
      (let* ((name (cl:first names))
             (getter `(defun ,name (,seq)
                        (elt ,seq ,n)))
             (setter `(defsetf ,name (,seq) (,elt-var)
                        `(setf (elt ,,seq ,,n)
                               ,,elt-var))))
        (cl:push getter getters)
        (cl:push setter setters)))))

(define-nth-accessors
  first
  second
  third
  fourth
  fifth
  sixth
  seventh
  eighth
  ninth
  tenth)

(defun rest (seq)
  (etypecase seq
    (vector
     (subseq seq 1))
    (list
     (cdr seq))))

(defun set-rest (seq new-rest)
  (etypecase seq
    (vector
     (unless (= (length seq) (1+ (length new-rest)))
       (if (adjustable-array-p seq)
           (adjust-array seq (1+ (length new-rest)))
           (error "Not adjustable seq")))
     (setf (subseq seq 1) new-rest)
     new-rest)
    (list
     (setf (cl:rest seq) new-rest))))

(defsetf rest set-rest)

(defun butlast (seq &optional (n 1))
  (etypecase seq
    (vector
     (check-type n (integer 0))
     (subseq seq 0 (max 0 (- (length seq) n))))
    (list
     (cl:butlast seq n))))

(defun append-as-list (seqs)
  (let* ((head (list nil))
         (tail head))
    (do ((seqs seqs (cdr seqs)))
        ((endp seqs)
         (cdr head))
      (let ((seq (car seqs)))
        (cond ((vectorp seq)
               (dotimes (i (length seq))
                 (setf tail (setf (cdr tail) (list (elt seq i))))))
              ((endp (cdr seqs))
               ;; This case is needed to retain identical behavior
               ;; with CL append, which doesn't copy the last list.
               (setf (cdr tail) seq))
              (t
               (dolist (elt seq)
                 (setf tail (setf (cdr tail) (list elt))))))))))

(defun append (&rest seqs)
  (cond ((cl:null seqs)
         '())
        ((listp (cl:first seqs))
         (append-as-list seqs))
        (t
         (apply #'concatenate
                `(vector ,(array-element-type (cl:first seqs)))
                seqs))))

(defmacro push (elem place &environment env)
  (multiple-value-bind (temp-vars temps place-vars set-place get-place)
      (get-setf-expansion place env)
    (assert (= 1 (cl:length place-vars)))
    (let ((place-var (cl:first place-vars))
          (elem-var (gensym "ELEM-")))
      `(let* ((,elem-var ,elem)
              ,@(cl:mapcar #'list temp-vars temps)
              (,place-var ,get-place))
         (progn (if (vectorp ,place-var)
                    (vector-push-extend ,elem-var ,place-var)
                    (setf ,place-var (cons ,elem-var ,place-var)))
                ,set-place)))))

(defmacro pushnew (elem place
                   &rest keys
                   &key key test test-not
                   &environment env)
  (declare (ignore key test test-not))
  (multiple-value-bind (temp-vars temps place-vars set-place get-place)
      (get-setf-expansion place env)
    (assert (= 1 (cl:length place-vars)))
    (let ((place-var (cl:first place-vars))
          (elem-var (gensym "ELEM-")))
      `(let* ((,elem-var ,elem)
              ,@(cl:mapcar #'list temp-vars temps)
              (,place-var ,get-place))
         (progn (unless (position ,elem-var ,place-var ,@keys)
                  (if (vectorp ,place-var)
                      (vector-push-extend ,elem-var ,place-var)
                      (setf ,place-var (cons ,elem-var ,place-var))))
                ,set-place)))))

(defmacro pop (place &environment env)
  (multiple-value-bind (temp-vars temps place-vars set-place get-place)
      (get-setf-expansion place env)
    (assert (= 1 (cl:length place-vars)))
    (let ((place-var (cl:first place-vars)))
      `(let* (,@(cl:mapcar #'list temp-vars temps)
              (,place-var ,get-place))
         (prog1 (if (vectorp ,place-var)
                    (vector-pop ,place-var)
                    (prog1 (car ,place-var)
                           (setf ,place-var (cdr ,place-var))))
                ,set-place)))))
