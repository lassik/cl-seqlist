(defpackage #:seqlist
  (:use #:cl)
  (:shadow #:first
           #:second
           #:third
           #:fourth
           #:fifth
           #:sixth
           #:seventh
           #:eighth
           #:ninth
           #:tenth

           #:append
           #:push
           #:pushnew
           #:pop)
  (:export #:first
           #:second
           #:third
           #:fourth
           #:fifth
           #:sixth
           #:seventh
           #:eighth
           #:ninth
           #:tenth

           #:append
           #:push
           #:pushnew
           #:pop))

(in-package #:seqlist)

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
    (unless (= 1 (cl:length place-vars))
      (error "Cannot expand this."))
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
    (unless (= 1 (cl:length place-vars))
      (error "Cannot expand this."))
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
    (unless (= 1 (cl:length place-vars))
      (error "Cannot expand this."))
    (let ((place-var (cl:first place-vars)))
      `(let* (,@(cl:mapcar #'list temp-vars temps)
              (,place-var ,get-place))
         (prog1 (if (vectorp ,place-var)
                    (vector-pop ,place-var)
                    (prog1 (car ,place-var)
                      (setf ,place-var (cdr ,place-var))))
                ,set-place)))))
