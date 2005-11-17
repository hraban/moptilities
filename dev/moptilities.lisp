;;;; -*- Mode: Common-Lisp; Package: METABANG.MOPTILITIES; Base: 10 -*-
;;;; -**- $Source: /Users/eksl/repository/moptilities/dev/moptilities.lisp,v $ -**-
;;;; -**- $Author: gwking $ -**-
;;;; -**- $Date: 2005/09/13 21:03:49 $ -**-
;;;; 

(in-package cl-user)

(defpackage "METABANG.MOPTILITIES"
  (:use "COMMON-LISP"
        #+ALLEGRO   "SYS"
        #+ALLEGRO   "CLOS"
        #+LISPWORKS "MP"
        #+LISPWORKS "CLOS"
        #+MCL       "CCL")
  (:nicknames "MOPU" "MOPTILITIES")
  (:export
   #:map-methods 
   #:remove-methods
   #:remove-methods-if
   #:display-methods
   #:direct-specializers-of
   #:all-specializers-of
   #:generic-function-list
   #:slot-names-of-class
   #:finalize-class-if-necessary
          
   #:mopu-find-method
   #:mopu-generic-function-methods
   #:mopu-specializer-direct-generic-functions
   #:mopu-method-qualifiers
   #:mopu-method-specializers
   #:mopu-generic-function-name
   #:mopu-reader-method-p 
   #:mopu-writer-method-p
   #:mopu-arglist
   #:mopu-method-name
   #:mopu-eql-specializer-p 
   #:mopu-find-slot-definition

   #:mopu-slot-definition-initform
   #:mopu-class-slot-names
   #:mopu-direct-slot-names
   #:mopu-class-slot-information
   
   #:mopu-class-direct-superclasses
   #:mopu-class-direct-subclasses
   #:mopu-class-precedence-list
   #:mopu-class-initargs
   
   #:subclassp
   #:subclasses
   #:subclasses*
   #:map-subclasses
          
   #:find-slot
   #:finalize-class-if-necessary
   #:leaf-class-p
   #:leaf-subclasses
   #:mopu-class-direct-subclasses
   
   #:class
   #:class-name-of))

(in-package "MOPTILITIES")

#|

Surely not cross platform yet.

However, there are three MOPs that matter:

  * MCL's ANSI+almost nothing 
  * Allegro's mostly AMOP
  * LispWork's mostly AMOP

to a (much) lesser extent there is 

  * PCL's mostly AMOP

For the moment I am going to just hack things.  There must be a better portability
model, though. (Like what exactly? GWK)


XXXX At the moment, Lispworks and Allegro are still not completely supported (i.e., 
XXXX there is code here that doesn't really do much useful on those platforms. Sigh.

   ---Louis

|#

(defmethod subclassp (child parent)
  "Returns t if child is a subclass of the parent."
  #+MCL
  (progn
    (not (null (find (find-class parent) (mopu-class-precedence-list child)))))
  #+(or Allegro LispWorks4)
  (progn
    (finalize-class-if-necessary child)
    (not (null (find (find-class parent) (mopu-class-precedence-list child)))))
  #-(or MCL ALLEGRO LISPWORKS4)
  (error "don't know how to subclassp"))

;;; ---------------------------------------------------------------------------

(defgeneric mopu-class-direct-superclasses (class)
  (:method ((class standard-class))
           (or #+MCL 
               (ccl:class-direct-superclasses class)
               #+ALLEGRO
               (clos:class-direct-superclasses class)
               #+LispWorks4
               (hcl:class-direct-superclasses class)
               (error "don't know how to mopu-class-direct-superclasses")))
  (:method ((class symbol))
           (mopu-class-direct-superclasses (find-class class)))
  (:method ((class standard-object))
           (mopu-class-direct-superclasses (class-of class))))

;;; ---------------------------------------------------------------------------

(defgeneric mopu-class-direct-subclasses (class)
  (:method ((class standard-class))
           (mopu-class-direct-subclasses-internal class))
  (:method ((class built-in-class))
           (mopu-class-direct-subclasses-internal class))
  (:method ((class symbol))
           (mopu-class-direct-subclasses (find-class class)))
  (:method ((class standard-object))
           (mopu-class-direct-subclasses (class-of class))))

;;; ---------------------------------------------------------------------------

(defun mopu-class-direct-subclasses-internal (class)
  (or #+MCL 
      (ccl:class-direct-subclasses class)
      #+ALLEGRO
      (clos:class-direct-subclasses class)
      #+LispWorks4
      (hcl::class-direct-subclasses class)
      #-(or MCL ALLEGRO LISPWORKS4)
      (error "don't know how to mopu-class-direct-superclasses")))

;;; ---------------------------------------------------------------------------

(defun finalize-class-if-necessary (class)
  "Finalizes the class \"class\" if necessary. Returns t."
  class                                 ; prevent warning
  #+(or Allegro LispWorks4)
  (unless (class-finalized-p class)
    (finalize-inheritance class))
  (values t))

;;; ---------------------------------------------------------------------------

(defgeneric mopu-class-precedence-list (class) 
  (:method ((class standard-class))
           #+MCL
           (ccl:class-precedence-list class)
           #+(or ALLEGRO LispWorks4)
           (progn
             (finalize-class-if-necessary class)
             (class-precedence-list class))
           #-(or MCL ALLEGRO LISPWORKS4)
           (error "don't know how to mopu-class-precedence-list"))
  (:method ((class symbol))
           (mopu-class-precedence-list (find-class class)))
  (:method ((class standard-object))
           (mopu-class-precedence-list (class-of class))))

;;; ---------------------------------------------------------------------------

(defgeneric mopu-method-name (method)
  (:method ((method standard-method))
           (mopu-generic-function-name (method-generic-function method))))

;;; ---------------------------------------------------------------------------

(defmethod mopu-method-specializers (m)
  #+MCL
  (ccl:method-specializers m)
  #+(OR ALLEGRO LISPWORKS4)
  (clos:method-specializers m))

;;; ---------------------------------------------------------------------------

(defmethod mopu-method-qualifiers (m)
  #+MCL
  (ccl::method-qualifiers m)
  #+(OR ALLEGRO LISPWORKS4)
  (clos:method-qualifiers m))

;;; ---------------------------------------------------------------------------

(defmethod mopu-generic-function-methods (gf)
  #+MCL
  (ccl:generic-function-methods gf)
  #+(or ALLEGRO LispWorks4)
  (clos:generic-function-methods gf))

;;; ---------------------------------------------------------------------------

(defmethod mopu-specializer-direct-generic-functions (class)
  #+MCL
  (ccl:specializer-direct-generic-functions class)
  #+ALLEGRO
  (clos:specializer-direct-generic-functions class)
  #+LispWorks4
  (clos::class-direct-generic-functions class))

;;; ---------------------------------------------------------------------------

(defmethod mopu-generic-function-name (gf)
  #+MCL
  (ccl:function-name gf)
  #+ALLEGRO
  (mop::generic-function-name gf)
  #+LISPWORKS
  (hcl::generic-function-name gf)
  #-(or MCL ALLEGRO LISPWORKS)
  (error "Need implementation of mopu-generic-function-name"))

;;; ---------------------------------------------------------------------------

(defun mopu-find-method (gf-name qualifiers &rest specializers)
  #+MCL
  (and (fboundp gf-name)
       (let ((def (fdefinition gf-name)))
         (and def
              (find-method def qualifiers
                           (mapcar #'find-class specializers) nil)))))

;;; ---------------------------------------------------------------------------

(defgeneric mopu-class-slot-names (class)
  (:documentation "Returns a list of the names of the slots of a class (including
both direct and inherited slots). It's like class-slot-names but on the
class, not an instance of the class.")
  (:method ((class standard-class))
           #+(and DIGITOOL (not MCL-COMMON-MOP-SUBSET))
           (mapcar #'ccl:slot-definition-name (ccl:class-instance-slots class))
           #+(OR ALLEGRO LISPWORKS4)
           (progn
             (finalize-class-if-necessary class)
             (mapcar #'clos:slot-definition-name (clos:class-slots class)))
           #+MCL-COMMON-MOP-SUBSET
           (mapcar #'ccl:slot-definition-name (class-slots class))
           #-(OR ALLEGRO DIGITOOL LISPWORKS4 OPENMCL)
           (class-slot-names (allocate-instance class)))
  (:method ((class symbol))
           (cond ((find-structure class nil)
                  #+MCL
                  (mapcar #'first (rest (aref (find-structure class nil) 1)))
                  #-(or MCL)
                  (nyi class))
                 ((find-class class nil)
                  (mopu-class-slot-names (find-class class)))
                 (t
                  (error "Cannot find class or structure ~A" class))))
  (:method ((class standard-object))
           (mopu-class-slot-names (class-of class)))
  (:method ((class structure-object))
           (mopu-class-slot-names (type-of class))))

;;; ---------------------------------------------------------------------------

(defgeneric mopu-class-slot-information (class slot-name)
  (:documentation "Returns a property list descring the slot.")
  (:method ((class symbol) slot-name)
           (mopu-class-slot-information (find-class class) slot-name))
  (:method ((object standard-object) slot-name)
           (mopu-class-slot-information (class-of object) slot-name))
  (:method ((class standard-class) slot-name)
           (declare (ignorable slot-name))
           (or #+MCL-COMMON-MOP-SUBSET
               (multiple-value-bind (slot-info indirect-slot?)
                                    (mopu-find-slot-definition class slot-name)
                 (values `(:name ,(ccl:slot-definition-name slot-info)
                                 ,@(when (eq (ccl:slot-definition-allocation slot-info) :class)
                                     `(:allocation ,(ccl:slot-definition-allocation slot-info)))
                                 :initargs ,(ccl:slot-definition-initargs slot-info)
                                 :initform ,(ccl:slot-definition-initform slot-info)
                                 ,@(when (and (not (eq (ccl:slot-definition-type slot-info) t))
                                              (not (eq (ccl:slot-definition-type slot-info) nil)))
                                     `(:type ,(ccl:slot-definition-type slot-info)))
                                 ,@(unless indirect-slot?
                                     `(:readers ,(ccl:slot-definition-readers slot-info)
                                                :writers ,(ccl:slot-definition-writers slot-info))))
                         t))
               #-MCL-COMMON-MOP-SUBSET
               (values nil nil))))

;;; ---------------------------------------------------------------------------

(defgeneric mopu-find-slot-definition (class slot-name)
  (:method ((class symbol) slot-name)
           (mopu-find-slot-definition (find-class class) slot-name))
  (:method ((object standard-object) slot-name)
           (mopu-find-slot-definition (class-of object) slot-name))
  (:method ((class standard-class) slot-name)
           (let* ((indirect-slot? nil)
                  (slot-info 
                   (or (find slot-name (class-direct-slots class)
                             :key #'mopu-slot-definition-name)
                       (and (setf indirect-slot? t)
                            (find slot-name (class-slots class)
                                  :key #'mopu-slot-definition-name)))))
             (values slot-info indirect-slot?))))

;;; ---------------------------------------------------------------------------

(defgeneric mopu-slot-definition-name (slot-def)
  (:method (slot-def)
           #+MCL
           (ccl:slot-definition-name slot-def)
           #+SBCL
           (sb-mop:slot-definition-name slot-def)
           ))
  
;;; ---------------------------------------------------------------------------

(defgeneric mopu-direct-slot-names (class)
  (:documentation "")
  (:method ((class symbol))
           (mopu-direct-slot-names (find-class class)))
  (:method ((class standard-object))
           (mopu-direct-slot-names (class-of class)))
  (:method ((class standard-class))
           #+(and DIGITOOL (not MCL-COMMON-MOP-SUBSET))
           (mapcar #'ccl:slot-definition-name (ccl:class-direct-instance-slots class))
           #+OPENMCL
           (mapcar #'ccl:slot-definition-name (ccl:class-direct-slots class))
           #-(or OPENMCL DIGITOOL)
           (error "Don't know how to mopu-direct-slot-names")))

;;; ---------------------------------------------------------------------------

(defgeneric mopu-reader-method-p (method)
  (:documentation "")
  (:method ((m #+Allegro   mop::standard-reader-method
               #+MCL       ccl:standard-reader-method
               #+Lispworks hcl::standard-reader-method
               #+SBCL      sb-pcl::standard-reader-method))
           (values t))
  (:method ((m t))
           (values nil)))

;;; ---------------------------------------------------------------------------
 
(defgeneric mopu-writer-method-p (method)
  (:documentation "")
  (:method ((m #+Allegro   mop::standard-writer-method
               #+MCL       ccl:standard-writer-method
               #+Lispworks hcl::standard-writer-method
               #+SBCL      sb-pcl::standard-writer-method))
           (values t))
  (:method ((m t))
           (values nil)))


;;; ---------------------------------------------------------------------------
;;; l1-methods
;;; ---------------------------------------------------------------------------

(defun map-methods (classname fn)
  "Applys fn to all of the methods of the class named classname. The function ~
should take two arguments: a generic function and a method."
  (let ((class (find-class classname nil)))
    (when class
      (loop for gf in (mopu-specializer-direct-generic-functions class) do
            (loop for m in (mopu-generic-function-methods gf) do
                  (when (member class (mopu-method-specializers m))
                    (funcall fn gf m)))))))

;;; ---------------------------------------------------------------------------

(defun remove-methods (classname &key (verbose? t))
  (let ((class (find-class classname nil)))
    (if class
      (map-methods classname 
                   (lambda (gf m) 
                     (when verbose?
                       (format t "~&~A" m))
                     (remove-method gf m)))
      (when verbose?
        (warn "Class '~A' not found." classname)))))

;;; ---------------------------------------------------------------------------

(defun remove-methods-if (classname predicate &key (verbose? t))
  (let ((class (find-class classname nil)))
    (if class
      (map-methods classname 
                   (lambda (gf m) 
                     (when (funcall predicate gf m)
                       (when verbose?
                         (format t "~&~A" m))
                       (remove-method gf m))))
      (when verbose?
        (warn "Class '~A' not found." classname)))))

;;; ---------------------------------------------------------------------------

(defun display-methods (classname)
  (map-methods classname (lambda (gf m) (format t "~&~A~%  ~A" gf m))))

;;; ---------------------------------------------------------------------------

(defun generic-function-list (classname)
  (let ((result nil))
    (map-methods classname
                 (lambda (gf m) 
                   (declare (ignore m))
                   (pushnew gf result)))
    result))

;;; ---------------------------------------------------------------------------

(defgeneric direct-specializers-of (class &key writers? readers? other? short-form?)
  (:method ((classname symbol) &key (short-form? t) (writers? t) (readers? t) 
            (other? t))
           ;;?? does this already have a name in the MOP
           (let ((result nil))
             (map-methods classname
                          (lambda (gf m)
                            (declare (ignore gf))
                            (when (or (and (mopu-reader-method-p m)
                                           readers?)
                                      (and (mopu-writer-method-p m)
                                           writers?)
                                      (and (not (mopu-reader-method-p m))
                                           (not (mopu-writer-method-p m))
                                           other?))
                              (push m result))))
             (when short-form?
               (setf result (delete-duplicates (mapcar #'mopu-method-name result))))
             (nreverse result)))
  (:method ((class standard-object) &rest args &key writers? readers? other? short-form?)
           (declare (ignore writers? readers? other? short-form?))
           (apply #'direct-specializers-of (class-name (class-of class)) args))
  (:method ((class standard-class) &rest args &key writers? readers? other? short-form?)
           (declare (ignore writers? readers? other? short-form?))
           (apply #'direct-specializers-of (class-name class) args)))

;;; ---------------------------------------------------------------------------

(defun all-specializers-of (class &rest args &key short-form? writers? readers? other?
                                  (ignore-classes '(t standard-object)))
  (declare (ignore writers? readers? other? short-form?))
  (remf args :ignore-classes)
  (setf ignore-classes (mapcar #'find-class (if (consp ignore-classes)
                                              ignore-classes
                                              (list ignore-classes))))
  (delete-duplicates
   (apply #'append
          (mapcar (lambda (super-class)
                    (unless  (member super-class ignore-classes)
                      (apply #'direct-specializers-of super-class args)))
                  (mopu-class-precedence-list class)))))


(defgeneric subclasses* (class)
  (:documentation "Returns all of the subclasses of the class including the class itself.")
  (:method ((class t))
           (let ((result nil))
             (map-subclasses class (lambda (class)
                                     (push class result)))
             (nreverse result))))

;;; ---------------------------------------------------------------------------

(defgeneric map-subclasses (class fn &key proper)
  (:documentation "Applies fn to each subclass of class. If proper is true, then
the class itself is not included in the mapping. Proper defaults to nil.")
  (:method ((class symbol) fn &key proper)
           (map-subclasses (find-class class) fn :proper proper))
  (:method ((class t) fn &key proper)
           (let ((mapped (make-hash-table :test #'eq)))
             (labels ((mapped-p (class)
                      (gethash class mapped))
                      (do-it (class root)
                        (unless (mapped-p class)
                          (setf (gethash class mapped) t)
                          (unless (and proper root)
                            (funcall fn class))
                          (mapc (lambda (class)
                                  (do-it class nil))
                                (mopu-class-direct-subclasses class)))))
               (do-it class t)))))


;;; ---------------------------------------------------------------------------
;;; from The Art of the MOP
;;; ---------------------------------------------------------------------------

;; 52
#+Old
(defgeneric subclasses* (class)
  (:documentation "Returns all of the subclasses of the class including the class itself.")
  (:method ((class symbol))
           (subclasses* (find-class class)))
  (:method ((class t))
           (remove-duplicates
            (cons class
                  (reduce #'append 
                          (mapcar #'subclasses*
                                  (ccl:class-direct-subclasses class)))))))

;;; ---------------------------------------------------------------------------

;; 52
(defun subclasses (class)
  "Returns all of the subclasses of a class."
  (typecase class
    (symbol (remove (find-class class) (subclasses* class)))
    (standard-class (remove class (subclasses* class)))))

;;; ---------------------------------------------------------------------------

;; 57
(defun in-order-p (c1 c2)
  (flet ((in-order-at-subclass-p (sub)
           (let ((cpl (mopu-class-precedence-list sub)))
             (not (null (member c2 (cdr (member c1 cpl))))))))
    (or (eq c1 c2)
        (every #'in-order-at-subclass-p 
               (intersection (subclasses* c1)
                             (subclasses* c2))))))

;;; ---------------------------------------------------------------------------
;;; structures
;;; ---------------------------------------------------------------------------

(defun find-structure (name &optional (errorp t))
  "get-desfstruct-description name

If `name' has been defined as a structure then return its
description.  Otherwise signal an error if errorp is t."
  (let ((found (or #+MCL (ccl::structure-class-p name)
                   #+Lispworks4 (structure:type-structure-predicate name))))
    (cond #+(not (or MCL Lispworks4)) 
          (t (error "no implementation exists"))
          (found
           (values t))
          (t
           (when errorp
             (error "~s is not the name of a defstruct." name))))))
                

;;; ---------------------------------------------------------------------------
;;; From Charles
;;; ---------------------------------------------------------------------------

(defun find-slot (class slot-name)
  (find slot-name (mopu-class-slot-names class)))

;;; ---------------------------------------------------------------------------

(defun mopu-slot-definition-initform (slotdef)
  #+(or Lucid Lispworks Allegro)
  (clos:slot-definition-initform slotdef)
  #+MCL
  (ccl:slot-definition-initform slotdef)
  #-(or MCL Lucid Lispworks Allegro)
  (slot-definition-initform slotdef))

#+MCL
(defun show-class-path (sub super)
  (let ((candidates (ccl::class-direct-superclasses sub)))
       (cond ((null candidates) nil) 
             ((find super candidates)
              (values super))
             (t (dolist (c candidates)
                  (let ((it (show-class-path c super))) 
                    (when it 
                      (print it)  
                      (return c))))))))

(defun mopu-arglist (symbol)
  "Returns two values, the arglist of symbol"
  #+MCL
  (ccl:arglist symbol)
  #+lispworks
  (lw:function-lambda-list symbol))

;;; ---------------------------------------------------------------------------

(defgeneric mopu-class-initargs (class) 
  (:method ((class standard-class))
           #+MCL
           (ccl::class-slot-initargs class)
           #+lispworks
           (lw-tools::class-initargs class)
           #-(or MCL LISPWORKS4)
           (error "don't know how to mopu-class-initargs"))
  (:method ((class symbol))
           (mopu-class-initargs (find-class class)))
  (:method ((class standard-object))
           (mopu-class-initargs (class-of class))))

;;; ---------------------------------------------------------------------------

(defmethod mopu-eql-specializer-p (s)
  (or
   #+MCL-COMMON-MOP-SUBSET
   (when (ccl::eql-specializer-p s) 
     (list 'eql (ccl:eql-specializer-object s)))
   #+MCL
   (when (consp s)
     s)))

;;; ---------------------------------------------------------------------------
;;; leaf classes
;;; ---------------------------------------------------------------------------

(defgeneric leaf-class-p (class)
  (:documentation "Returns true if the class has no subclasses."))

;;; ---------------------------------------------------------------------------

(defmethod leaf-class-p ((class standard-class))
  (map-subclasses class
                  (lambda (subclass)
                    (declare (ignore subclass))
                    (return-from leaf-class-p nil))
                  :proper t)
  (values t))

;;; ---------------------------------------------------------------------------

(defmethod leaf-class-p ((class-name symbol))
  (leaf-class-p (find-class class-name)))

;;; ---------------------------------------------------------------------------

(defun leaf-subclasses (class)
  (let ((result nil))
    (map-subclasses class 
                    (lambda (class)
                      (when (leaf-class-p class)
                        (push class result))))
    (values result)))

;;; ---------------------------------------------------------------------------

#+No
;;?? Gary King 2005-11-16: bad lisp, down boy
(defgeneric (setf class) (class object)
  (:documentation "")
  (:method ((class symbol) (object standard-object))
           (change-class object (find-class class)))
  (:method ((class standard-object) (object standard-object))
           (setf (class object) (class-of class)))
  (:method ((class standard-class) (object standard-object))
           (change-class object class)))

#|

;;?? how to munge lambda-list properly...

(defmacro build-generalized-mopu-method (name lambda-list &body body)
  `(defgeneric ,name ,lambda-list 
    (:method ((class standard-class))
             ,@body)
    (:method ((class symbol))
             (,name (find-class class)))
    (:method ((class standard-object))
             (,name (class-of class)))))

(build-generalized-mopu-method mopu-class-initargs (class)
  #+MCL
  (ccl::class-slot-initargs class)
  #+lispworks
  (lw-tools::class-initargs class)
  #-(or MCL LISPWORKS4)
  (error "don't know how to mopu-class-initargs"))

(build-generalized-mopu-method leaf-subclasses (class)
  (let ((result nil))
    (map-subclasses class 
                    (lambda (class)
                      (when (leaf-class-p class)
                        (push class result))))
    (values result)))

|#

;;; ---------------------------------------------------------------------------

(defun class-name-of (object)
  "Returns the name of OBJECT's class."
  (class-name (class-of object)))
                               


;;; ***************************************************************************
;;; *                              End of File                                *
;;; ***************************************************************************
