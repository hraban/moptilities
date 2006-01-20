;;;; -*- Mode: Common-Lisp; Package: METABANG.MOPTILITIES; Base: 10 -*-
;;;; some definitions are from the Art of the MOP

(in-package cl-user)

(defpackage "METABANG.MOPTILITIES"
  (:documentation "Moptilities builds on the Lisp Meta-Object Protodol (MOP).")
  (:use "CLOSER-COMMON-LISP")
  (:nicknames "MOPU" "MOPTILITIES")
  (:export
   #:map-methods 
   #:remove-methods
   #:remove-methods-if
   #:direct-specializers-of
   #:specializers-of
   
   #:generic-functions
   
   #:finalize-class-if-necessary
   
   #:reader-method-p 
   #:writer-method-p
   #:method-name
   #:get-slot-definition

   #:slot-names
   #:direct-slot-names
   #:slot-properties
      
   #:map-subclasses
   #:superclasses
   #:subclasses
   #:direct-subclasses
   #:direct-superclasses
   #:subclassp
       
   #:finalize-class-if-necessary
   #:leaf-class-p
   #:leaf-subclasses
   
   #+Ignore #:path-from-class-to-class
   
   #:class
   #:get-class
   #:get-method
   #:class-name-of
   #:copy-template                      ; lousy name

   #:eql-specializer-p 
   
   #:mopu-arglist
   #:mopu-class-initargs)
  
  (:export 
   #:generic-function-methods
   #:method-specializers)
  
  ;; should just re-export the whole mop
  (:export
   #:method-generic-function
   #:generic-function-name
   ))

(in-package "MOPTILITIES")

;;; ---------------------------------------------------------------------------

(defmacro NYI (function-name &rest args)
  "Signals an error saying that this function is not yet implemented.  The args
are ignored, but by supplying args from the calling function, you can get them
ignored by the compiler."
  `(error "The function ~A is not yet implemented for ~A ~A on ~A."
          ,function-name
	  (lisp-implementation-type)
	  (lisp-implementation-version)
	  (machine-type)
          . ,args))

;;; ---------------------------------------------------------------------------

(defgeneric get-class (thing)
  (:documentation "Returns the class of thing or nil if the class cannot be found. Thing can be a class, an object representing a class or a symbol naming a class. Get-class is like find-class only not as particular.")
  (:method ((thing symbol))
           (find-class thing nil))
  (:method ((thing standard-object)) 
           (class-of thing))
  (:method ((thing class))
           thing))

;;; ---------------------------------------------------------------------------

(defmethod subclassp (child parent)
  "Returns t if child is a subclass of the parent."
  (finalize-class-if-necessary child)
  (not (null (find (get-class parent) (superclasses child)))))

;;; ---------------------------------------------------------------------------

(defun finalize-class-if-necessary (thing)
  "Finalizes thing if necessary. Thing can be a class, object or symbol naming a class. Returns the class of thing."
  (let ((class (get-class thing)))
    (unless (class-finalized-p class)
      (finalize-inheritance class))
    (values class)))

;;; ---------------------------------------------------------------------------

(defun superclasses (thing &key (proper? t))
  "Returns a list of superclasses of thing. Thing can be a class, object or symbol naming a class. The list of classes returned is 'proper'; it does not include the class itself."
  (finalize-class-if-necessary thing) 
  (let ((result (class-precedence-list (get-class thing))))
    (if proper? (rest result) result)))

;;; ---------------------------------------------------------------------------

(defun direct-superclasses (thing)
  "Returns the immediate superclasses of thing. Thing can be a class, object or symbol naming a class."
  (class-direct-superclasses (get-class thing)))

;;; ---------------------------------------------------------------------------

(defun direct-subclasses (thing)
  "Returns the immediate subclasses of thing. Thing can be a class, object or symbol naming a class."
  (class-direct-subclasses (get-class thing)))

;;; ---------------------------------------------------------------------------

(defgeneric method-name (method)
  (:method ((method standard-method))
           (generic-function-name (method-generic-function method))))

;;; ---------------------------------------------------------------------------

(defgeneric get-method (function qualifiers &rest specializers)
  (:documentation "")
  (:method ((function symbol) qualifiers &rest specializers)
           (declare (dynamic-extent specializers))
           (if (fboundp function)
             (apply #'get-method
                    (fboundp function)
                    qualifiers specializers)
             nil))
  (:method ((function standard-generic-function) qualifiers &rest specializers)
           (declare (dynamic-extent specializers))
           (find-method function qualifiers
               (mapcar #'get-class specializers) nil)))

;;; ---------------------------------------------------------------------------

(defgeneric slot-names (class)
  (:documentation "Returns a list of the names of the slots of a class (including
both direct and inherited slots). It's like class-slot-names but on the
class, not an instance of the class.")
  (:method ((class class))
           (finalize-class-if-necessary class)
           (mapcar #'slot-definition-name (class-slots class)))
  (:method ((class symbol))
           (cond ((get-structure class nil)
                  #+MCL
                  (mapcar #'first (rest (aref (get-structure class nil) 1)))
                  #-(or MCL)
                  (nyi "slot-names for structures"))
                 ((find-class class nil)
                  (slot-names (find-class class)))
                 (t
                  (error "Cannot find class or structure ~A" class))))
  (:method ((class standard-object))
           (slot-names (class-of class)))
  (:method ((class structure-object))
           (slot-names (type-of class))))

;;; ---------------------------------------------------------------------------

(defgeneric slot-properties (class slot-name)
  (:documentation "Returns a property list descring the slot named slot-name in class.")
  (:method ((class symbol) slot-name)
           (slot-properties (find-class class) slot-name))
  (:method ((object standard-object) slot-name)
           (slot-properties (class-of object) slot-name))
  (:method ((class class) slot-name)
           (declare (ignorable slot-name))
           (multiple-value-bind (slot-info indirect-slot?)
                                (get-slot-definition class slot-name)
             `(:name ,(slot-definition-name slot-info)
                     ,@(when (eq (slot-definition-allocation slot-info) :class)
                         `(:allocation ,(slot-definition-allocation slot-info)))
                     :initargs ,(slot-definition-initargs slot-info)
                     :initform ,(slot-definition-initform slot-info)
                     ,@(when (and (not (eq (slot-definition-type slot-info) t))
                                  (not (eq (slot-definition-type slot-info) nil)))
                         `(:type ,(slot-definition-type slot-info)))
                     ,@(unless indirect-slot?
                         `(:readers ,(slot-definition-readers slot-info)
                                    :writers ,(slot-definition-writers slot-info)))))))

;;; ---------------------------------------------------------------------------

(defgeneric get-slot-definition (class slot-name)
  (:method ((class symbol) slot-name)
           (get-slot-definition (find-class class) slot-name))
  (:method ((object standard-object) slot-name)
           (get-slot-definition (class-of object) slot-name))
  (:method ((class class) slot-name)
           (let* ((indirect-slot? nil)
                  (slot-info 
                   (or (find slot-name (class-direct-slots class)
                             :key #'slot-definition-name)
                       (and (setf indirect-slot? t)
                            (find slot-name (class-slots class)
                                  :key #'slot-definition-name)))))
             (values slot-info indirect-slot?))))
  
;;; ---------------------------------------------------------------------------

(defgeneric direct-slot-names (class)
  (:documentation "")
  (:method ((class symbol))
           (direct-slot-names (find-class class)))
  (:method ((class standard-object))
           (direct-slot-names (class-of class)))
  (:method ((class class))
           (mapcar #'slot-definition-name (class-direct-slots class))))

;;; ---------------------------------------------------------------------------

(defgeneric reader-method-p (thing)
  (:documentation "Returns true if thing is a reader method (i.e., a subclass of standard-reader-method).")
  (:method ((m standard-reader-method))
           (values t))
  (:method ((m t))
           (values nil)))

;;; ---------------------------------------------------------------------------
 
(defgeneric writer-method-p (thing)
  (:documentation "Returns true if thing is a writer method (i.e., a subclass of standard-writer-method).")
  (:method ((m standard-writer-method))
           (values t))
  (:method ((m t))
           (values nil)))


;;; ---------------------------------------------------------------------------

(defun map-methods (thing fn)
  "Applys fn to all of the methods of thing (which can be a class, object or symbol naming a class). The function should take two arguments: a generic function and a method."
  (let ((class (get-class thing)))
    (when class
      (loop for gf in (specializer-direct-generic-functions class) do
            (loop for m in (generic-function-methods gf) do
                  (when (member class (method-specializers m))
                    (funcall fn gf m)))))))

;;; ---------------------------------------------------------------------------

(defun remove-methods (thing &rest args 
                             &key (dry-run? nil) (verbose? dry-run?)
                             (ignore-errors? nil))
  "Removes all methods associated with thing. Thing can be a class, object representing a class or symbol naming a class. If dry-run? is true \(and verbose? is also true\), then the methods that would be removed are printed but no methods are actually removed. Returns the number of methods that are removed \(or that would have been removed if dry-run? is true\)."
  (declare (ignore verbose? ignore-errors?)
           (dynamic-extent args))
  (apply #'remove-methods-if thing (constantly t) args))

;;; ---------------------------------------------------------------------------

(defun remove-methods-if (thing predicate &key (dry-run? nil) (verbose? dry-run?)
                                (ignore-errors? nil))
  "Removes all methods associated with thing that pass a predicate. Thing can be a class, object representing a class or symbol naming a class. The predicate should be a function of two arguments: a generic-function and a method. If dry-run? is true \(and verbose? is also true\), then the methods that would be removed are printed but no methods are actually removed. Returns the number of methods that are removed \(or that would have been removed if dry-run? is true\)."
  (let ((class (get-class thing))
        (count 0))
    (if class
      (map-methods thing 
                   (lambda (gf m) 
                     (when (funcall predicate gf m)
                       (incf count)
                       (when verbose?
                         (format t "~&~A" m))
                       (unless dry-run?
                         (remove-method gf m)))))
      (unless ignore-errors?
        (error "Class '~A' not found." thing)))
    (values count)))

;;; ---------------------------------------------------------------------------

#+Ignore
(defun display-methods (classname)
  (map-methods classname (lambda (gf m) (format t "~&~A~%  ~A" gf m))))

;;; ---------------------------------------------------------------------------

(defun generic-functions (thing)
  "Returns a list of all of the methods associated with thing. Thing can be a class, object, or symbol naming a class."
  (let ((result nil))
    (map-methods thing
                 (lambda (gf m) 
                   (declare (ignore m))
                   (pushnew gf result)))
    result))

;;; ---------------------------------------------------------------------------

(defun direct-specializers-of (thing &key (writers? t) (readers? t)
                                     (other? t) (short-form? t))
  "Returns a list of the direct specializers of thing. Thing can a class, object representing a class or symbol naming a class. The keyword arguments :readers?, :writers?, and :other? control which specializers are returned \(reader methods, writer methods and other methods respectively\). The keyword argument :short-form? controls whether a list of methods is returned or just a list of names."
  (let ((result nil)
        (transform (if short-form? #'method-name #'identity)))
    (map-methods thing
                 (lambda (gf m)
                   (declare (ignore gf))
                   (when (or (and (reader-method-p m)
                                  readers?)
                             (and (writer-method-p m)
                                  writers?)
                             (and (not (reader-method-p m))
                                  (not (writer-method-p m))
                                  other?))
                     (push (funcall transform m) result))))
    (nreverse result)))

;;; ---------------------------------------------------------------------------

(defun specializers-of (class &rest args &key short-form? writers? readers? other?
                              (ignore-classes '(t standard-object)))
  (declare (ignore writers? readers? other? short-form?))
  "Like direct-specializers-of but returns all the specializers, not just the direct ones."
  (remf args :ignore-classes)
  (setf ignore-classes (mapcar #'find-class (if (consp ignore-classes)
                                              ignore-classes
                                              (list ignore-classes))))
  (delete-duplicates
   (apply #'nconc
          (mapcar (lambda (super-class)
                    (unless  (member super-class ignore-classes)
                      (apply #'direct-specializers-of super-class args)))
                  (superclasses class)))))

;;; ---------------------------------------------------------------------------

(defun map-subclasses (class fn &key proper?)
  "Applies fn to each subclass of class. If proper? is true, then
the class itself is not included in the mapping. Proper? defaults to nil."
  (let ((mapped (make-hash-table :test #'eq)))
    (labels ((mapped-p (class)
               (gethash class mapped))
             (do-it (class root)
               (unless (mapped-p class)
                 (setf (gethash class mapped) t)
                 (unless (and proper? root)
                   (funcall fn class))
                 (mapc (lambda (class)
                         (do-it class nil))
                       (class-direct-subclasses class)))))
      (do-it (get-class class) t))))

;;; ---------------------------------------------------------------------------

(defun subclasses (class &key (proper? t))
  "Returns all of the subclasses of the class including the class itself."
  (let ((result nil))
    (map-subclasses class (lambda (class)
                            (push class result))
                    :proper? proper?)
    (nreverse result)))

;;; ---------------------------------------------------------------------------

(defun in-order-p (c1 c2)
  (flet ((in-order-at-subclass-p (sub)
           (let ((cpl (superclasses sub)))
             (not (null (member c2 (cdr (member c1 cpl))))))))
    (or (eq c1 c2)
        (every #'in-order-at-subclass-p 
               (intersection (subclasses c1 :proper? nil)
                             (subclasses c2 :proper? nil))))))

;;; ---------------------------------------------------------------------------
;;; structures
;;; ---------------------------------------------------------------------------

(defun get-structure (name &optional (errorp t))
  "get-desfstruct-description name

If `name' has been defined as a structure then return its
description.  Otherwise signal an error if errorp is t."
  (declare (ignorable name errorp))
  #+(or MCL LISPWORKS4)
  (let ((found (or #+MCL (ccl::structure-class-p name)
                   #+Lispworks4 (structure:type-structure-predicate name))))
    (cond (found
           (values t))
          (t
           (when errorp
             (error "~s is not the name of a defstruct." name)))))
  #-(or MCL LISPWORKS)
  (nyi "get-structure")) 

;;; ---------------------------------------------------------------------------

#+Ignore
(defun path-from-class-to-class (sub super)
  "Returns a list representing a path of classes that runs from sub to super" 
  (let ((super-class (get-class super)))
    (let ((candidates (direct-superclasses sub)))
      (cond ((null candidates) nil) 
            ((find super-class candidates)
             (values super-class))
            (t (dolist (c candidates)
                 (let ((it (path-from-class-to-class c super-class))) 
                   (when it 
                     (print it)  
                     (return c)))))))))

;;; ---------------------------------------------------------------------------

(defun mopu-arglist (symbol)
  "Returns two values, the arglist of symbol"
  #+MCL
  (ccl:arglist symbol)
  #+lispworks
  (lw:function-lambda-list symbol)
  #+allegro
  (common-lisp-user::arglist symbol)
  #-(or MCL LISPWORKS ALLEGRO)
  (nyi "mopu-arglist"))

;;; ---------------------------------------------------------------------------

(defun mopu-class-initargs (thing) 
  (let ((class (get-class thing)))
    (declare (ignorable class))
    #+MCL
    (ccl::class-slot-initargs class)
    #+lispworks
    (lw-tools::class-initargs class)
    #-(or MCL LISPWORKS4)
    (nyi "mopu-class-initargs")))

;;; ---------------------------------------------------------------------------

(defgeneric eql-specializer-p (thing)
  (:documentation "If thing is an eql-specializer, returns a representation of thing as \(eql <object>\).")
  (:method ((thing eql-specializer))
           (list 'eql (eql-specializer-object thing)))
  (:method ((thing t))
    (values nil))
  #+DIGITOOL
  (:method ((thing cons))
           thing))

;;; ---------------------------------------------------------------------------
;;; leaf classes
;;; ---------------------------------------------------------------------------

(defun leaf-class-p (thing)
  "Returns true if the class has no subclasses."
  (map-subclasses thing
                  (lambda (subclass)
                    (declare (ignore subclass))
                    (return-from leaf-class-p nil))
                  :proper? t)
  (values t))

;;; ---------------------------------------------------------------------------

(defun leaf-subclasses (thing)
  "Returns a list of subclasses of thing that have no subclasses of their own; i.e., the leaves of the class tree rooted at thing. Thing can be a class, object or symbol naming a class." 
  (let ((result nil))
    (map-subclasses thing 
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
  (:method ((class class) (object standard-object))
           (change-class object class)))

#| a macro to handle this would be good but we don't have much machinery 
   around to help since moptilities is supposed to down in the foundations.


;;?? grrr, what is minimal! What is the root!!
(defmacro with-unique-names ((&rest vars) &body body)
  "Binds the symbols in VARS to gensyms.  cf with-gensyms."
  (assert (every #'symbolp vars) () "Can't rebind an expression.")
  `(let ,(mapcar #'(lambda (x) `(,x (gensym))) vars)
     ,@body))

;;?? how to munge lambda-list properly...

(defmacro build-generalized-mopu-method (name lambda-list &body body)
  (with-unique-names (args)
    (let ((arglist (rest lambda-list)))
      `(defgeneric ,name ,lambda-list
         ; (:documentation ,documentation)
         (:method ((classname symbol) ,@arglist)
                  ,@body)
         (:method ((object standard-object) &rest ,args ,@arglist)
                  (declare (dynamic-extent ,args))
                  (apply #',name (class-name-of object) ,args))
         (:method ((class class) &rest ,args ,@arglist)
                  (declare (dynamic-extent ,args))
                  (apply #',name (class-name class) ,args))))))
              
;;; ---------------------------------------------------------------------------

(build-generalized-mopu-method direct-specializers-of (class &key writers? readers? other? short-form?)
  ;;?? does this already have a name in the MOP
  (let ((result nil))
    (map-methods classname
                 (lambda (gf m)
                   (declare (ignore gf))
                   (when (or (and (reader-method-p m)
                                  readers?)
                             (and (writer-method-p m)
                                  writers?)
                             (and (not (reader-method-p m))
                                  (not (writer-method-p m))
                                  other?))
                     (push m result))))
    (when short-form?
      (setf result (delete-duplicates (mapcar #'method-name result))))
    (nreverse result)))

(build-generalized-mopu-method mopu-class-initargs (class)
  #+MCL
  (ccl::class-slot-initargs class)
  #+lispworks
  (lw-tools::class-initargs class)
  #-(or MCL LISPWORKS4)
  (nyi "mopu-class-initargs"))

(build-generalized-mopu-method leaf-subclasses (class)
  (let ((result nil))
    (map-subclasses class 
                    (lambda (class)
                      (when (leaf-class-p class)
                        (push class result))))
    (values result)))

|#

;;; ---------------------------------------------------------------------------

(defgeneric class-name-of (thing)
  (:documentation "Returns the name of thing's class.")
  (:method ((thing standard-object)) 
           (class-name (class-of thing)))
  (:method ((thing class))
           (class-name thing)))


;;; ---------------------------------------------------------------------------
;;; copy-template
;;; suppose you have an object
;;; ? (defclass foo ()
;;;     ((test :accessor test :initform #'equal :initarg :test))) =>
;;; #<STANDARD-CLASS FOO>
;;; 
;;; ? (setf *foo* (make-instance 'foo :test #'eql))
;;;
;;; ? (test *foo*) => #'eql 
;;; 
;;; Now you want to make another instance of foo that has the test as foo.
;;; 
;;; ? (setf *new-foo* (make-instance (type-of foo)))
;;;
;;; ? (test *new-foo*) => #'equal
;;;
;;; Wait, we wanted *new-foo* to have slot test to be #'eql.  This seems trival
;;; for simple objects, but consider this from make-filtered-graph
;;;
;;; (make-graph (type-of old-graph)
;;;              :vertex-test (vertex-test old-graph) 
;;;              :vertex-key (vertex-key old-graph)
;;;              :edge-test (edge-test old-graph)
;;;              :edge-key (edge-key old-graph)
;;;              :default-edge-type (default-edge-type old-graph)
;;;              :default-edge-class (default-edge-class old-graph)
;;;              :directed-edge-class (directed-edge-class old-graph)
;;;              :undirected-edge-class (undirected-edge-class old-graph))))
;;; Yuck!
;;; 
;;; So we offer copy-template as a reasonable, though not perfect, solution
;;; ---------------------------------------------------------------------------

(defmethod copy-template ((object standard-object))
  (apply 
   #'make-instance 
   (type-of object) 
   (loop 
     for (nil name nil initargs) in (mapcar (lambda (slot-value)
                                              (slot-properties 
                                               object slot-value))
                                            (slot-names object)) 
     when (and initargs
               (slot-boundp object name)) nconc
     (list (if (listp initargs)
             (first initargs)
             initargs) (slot-value object name)))))

;;; ***************************************************************************
;;; *                              End of File                                *
;;; ***************************************************************************
