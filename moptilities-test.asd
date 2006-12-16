;;;-*- Mode: Lisp; Package: asdf-moptilities -*-

#| copyright

See the file COPYING for details

|#

(defpackage :asdf-moptilities-test (:use #:asdf #:cl))
(in-package :asdf-moptilities-test)

(defsystem moptilities-test
  :version "0.1"
  :author "Gary Warren King <gwking@metabang.com>"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT Style license"
  :description "Test for Common Lisp MOP utilities"

  :components ((:module "test"
                        :components ((:file "package")
                                     (:file "tests" :depends-on ("package"))))
               
               (:module "dev"
                        :components ((:static-file "notes.text"))))
  :in-order-to ((test-op (load-op moptilities-test)))
  :perform (test-op :after (op c)
                    (describe 
		     (funcall (intern (symbol-name '#:run-tests) :lift) 
			      :suite '#:moptilities-test)))
  :depends-on (moptilities lift))

;;; ---------------------------------------------------------------------------

(defmethod operation-done-p 
           ((o test-op)
            (c (eql (find-system 'moptilities-test))))
  (values nil))

