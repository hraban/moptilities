;;;-*- Mode: Lisp; Package: asdf-moptilities -*-

#| copyright

See the file COPYING for details

|#

(defpackage #:asdf-moptilities (:use #:asdf #:cl))
(in-package #:asdf-moptilities)

(defsystem moptilities
  :author "Gary Warren King <gwking@metabang.com>"
  :version "0.3.13"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT Style license"
  :description "Common Lisp MOP utilities"
  :long-description "MOP utilities provide a common interface between lisps and make the MOP easier to use."

  :components
  ((:module
    "dev"
    :components ((:file "moptilities")
		 (:static-file "notes.text")))
   (:module
    "website"
    :components 
    ((:module "source"
	      :components ((:static-file "index.md"))))))
  :in-order-to ((test-op (load-op moptilities-test)))
  :perform (test-op :after (op c)
		    (funcall
		      (intern (symbol-name '#:run-tests) :lift)
		      :config :generic))
  :depends-on ((:version :closer-mop "0.55")))

(defmethod operation-done-p 
           ((o test-op) (c (eql (find-system 'moptilities))))
  (values nil))
