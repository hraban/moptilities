;;;-*- Mode: Lisp; Package: asdf-moptilities -*-

#| copyright

See the file COPYING for details

|#

(defpackage #:asdf-moptilities-test (:use #:asdf #:cl))
(in-package #:asdf-moptilities-test)

(defsystem moptilities-test
  :version "0.1"
  :author "Gary Warren King <gwking@metabang.com>"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT Style license"
  :description "Test for Common Lisp MOP utilities"
  :components ((:module 
		"test"
		:components ((:file "package")
			     (:file "tests" :depends-on ("package"))))
               
               (:module "dev"
                        :components ((:static-file "notes.text"))))
  :depends-on (moptilities lift))


