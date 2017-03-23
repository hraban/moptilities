;;;-*- Mode: Lisp; Package: asdf-moptilities -*-

#| copyright

See the file COPYING for details

|#

(defsystem "moptilities"
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
  :in-order-to ((test-op (test-op "moptilities-test")))
  :depends-on ("closer-mop"
               (:feature :sbcl (:require :sb-introspect)))) ; for function-arglist
