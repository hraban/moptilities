;;;-*- Mode: Lisp; Package: ASDF-MOPTILITIES -*-

#| copyright

See the file COPYING for details

|#

(defpackage :asdf-moptilities (:use #:asdf #:cl))
(in-package :asdf-moptilities)

(defsystem moptilities
  :author "Gary Warren King <gwking@metabang.com>"
  :version "0.3"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT Style license"
  :description "Common Lisp MOP utilities"
  :long-description "MOP utilities is designed to provide a common interface between lisps and make the MOP easier to use."

  :components ((:file "moptilities")))
