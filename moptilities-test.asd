;;;-*- Mode: Lisp; Package: asdf-moptilities -*-

#| copyright

See the file COPYING for details

|#

(defsystem "moptilities-test"
  :version "0.1"
  :author "Gary Warren King <gwking@metabang.com>"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT Style license"
  :description "Test for Common Lisp MOP utilities"
  :components ((:module
                "tests"
                :components ((:file "package")
                             (:file "tests" :depends-on ("package"))
                             (:file "copy-template" :depends-on ("tests"))))
               (:module "dev"
                        :components ((:static-file "notes.text"))))
  :depends-on ("moptilities" "lift")
  :perform (test-op (o c) (symbol-call :lift :run-tests :config :generic)))
