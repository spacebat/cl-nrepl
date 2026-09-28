(asdf:defsystem :nrepl
  :name "nrepl"
  :description "An implementation of the NREPL protocol for Common Lisp."

  :author "Steve Losh <steve@stevelosh.com>"
  :maintainer "Steve Losh <steve@stevelosh.com>"

  :license "EPL"
  :version "0.0.1"

  :depends-on (
               :bencode
               :bordeaux-threads
               :dissect
               :flexi-streams
               :fset
               :split-sequence
               :trivial-arguments
               :trivial-gray-streams
               :usocket
               :uuid
               )

  :in-order-to ((asdf:test-op (asdf:test-op :nrepl/test)))

  :serial t
  :components ((:module "vendor" :serial t
                :components ((:file "quickutils-package")
                             (:file "quickutils")))
               (:file "package")
               (:module "src" :serial t
                :components ((:file "utils")
                             (:file "sockets")
                             (:file "sessions")
                             (:file "output")
                             (:file "evaluation")
                             (:module "middleware" :serial t
                              :components ((:file "core")
                                           (:file "describe")
                                           (:file "documentation")
                                           (:file "macroexpand")
                                           (:file "eval")
                                           (:file "load-file")
                                           (:file "session")))
                             (:file "server")))))


(asdf:defsystem :nrepl/test
  :description "Tests for nrepl."
  :license "EPL"
  :depends-on (:nrepl :fiveam)
  :serial t
  :components ((:module "test" :serial t
                :components ((:file "package")
                             (:file "client")
                             (:file "tests"))))
  :perform (asdf:test-op (op system)
             (unless (uiop:symbol-call :nrepl.test :run-tests)
               (error "nrepl tests failed"))))
