;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-
;;;
;;; ASDF system definition for jackdaw-cli — a command-line interface for
;;; jackdaw.

(asdf:defsystem :jackdaw-cli
  :version      "0.1"
  :description  "command-line interface for jackdaw"
  :author       "Bastiaan van der Weij"
  :license      "MIT"
  :depends-on   (:unix-opts :jackdaw)
  :components   ((:file "jackdaw-cli")))


(asdf:defsystem :jackdaw-cli/tests
  :version      "0.1"
  :description  "tests for jackdaw-cli"
  :author       "Bastiaan van der Weij"
  :license      "MIT"
  :components   ((:file "tests"))
  :depends-on   (:jackdaw-cli))

