;;;; nastafile.asd

(asdf:defsystem #:nastafile
  :description "A News Archive to Static File (HTML) Generator."
  :author "Abishek Goda <abishek.goda@hey.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on ("cl-nntp"
               "spinneret"
               "alexandria"
               "str"
               "anaphora"
               "local-time"
               "cl-date-time-parser"
               "cl-pop"
               "trivial-imap"
               "group-by"
               "rutils"
               "log4cl-extras"
               "trivial-garbage"
               "defmain"
               "deploy")
  :components ((:file "package")
               (:file "nastafile"))
  :defsystem-depends-on (:deploy)
  :build-operation "deploy-op"
  :build-pathname "nastafile"
  :entry-point "nastafile:main")
