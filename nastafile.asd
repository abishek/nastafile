;;;; nastafile.asd

(asdf:defsystem #:nastafile
  :description "A News Archive to Static File (HTML) Generator."
  :author "Abishek Goda <abishek.goda@hey.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-nntp #:spinneret)
  :components ((:file "package")
               (:file "nastafile")))
