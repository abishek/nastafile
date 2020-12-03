;;;; nastafile.lisp

(in-package #:nastafile)

(defun connect-to-newsgroup (&key server (port 119) group-name)
  (progn
    (cl-nntp:connect server port)
    (cl-nntp:group group-name)))

(defun show-article ()
    (dump-html-file (cl-nntp:head) (cl-nntp:body)))

(defun next-article ()
  (cl-nntp:next-article))

(defun construct-html (head body)
  (spinneret:with-html-string
    (:doctype)
    (:html
     (:head
      (:title "Nastafile HTML Dump")
      ;; bootstrap cdn for css alone
      (:link :rel "stylesheet"
             :href "https://cdn.jsdelivr.net/npm/bootstrap@4.5.3/dist/css/bootstrap.min.css"
             :integrity "sha384-TX8t27EcRE3e/ihU7zmQxVncDAy5uIKz4rEkgIXeMed4M0jlfIDPvg6uqKI2xXr2"
             :crossorigin "anonymous"))
     (:body
      (:header :class "container"
               (:div :class "meta" head))
      (:div :id "container" :class "container"
            (:section :id "main" :class "main" (dolist (line (split-sequence #\newline body))
                                                 (:p line))))))))

(defun dump-html-file (head body)
  (with-open-file (stream "dump.html" :direction :output)
    (format stream (construct-html head body))))
