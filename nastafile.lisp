(in-package nastafile)

(defvar *client* nil)


(defparameter *email-line-separator*
  (coerce (list #\Return #\Newline) 'string))

(defvar *all-articles* nil)
(defvar *id->article* nil)


(defun parse-datetime (string)
  (local-time:universal-to-timestamp
   (cl-date-time-parser:parse-date-time string)))


(defun format-datetime (dt)
  (check-type dt local-time:timestamp)
  (local-time:format-timestring
   nil dt
   :format '((:YEAR 4) #\- (:MONTH 2) #\- (:DAY 2)
             #\Space
             (:HOUR 2) #\: (:MIN 2)
             " UTC")
   :timezone local-time:+utc-zone+))


(defun connect-to-newsgroup (&key server (port 119) group-name)
  (cl-nntp:connect server port *client*)
  
  (let ((client (first cl-nntp::*clients*)))
    (when group-name
      (cl-nntp:group group-name client))
    (setf *client* client)))


(defun show-article ()
  (dump-html-file (cl-nntp:head *client*)
                  (cl-nntp:body *client*)))


(defun next-article ()
  (cl-nntp:next-article *client*))


(defclass article ()
  ((id :initarg :id
       :reader id)
   (headers :initarg :headers
            :reader headers)
   (email :initform nil
          :reader full-email)))


(defun retrieve-headers (id)
  (cl-pop::parse-raw-message-headers
   (str:split *email-line-separator*
              (cl-nntp:head *client*
                            :article-id id))))


(defun retrieve-raw-email (article)
  (cl-nntp:article *client*
                   :article-id (id article)))


(defmethod full-email :around (article)
  (let* ((pointer (call-next-method))
         (result (when pointer
                   (trivial-garbage:weak-pointer-value pointer))))
    (unless result
      (let ((raw-email (retrieve-raw-email article)))
        (unless raw-email
          (error "Unable to fetch raw email for article ~A"
                 (id article)))

        (setf result
              (trivial-imap::make-email (id article)
                                        raw-email))
        (setf (slot-value article 'email)
              (trivial-garbage:make-weak-pointer result))))
    result))


(defun reset-email (article)
  "Forces email parsing.

   Useful for debugging trivial-imap and cl-mime."
  (setf (slot-value article 'email) nil)
  (full-email article))


(defun subject (article)
  (or (ignore-errors
       (log4cl-extras/error:with-log-unhandled ()
           (trivial-imap:get-subject (full-email article))))
      "[Unable to parse email subject]"))


(defun date (article)
  (parse-datetime (header article "date")))


(defun retrieve-articles (group-name)
  (loop with ids = (cl-nntp:listgroup group-name *client*)
        for idx upfrom 0
        for id in ids
        for headers = (retrieve-headers id)
        for article = (make-instance 'article
                                     :id id
                                     :headers headers)
        do (push article *all-articles*)
        when (zerop (mod idx 50))
          do (format t "PROGRESS: ~A%~%" (ceiling (* (/ idx (length ids))
                                                     100))))
  (setf *id->article*
        (build-id-to-article-map *all-articles*))
  (values))


(defun article-by-id (id)
  (when *id->article*
    (gethash id *id->article*)))


(defun header (article name)
  (values
   (alexandria:assoc-value  (headers article) name
                            :test #'string-equal)))

(defun email-id (article)
  (header article "message-id"))

(defun parent-email-id (article)
  (header article "References"))

(defun content-type (article)
  (anaphora:awhen (header article "content-type")
    (string-downcase
     (first (str:split #\;
                       anaphora:it)))))

(defun content-encoding (article)
  (anaphora:awhen (header article "Content-Transfer-Encoding")
    (string-downcase anaphora:it)))

(defun content-type-frequency (articles)
  "Returns a hash useful for debugging to understand
   which content-types are present."
  (loop with result = (make-hash-table :test 'equal)
        for article in articles
        for type = (content-type article)
        do (incf (gethash type result 0))
        finally (return result)))

(defun content-encoding-frequency (articles)
  "Returns a hash useful for debugging to understand
   which content-types are present."
  (loop with result = (make-hash-table :test 'equal)
        for article in articles
        for encoding = (content-encoding article)
        do (incf (gethash encoding result 0))
        finally (return result)))


(defun filter-by-content-type (articles content-type)
  (remove-if-not
   (lambda (a)
     (string-equal (content-type a)
                   content-type))
   articles))


(defun body (article)
  (str:replace-all
   *email-line-separator*
   "
"
   (cl-nntp:body *client*
                 :article-id (id article))))


(defun render-plain-text (content)
  (spinneret:with-html
    (:pre content)))


(defun render (article)
  "
NIL = 2037 [remove entry]
multipart/alternative = 2207 [remove entry]
multipart/mixed = 142 [remove entry]
multipart/related = 23 [remove entry]
multipart/signed = 122 [remove entry]
text/html = 51 [remove entry]
text/plain = 10804 [remove entry]

"
  (handler-case
      (log4cl-extras/error:with-log-unhandled ()
        (let* ((email (full-email article))
               (subject (ignore-errors
                         (trivial-imap:get-subject email)))
               (html (ignore-errors
                      (trivial-imap:get-html email)))
               (text (ignore-errors
                      (trivial-imap:get-text email))))
          (spinneret:with-html
            (:article :class "email"
                      (:h1 subject)
                      (cond
                        (html (:raw html))
                        (text (:pre text))
                        (t (:p ("Unable to parse email body. Email id is ~A"
                                (id article)))))))))
    (error (condition)
      (spinneret:with-html
        (:article :class "email"
                  (:h1 ("Unable to render article ~A because of ~S error"
                        (id article)
                        condition)))))))


(defun build-email-id-to-id-map (articles)
  (loop with hash = (make-hash-table :test 'equal)
        for article in articles
        do (setf (gethash (email-id article) hash)
                 (id article))
        finally (return hash)))


(defun build-id-to-article-map (articles)
  (loop with hash = (make-hash-table :test 'equal)
        for article in articles
        do (setf (gethash (id article) hash)
                 article)
        finally (return hash)))


(defun build-tree (articles)
  "
Returns a hash from id to a list of child message ids.

As the second value it returns ids of root articles.
"
  (loop with tree = (make-hash-table :test 'equal)
        with roots = nil
        with email->id = (build-email-id-to-id-map articles)
        for article in articles
        for parent-email-id = (parent-email-id article)
        do (if parent-email-id
               (push (id article)
                     (gethash (gethash parent-email-id email->id)
                              tree))
               (push (id article)
                     roots))
        finally (return (values tree
                                roots))))


(defmacro page-template ((title) &body body)
  `(spinneret:with-html
     (:doctype)
     (:html
      (:head
       (:title ,title)
       ;; To make HTML looks nice on mobile devices
       (:meta :name "viewport"
              :content "width=device-width, initial-scale=1.0")
       ;; bootstrap cdn for css alone
       (:link :rel "stylesheet"
              :href "https://cdn.jsdelivr.net/npm/bootstrap@4.5.3/dist/css/bootstrap.min.css"
              :integrity "sha384-TX8t27EcRE3e/ihU7zmQxVncDAy5uIKz4rEkgIXeMed4M0jlfIDPvg6uqKI2xXr2"
              :crossorigin "anonymous")
       (:style "
section.tree {
    padding-left: 2em;
}
section.tree:first-child {
    padding-left: 0;
}
.article-link {
  margin-bottom: 1em;
}
")
       (:raw "
<!-- Google Analytics -->
<script async src=\"https://www.googletagmanager.com/gtag/js?id=G-9X3G9MMWZP\"></script>
<script>
  window.dataLayer = window.dataLayer || [];
  function gtag(){dataLayer.push(arguments);}
  gtag('js', new Date());

  gtag('config', 'G-9X3G9MMWZP');
</script>
"))
      (:body
       (:header :class "d-flex justify-content-center"
             (:nav :class "navbar navbar-light bg-light w-100 mx-5 mb-3"
                   (:a :class "navbar-brand"
                       :href "/"
                       "Lisp HUG Maillist Archive")))
       (:div :class "d-flex justify-content-center"
             (:div :class "w-100 mx-5 px-3"
                   ,@body))
       (:footer :class "d-flex justify-content-center"
                (:div (format nil "Updated at: ~A"
                              (format-datetime
                               (local-time:now)))))))))


(defun render-tree (article children-getter)
  (page-template ((subject article))
    (labels ((render-child (article)
               (:section :class "tree"
                         (render article)
                         (mapc #'render-child
                               (funcall children-getter
                                        article)))))
      (render-child article))))


(defun dump-tree (year article children-getter)
  (let ((filename (format nil "site/~A/~A.html"
                          year
                          (id article))))
    (ensure-directories-exist filename)
    (with-open-file (spinneret:*html*
                     filename
                     :direction :output
                     :if-exists :supersede)
      (render-tree article children-getter))))


(defun dump-year-trees (year articles children-getter)
  "
Returns a hash from id to a list of child message ids.

As the second value it returns ids of root articles.
"
  (let ((filename (format nil "site/~A/index.html" year))
        (title (format nil "~A year" year)))
    (ensure-directories-exist filename)
    (with-open-file (spinneret:*html*
                     filename
                     :direction :output
                     :if-exists :supersede)

      (page-template (title)
        (spinneret:with-html
          (:h1 title)
          
          (loop for (month . month-articles) in (group-by-month articles)
                do (:h2 month)
                   (:dl
                    (loop for article in month-articles
                          do (dump-tree year article children-getter)
                             (:dt :class "date"
                                  (format-datetime
                                   (date article)))
                             (:dd :class "article-link"
                                  (:a :href (format nil "~A.html"
                                                    (id article))
                                      (subject article)))))))))))


(defun group-by-years (articles)
  (group-by:group-by articles
                     :key
                     (lambda (a)
                       (local-time:timestamp-year
                        (date a)))
                     :value #'identity ))


(defun group-by-month (articles)
  (group-by:group-by articles
                     :key
                     (lambda (a)
                       (case (local-time:timestamp-month
                              (date a))
                         (1 "January")
                         (2 "February")
                         (3 "March")
                         (4 "April")
                         (5 "May")
                         (6 "June")
                         (7 "July")
                         (8 "August")
                         (9 "September")
                         (10 "October")
                         (11 "November")
                         (12 "December")))
                     :value #'identity ))


(defun dump-trees (articles &key (limit nil))
  "
Returns a hash from id to a list of child message ids.

As the second value it returns ids of root articles.
"
  (format t "Dumping ~A articles to the disk~%"
          (length articles))
  
  (handler-bind ((SB-INT:STREAM-DECODING-ERROR
                   (lambda (c)
                     (declare (ignorable c))
                     (invoke-restart 'sb-int:attempt-resync))))
    (multiple-value-bind (tree roots)
        (build-tree articles)
      (setf roots
            (sort (copy-list roots)
                  #'>))
      (when limit
        (setf roots
              (rutils:take limit roots)))
      
      (let ((id->article (build-id-to-article-map articles)))

        (labels ((get-article (id)
                   (gethash id id->article))
                 (get-children (article)
                   (mapcar #'get-article
                           (gethash (id article)
                                    tree))))
          (let ((filename "site/index.html"))
            (ensure-directories-exist filename)
            (with-open-file (spinneret:*html*
                             filename
                             :direction :output
                             :if-exists :supersede)

              (page-template ("Lisp Hug Archive")
                (spinneret:with-html
                  (:dl
                   (loop with root-articles = (mapcar #'get-article roots)
                         with groups = (group-by-years root-articles)
                         for (year . roots) in groups
                         do (format t "Processing ~A year~%" year)
                            (report-space)
                            (dump-year-trees year roots #'get-children)
                            (:dt :class "date"
                                 ("~A" year))
                            (:dd :class "year-link"
                                 (:a :href (format nil "~A/index.html" year)
                                     ("~A threads" (length roots)))))))))))))))


(defun dynamic-space-size ()
  "Returns in bytes the size of the dynamic space"
  ;; #-sbcl
  ;; (error "This function works only on SBCL!")
  ;; #+sbcl

  ;; For some reason this code stopped workking and just hangs now
  (parse-integer
   (str:replace-all
    "," ""
    (nth 6
         (str:split
          #\Space
          (first
           (str:split #\Newline
                      (with-output-to-string (*standard-output*)
                        (room nil)))))))))

(defun report-space ()
  (room nil))


(defun add-nojekyll-file ()
  "We need this file to prevent GitHub from trying to use Jekyll static generator."
  (alexandria:with-output-to-file (s "site/.nojekyll" :if-exists :supersede)))


(defun process (&key (server "news.gmane.io")
                  (group "gmane.lisp.lispworks.general"))
  (connect-to-newsgroup :server server :group-name group)
  (retrieve-articles group)
  (dump-trees *all-articles*)
  (add-nojekyll-file))

