;;;; spam.asd

(asdf:defsystem #:spam
  :description "Exercise from chapter 23 of Practical Common Lisp by Peter Seibel"
  :author "Nikhilesh Sigatapu <s.nikhilesh@gmail.com>"
  :depends-on (#:pcl-pathnames
               #:cl-ppcre)
  :serial t
  :components ((:file "package")
               (:file "spam")))

