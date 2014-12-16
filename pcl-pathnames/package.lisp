;;;; package.lisp

(defpackage #:pcl-pathnames
  (:use #:cl)
  (:export :list-directory
           :file-exists-p
           :directory-pathname-p
           :file-pathname-p
           :pathname-as-directory
           :pathname-as-file
           :walk-directory
           :directory-p
           :file-p))

