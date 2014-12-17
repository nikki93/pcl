;;;; pcl-pathnames.lisp

(in-package #:pcl-pathnames)

(defun component-present-p (value)
  (and value (not (eql value :unpsecific))))

(defun directory-pathname-p (p)
  (and
   (not (component-present-p (pathname-name p)))
   (not (component-present-p (pathname-type p)))
   p))

(defun pathname-as-directory (name)
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "can't convert wild pathnames"))
    (if (not (directory-pathname-p name))
        (make-pathname
         :directory (append (or (pathname-directory pathname) '(:relative))
                            `(,(file-namestring pathname)))
         :name nil
         :type nil
         :defaults pathname)
        pathname)))

(defun list-directory (dirname)
  (when (wild-pathname-p dirname)
    (error "can't list wild directories"))
  (let ((wildcard (make-pathname :name :wild :type :wild
                                 :defaults (pathname-as-directory dirname))))
    (directory wildcard)))

(defun file-exists-p (pathname)
  (probe-file pathname))

(defun walk-directory (dirname fn &key directories (test (constantly t)))
  (labels
      ((walk (name)
         (cond
           ((directory-pathname-p name)
            (if (and directories (funcall test name))
                (funcall fn name))
            (dolist (x (list-directory name)) (walk x)))
           ((funcall test name) (funcall fn name)))))
    (walk (pathname-as-directory dirname))))

