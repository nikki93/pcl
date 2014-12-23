;;;; pcl-binary.lisp

(in-package #:pcl-binary)



;;;
;;; macro utils
;;;

(defmacro with-gensyms (syms &body body)
  `(let ,(loop for sym in syms collect `(,sym (gensym)))
     ,@body))

(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop for n in names collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
      `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
        ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
           ,@body)))))



;;;
;;; generic read/write interface
;;;

(defgeneric read-object (object stream)
  (:method-combination progn :most-specific-last)
  (:documentation "Read slots of object from stream."))

(defgeneric read-value (type stream &key)
  (:documentation "Read a value of the given type from the stream."))

(defmethod read-value ((type symbol) stream &key)
  (let ((object (make-instance type)))
    (read-object object stream)
    object))


(defgeneric write-object (object stream)
  (:method-combination progn :most-specific-last)
  (:documentation "Write slots of object to stream."))

(defgeneric write-value (type stream value &key)
  (:documentation "Write a value of the given type to the stream."))

(defmethod write-value ((type symbol) stream value &key)
  (assert (typep value type))
  (write-object value stream))



;;;
;;; define-binary-type
;;;

(defmacro define-binary-type (name (&rest args) &body spec)
  (ecase (length spec)
    (1
     (with-gensyms (type stream val)
       (let* ((spec (first spec))
              (lspec (if (listp spec) spec (list spec))))
        (destructuring-bind (parent &rest parent-args) lspec
          `(progn
             (defmethod read-value ((,type (eql ',name)) ,stream &key ,@args)
               (read-value ',parent ,stream ,@parent-args))
             (defmethod write-value ((,type (eql ',name)) ,stream ,val &key ,@args)
               (read-value ',parent ,stream ,val ,@parent-args)))))))
    (2
     (with-gensyms (type)
       `(progn
          ,(destructuring-bind
            ((in) &body body) (rest (assoc :reader spec))
            `(defmethod read-value ((,type (eql ',name)) ,in &key ,@args)
               ,@body))
          ,(destructuring-bind
            ((out val) &body body) (rest (assoc :writer spec))
            `(defmethod write-value ((,type (eql ',name)) ,out ,val &key ,@args)
               ,@body)))))))



;;;
;;; define-binary-class, define-tagged-binary-class
;;;

;;; macro helpers

(defun as-keyword (sym) (intern (string sym) :keyword))

(defun slot->defclass-slot (spec)
  (let ((name (first spec)))
    `(,name :initarg ,(as-keyword name) :accessor ,name)))

(defun slot->read-value (spec stream)
  (let* ((name (first spec)) 
         (second (second spec))
         (type (if (listp second) (first second) second))
         (args (if (listp second) (rest second))))
    `(setf ,name (read-value ',type ,stream ,@args))))

(defun slot->write-value (spec stream)
  (let* ((name (first spec))
         (second (second spec))
         (type (if (listp second) (first second) second))
         (args (if (listp second) (rest second))))
    `(write-value ',type ,stream ,name ,@args)))

(defun slot->binding (spec stream)
  (let* ((name (first spec)) 
         (second (second spec))
         (type (if (listp second) (first second) second))
         (args (if (listp second) (rest second))))
    `(,name (read-value ',type ,stream ,@args))))

(defun slot->initarg (spec)
  (let ((name (first spec)))
    `(,(as-keyword name) ,name)))


;;; slot metadata

(defun direct-slots (class)
  (copy-list (get class 'slots)))

(defun inherited-slots (class)
  (loop for super in (get class 'superclass)
     nconc (direct-slots super)
     nconc (inherited-slots super)))

(defun all-slots (class)
  (nconc (direct-slots class) (inherited-slots class)))

(defun new-class-all-slots (slots superclasses)
  (nconc (mapcan #'all-slots superclasses) (mapcar #'first slots)))


;;; main macros

(defmacro define-generic-binary-class (class (&rest superclasses) slots
                                       &body read-method)
  (with-gensyms (instance-var stream-var)
    `(progn
       ;; update metadata
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (get ',class 'slots) ',(mapcar #'first slots))
         (setf (get ',class 'superclasses) ',superclasses))

       ;; the class itself
       (defclass ,class ,superclasses
         ,(mapcar #'slot->defclass-slot slots))

       ;; read method
       ,@read-method

       ;; write method
       (defmethod write-object progn ((,instance-var ,class) ,stream-var)
         (with-slots ,(new-class-all-slots slots superclasses) ,instance-var
           ,@(mapcar #'(lambda (slot) (slot->write-value slot stream-var))
                     slots))))))

(defmacro define-binary-class (class (&rest superclasses) slots)
  (with-gensyms (instance-var stream-var)
    `(define-generic-binary-class ,class ,superclasses ,slots
       (defmethod read-object progn ((,instance-var ,class) ,stream-var)
         (with-slots ,(new-class-all-slots slots superclasses) ,instance-var
           ,@(mapcar #'(lambda (slot) (slot->read-value slot stream-var))
                     slots))))))

(defmacro define-tagged-binary-class (class (&rest superclasses)
                                               slots &rest options)
  (with-gensyms (instance-var stream-var type-var)
    `(define-generic-binary-class ,class ,superclasses ,slots
       (defmethod read-value ((,type-var (eql ',class)) ,stream-var &key)
         (let* ,(mapcar #'(lambda (slot) (slot->binding slot stream-var))
                        slots)
           (let ((,instance-var
                  (make-instance ,@(or (cdr (assoc :dispatch options))
                                       (error "Must supply :displatch form."))
                                 ,@(mapcan #'slot->initarg slots))))
             (read-object ,instance-var ,stream-var)
             ,instance-var))))))



;;;
;;; current object stack
;;;

(defvar *in-progress-objects* nil)

(defmethod read-object :around (object stream)
  (declare (ignore stream))
  (let ((*in-progress-objects* (cons object *in-progress-objects*)))
    (call-next-method)))

(defmethod write-object :around (object stream)
  (declare (ignore stream))
  (let ((*in-progress-objects* (cons object *in-progress-objects*)))
    (call-next-method)))

(defun current-binary-object () (first *in-progress-objects*))

(defun parent-of-type (type)
  (find-if #'(lambda (x) (typep x type)) *in-progress-objects*))


;;;
;;; test
;;;

(define-binary-class id3-tag ()
  ((file-identifier (iso-8859-1-string :length 3))
   (major-version u1)
   (revision u1)
   (flags u1)
   (size id3-tag-size)
   (frames (id3-frames :tag-size size))))

(define-binary-type iso-8859-1-string (length)
  (:reader (in)
    (let ((string (make-string length)))
      (dotimes (i length)
        (setf (char string i) (code-char (read-byte in))))
      string))
  (:writer (out string)
    (dotimes (i length)
      (write-byte (char-code (char string i)) out))))

(define-binary-type u1 () (unsigned-integer :bytes 1))


