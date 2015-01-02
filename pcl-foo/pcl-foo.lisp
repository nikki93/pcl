;;;; pcl-foo.lisp

(in-package #:pcl-foo)

(defvar *html-output* *standard-output*)


;;;
;;; language basics
;;;
;;; strings, numbers, keywords are self-evaluating
;;;
;;; cons forms are either with 'implicit attributes':
;;;     (:tag :attr1 val1 :attr2 val2 ... :attrn valn body)
;;;   or with 'explicit attributes':
;;;     ((:tag :attr1 val1 :attr2 val2 ... :attrn valn) body)
;;;

(defun sexp-html-p (form)
  (or (self-evaluating-p form) (cons-form-p form)))


(defun self-evaluating-p (form)
  (and (atom form) (if (symbolp form) (keywordp form) t)))


(defun cons-form-p (form &optional (test #'keywordp))
  (and (consp form)
       (or (funcall test (car form))
           (and (consp (car form)) (funcall test (caar form))))))

(defun parse-cons-form (form)
  "Return tag, attributes, body."
  (if (consp (first form))
      (parse-explicit-attributes-form form)
      (parse-implicit-attributes-form form)))

(defun parse-explicit-attributes-form (form)
  (destructuring-bind ((tag &rest attributes) &body body) form
    (values tag attributes body)))

(defun parse-implicit-attributes-form (form)
  ;; skip tag, step two at a time collecting attributes until
  ;; first non-keyword or keyword with no following value
  (do ((tag (first form))
       (attributes ())
       (rest (cdr form)))
      ((not (and (keywordp (first rest)) (second rest)))
       (values tag (nreverse attributes) rest))
    (push (first rest) attributes)
    (push (second rest) attributes)
    (setf rest (cddr rest))))



;;;
;;; character escaping
;;;

(defun escape-char (char)
  (case char
    (#\& "&amp;")
    (#\< "&lt;")
    (#\> "&gt;")
    (#\' "&apos;")
    (#\" "&quot;")
    (t (format nil "&#~d;" (char-code char)))))

(defun escape (in escapes)
  (with-output-to-string (out)
    (do ((start 0) pos) (nil)
      (setf pos (position-if #'(lambda (x) (find x escapes))
                             in :start start))
      (write-sequence in out :start start :end pos)
      (cond (pos (write-sequence (escape-char (char in pos)) out)
                 (setf start (1+ pos)))
            (t (return)))))) 

(defparameter *element-escapes* "<>&")
(defparameter *attribute-escapes* "<>&\"'")

(defvar *escapes* *element-escapes*)



;;;
;;; element types
;;;

(defparameter *block-elements*
  '(:body :colgroup :dl :fieldset :form :head :html :map :noscript :object
    :ol :optgroup :pre :script :select :style :table :tbody :tfoot :thead
    :tr :ul))

(defparameter *paragraph-elements*
  '(:area :base :blockquote :br :button :caption :col :dd :div :dt :h1
    :h2 :h3 :h4 :h5 :h6 :hr :input :li :link :meta :option :p :param
    :td :textarea :th :title))

(defparameter *inline-elements*
  '(:a :abbr :acronym :address :b :bdo :big :cite :code :del :dfn :em
    :i :img :ins :kbd :label :legend :q :samp :small :span :strong :sub
    :sup :tt :var))

(defun block-element-p (tag) (find tag *block-elements*))

(defun paragraph-element-p (tag) (find tag *paragraph-elements*))

(defparameter *empty-elements*
  '(:area :base :br :col :hr :img :input :link :meta :param))

(defparameter *preserve-whitespace-elements* '(:pre :script :style))

(defun empty-element-p (tag) (find tag *empty-elements*))

(defun preserve-whitespace-p (tag) (find tag *preserve-whitespace-elements*))



;;;
;;; indenting printer
;;;

(defclass indenting-printer ()
  ((out :accessor out :initarg :out)
   (beginning-of-line-p :accessor beginning-of-line-p :initform t)
   (indentation :accessor indentation :initform 0)
   (indenting-p :accessor indenting-p :initform t)))

(defun emit (ip string)
  (do ((start 0) pos) (nil)
    (setf pos (position #\Newline string :start start))
    (emit/no-newlines ip string :start start :end pos)
    (cond (pos (emit-newline ip) (setf start (1+ pos)))
          (t (return)))))

(defun emit/no-newlines (ip string &key (start 0) end)
  (indent-if-necessary ip)
  (write-sequence string (out ip) :start start :end end)
  (unless (zerop (- (or end (length string)) start))
    (setf (beginning-of-line-p ip) nil)))

(defun indent-if-necessary (ip)
  (when (and (beginning-of-line-p ip) (indenting-p ip))
    (dotimes (i (indentation ip)) (write-char #\Space (out ip)))
    (setf (beginning-of-line-p ip) nil)))

(defun emit-newline (ip)
  (write-char #\Newline (out ip))
  (setf (beginning-of-line-p ip) t))

(defun emit-freshline (ip)
  (unless (beginning-of-line-p ip)
    (emit-newline ip)))



;;;
;;; html processor interface
;;;

(defgeneric raw-string (processor string &optional newlines-p))
(defgeneric newline (processor))
(defgeneric freshline (processor))
(defgeneric indent (processor))
(defgeneric unindent (processor))
(defgeneric toggle-indenting (processor))
(defgeneric embed-value (processor value))
(defgeneric embed-code (processor code))



;;;
;;; pretty-printing backend
;;;

(defparameter *pretty* t)

(defclass html-pretty-printer ()
  ((printer :accessor printer :initarg :printer)
   (tab-width :accessor tab-width :initarg :tab-width :initform 2)))

(defmethod raw-string ((pp html-pretty-printer) string &optional newlines-p)
  (if newlines-p
      (emit (printer pp) string)
      (emit/no-newlines (printer pp) string)))

(defmethod newline ((pp html-pretty-printer))
  (emit-newline (printer pp)))

(defmethod freshline ((pp html-pretty-printer))
  (when *pretty* (emit-freshline (printer pp))))

(defmethod indent ((pp html-pretty-printer))
  (when *pretty* (incf (indentation (printer pp)) (tab-width pp))))

(defmethod unindent ((pp html-pretty-printer))
  (when *pretty* (decf (indentation (printer pp)) (tab-width pp))))

(defmethod toggle-indenting ((pp html-pretty-printer))
  (when *pretty*
    (with-slots (indenting-p) (printer pp)
      (setf indenting-p (not indenting-p)))))

(defmethod embed-value ((pp html-pretty-printer) value)
  (error "Can't embed values when interpreting. Value: ~s" value))

(defmethod embed-code ((pp html-pretty-printer) code)
  (error "Can't embed code when interpreting. Code: ~s" code))



;;;
;;; evaluation
;;;

(defparameter *xhtml* nil)

(defun process (processor form)
  (cond
    ((sexp-html-p form) (process-sexp-html processor form))
    ((consp form) (embed-code processor form))
    (t (embed-value processor form))))

(defun process-sexp-html (processor form)
  (if (self-evaluating-p form)
      (raw-string processor (escape (princ-to-string form) *escapes*) t)
      (process-cons-sexp-html processor form)))

(defun process-cons-sexp-html (processor form)
  (when (string= *escapes* *attribute-escapes*)
    (error "Can't use cons forms in attributes: ~a" form))
  (multiple-value-bind (tag attributes body) (parse-cons-form form)
    (emit-open-tag processor tag body attributes)
    (emit-element-body processor tag body)
    (emit-close-tag processor tag body)))

(defun emit-open-tag (processor tag body-p attributes)
  (when (or (paragraph-element-p tag) (block-element-p tag))
    (freshline processor))
  (raw-string processor (format nil "<~(~a~)" tag))
  (emit-attributes processor attributes)
  (raw-string processor (if (and *xhtml* (not body-p)) "/>" ">")))

(defun emit-attributes (processor attributes)
  (do ((curr attributes (cddr curr))) ((null curr))
    (let ((k (first curr)) (v (second curr)))
      (raw-string processor (format nil " ~(~a~)='" k))
      (let ((*escapes* *attribute-escapes*))
        (process processor (if (eql v t) (string-downcase k) v)))
      (raw-string processor "'"))))

(defun emit-element-body (processor tag body)
  (when (block-element-p tag)
    (freshline processor)
    (indent processor))

  (when (preserve-whitespace-p tag) (toggle-indenting processor))
  (dolist (item body) (process processor item))
  (when (preserve-whitespace-p tag) (toggle-indenting processor))

  (when (block-element-p tag)
    (unindent processor)
    (freshline processor)))

(defun emit-close-tag (processor tag body-p)
  (unless (and (or *xhtml* (empty-element-p tag)) (not body-p))
    (raw-string processor (format nil "</~(~a~)>" tag)))
  (when (or (paragraph-element-p tag) (block-element-p tag))
    (freshline processor)))

(defun emit-html (sexp) (process (get-pretty-printer) sexp))

(defvar *html-pretty-printer* nil)

(defun get-pretty-printer ()
  (or *html-pretty-printer*
      (make-instance 'html-pretty-printer
                     :printer (make-instance 'indenting-printer
                                             :out *html-output*))))



;;;
;;; compiler
;;;

;;; ops buffer

(defun make-op-buffer ()
  (make-array 10 :adjustable t :fill-pointer 0))

(defun push-op (op ops-buffer)
  (vector-push-extend op ops-buffer))


;;; compiler backend

(defclass html-compiler ()
  ((ops :accessor ops :initform (make-op-buffer))))

(defmethod raw-string ((compiler html-compiler) string &optional newlines-p)
  (push-op `(:raw-string ,string ,newlines-p) (ops compiler)))

(defmethod newline ((compiler html-compiler))
  (push-op `(:newline) (ops compiler)))

(defmethod freshline ((compiler html-compiler))
  (push-op `(:freshline) (ops compiler)))

(defmethod indent ((compiler html-compiler))
  (push-op `(:indent) (ops compiler)))

(defmethod unindent ((compiler html-compiler))
  (push-op `(:unindent) (ops compiler)))

(defmethod toggle-indent ((compiler html-compiler))
  (push-op `(:toggle-indent) (ops compiler)))

(defmethod embed-value ((compiler html-compiler) value)
  (push-op `(:embed-value ,value *escapes*) (ops compiler)))

(defmethod embed-code ((compiler html-compiler) code)
  (push-op `(:embed-code ,code) (ops compiler)))

(defun sexp->ops (body)
  (let ((compiler (make-instance 'html-compiler)))
    (dolist (form body) (process compiler form))
    (ops compiler)))


;;; code generation

(defun optimize-static-output (ops)
  (let ((new-ops (make-op-buffer)))
    (with-output-to-string (buf)
      (flet ((add-op (op)
               (compile-buffer buf new-ops)
               (push-op op new-ops)))
        (loop for op across ops do
             (ecase (first op)
               (:raw-string (write-sequence (second op) buf))
               ((:newline :embed-value :embed-code) (add-op op))
               ((:indent :unindent :freshline :toggle-indenting)
                (when *pretty* (add-op op)))))
        (compile-buffer buf new-ops)))
    new-ops))

(defun compile-buffer (buf ops)
  (do ((str (get-output-stream-string buf)) (start 0) (pos))
      (nil)
    (setf pos (position #\Newline str :start start))
    (when (< start (length str))
      (push-op `(:raw-string ,(subseq str start pos) nil) ops))
    (if pos (push-op '(:newline) ops) (return))))

(defun generate-code (ops)
  (loop for op across ops collect (apply #'op->code op)))


(defgeneric op->code (op &rest operands))

(defmethod op->code ((op (eql :raw-string)) &rest operands)
  (destructuring-bind (string check-for-newlines) operands
    (if *pretty*
        `(raw-string *html-pretty-printer* ,string ,check-for-newlines)
        `(write-sequence ,string *html-output*))))

(defmethod op->code ((op (eql :newline)) &rest operands)
  (declare (ignore operands))
  (if *pretty*
      `(newline *html-pretty-printer*)
      `(write-char #\Newline *html-output*)))

(defmethod op->code ((op (eql :freshline)) &rest operands)
  (declare (ignore operands))
  (if *pretty*
      `(freshline *html-pretty-printer*)
      (error "Bad op when not pretty-printing: ~a" op)))

(defmethod op->code ((op (eql :indent)) &rest operands)
  (declare (ignore operands))
  (if *pretty*
      `(indent *html-pretty-printer*)
      (error "Bad op when not pretty-printing: ~a" op)))

(defmethod op->code ((op (eql :unindent)) &rest operands)
  (declare (ignore operands))
  (if *pretty*
      `(unindent *html-pretty-printer*)
      (error "Bad op when not pretty-printing: ~a" op)))

(defmethod op->code ((op (eql :toggle-indenting)) &rest operands)
  (declare (ignore operands))
  (if *pretty*
      `(toggle-indenting *html-pretty-printer*)
      (error "Bad op when not pretty-printing: ~a" op)))

(defmethod op->code ((op (eql :embed-value)) &rest operands)
  (destructuring-bind (value escapes) operands
    (if *pretty*
        (if escapes
            `(raw-string *html-pretty-printer* (escape (princ-to-string ,value)
                                                       ,escapes) t)
            `(raw-string *html-pretty-printer* (princ-to-string ,value) t))
        (if escapes
            `(write-sequence (escape (princ-to-string ,value) ,escapes)
                             *html-output*)
            `(princ ,value *html-output*)))))


(defmethod op->code ((op (eql :embed-code)) &rest operands)
  (first operands))
