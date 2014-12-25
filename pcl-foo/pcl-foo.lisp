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
;;; evaluation
;;;

(defparameter *xhtml* nil)

(defun process (processor form)
  (if (sexp-html-p form)
      (process-sexp-html processor form)
      (error "Malformed FOO form: ~s" form)))

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
