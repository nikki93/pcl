;;;; spam.lisp

(in-package #:spam)

(defun classify (text)
  (classification (score (extract-features text))))


;;; classification

(defparameter *max-ham-score* .4)
(defparameter *min-spam-score* .6)

(defun classification (score)
  (values
   (cond
     ((<= score *max-ham-score*) 'ham)
     ((>= score *min-spam-score*) 'spam)
     (t 'unsure))
   score))


;;; features

(defclass word-feature ()
  ((word
    :documentation "the word this feature represents"
    :initarg :word
    :accessor word
    :initform (error "must supply :word"))
   (spam-count
    :documentation "the number of times this word appears in spam"
    :initarg :spam-count
    :accessor spam-count
    :initform 0)
   (ham-count
    :documentation "the number of times this word appears in ham"
    :initarg :ham-count
    :accessor ham-count
    :initform 0)))

(defmethod print-object ((object word-feature) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (word spam-count ham-count) object
      (format stream "~a :ham-count ~a :spam-count ~a" word spam-count ham-count))))

(defparameter *feature-database* (make-hash-table :test #'equal))

(defun clear-database ()
  (setf
   *feature-database* (make-hash-table :test #'equal)
   *total-spams* 0
   *total-hams* 0))

(defun intern-feature (word)
  (or (gethash word *feature-database*)
      (setf (gethash word *feature-database*)
            (make-instance 'word-feature :word word))))

(defun extract-words (text)
  (delete-duplicates
   (cl-ppcre:all-matches-as-strings "[a-zA-z]{3,}" text)
   :test #'string=))

(defun extract-features (text)
  (mapcar #'intern-feature (extract-words text)))


;;; training

(defun train (text type)
  (dolist (feature (extract-features text))
    (increment-count feature type))
  (increment-total-count type))

(defun untrained-p (feature)
  (and (zerop (spam-count feature)) (zerop (ham-count feature))))

(defun increment-count (feature type)
  (ecase type
    (spam (incf (spam-count feature)))
    (ham (incf (ham-count feature)))))

(defvar *total-spams* 0)
(defvar *total-hams* 0)

(defun increment-total-count (type)
  (ecase type
    (spam (incf *total-spams*))
    (ham (incf *total-hams*))))


;;; scoring

(defun spam-probability (feature)
  (with-slots (spam-count ham-count) feature
    (let ((spam-freq (/ spam-count (max 1 *total-spams*)))
          (ham-freq (/ ham-count (max 1 *total-hams*))))
      (/ spam-freq (+ spam-freq ham-freq)))))

(defun bayesian-spam-probability (feature &optional
                                            (assumed-probability 1/2)
                                            (weight 1))
  (let ((basic-probability (spam-probability feature))
        (data-points (+ (spam-count feature) (ham-count feature))))
    (/ (+ (* weight assumed-probability) (* data-points basic-probability))
       (+ weight data-points))))

(defun score (features)
  (let ((spam-probs ()) (ham-probs ()) (number-of-probs 0))
    (dolist (feature features)
      (unless (untrained-p feature)
        (let ((spam-prob (float (bayesian-spam-probability feature) 0.0d0)))
          (push spam-prob spam-probs)
          (push (- 1.0d0 spam-prob) ham-probs)
          (incf number-of-probs))))
    (let ((h (- 1 (fisher spam-probs number-of-probs)))
          (s (- 1 (fisher ham-probs number-of-probs))))
      (/ (+ (- 1 h) s) 2.0d0))))

(defun fisher (probs number-of-probs)
  (inverse-chi-square
   (* -2 (reduce #'+ probs :key #'log))
   (* 2 number-of-probs)))

(defun inverse-chi-square (value degrees-of-freedom)
  (assert (evenp degrees-of-freedom))
  (min
   (loop with m = (/ value 2)
      for i below (/ degrees-of-freedom 2)
      for prob = (exp (- m)) then (* prob (/ m i))
      summing prob)
   1.0))


;;; testing

(defun add-file-to-corpus (filename type corpus)
  (vector-push-extend (list filename type) corpus))

(defun add-directory-to-corpus (dir type corpus)
  (dolist (file (pcl-pathnames:list-directory dir))
    (add-file-to-corpus file type corpus)))

(defun test-classifier (corpus testing-fraction)
  (clear-database)
  (let* ((shuffled (shuffle-vector corpus))
         (size (length corpus))
         (train-on (floor (* size (- 1 testing-fraction)))))
    (train-from-corpus shuffled :start 0 :end train-on)
    (test-from-corpus shuffled :start train-on)))

(defparameter *max-chars* (* 10 1024))

(defun train-from-corpus (corpus &key (start 0) end)
  (loop for idx from start below (or end (length corpus)) do
       (destructuring-bind (file type) (aref corpus idx)
         (train (start-of-file file *max-chars*) type))))

(defun test-from-corpus (corpus &key (start 0) end)
  (loop for idx from start below (or end (length corpus)) collect
       (destructuring-bind (file type) (aref corpus idx)
         (multiple-value-bind (classification score)
             (classify (start-of-file file *max-chars*))
           (list :file file :type type
                 :classification classification :score score)))))

(defun init-corpus ()
  (defparameter *corpus* (make-array 1000 :adjustable t :fill-pointer 0))
  (add-directory-to-corpus "data/easy_ham/" 'ham *corpus*)
  (add-directory-to-corpus "data/hard_ham/" 'ham *corpus*)
  (add-directory-to-corpus "data/spam/" 'spam *corpus*))


;;; utils

(defun nshuffle-vector (vector)
  (loop
     for i from (1- (length vector)) downto 1
     for j = (random (1+ i))
     do (unless (= i j)
          (rotatef (aref vector i) (aref vector j))))
  vector)

(defun shuffle-vector (vector)
  (nshuffle-vector (copy-seq vector)))

(defun start-of-file (file max-chars)
  (with-open-file (in file :external-format :iso-8859-1)
    (let* ((length (min (file-length in) max-chars))
           (text (make-string length))
           (read (read-sequence text in)))
      (if (< read length)
          (subseq text 0 read)
          text))))


;;; analysis

(defun result-type (result)
  (destructuring-bind (&key type classification &allow-other-keys) result
    (cond ((eq type classification) 'correct)
          ((and (eql type 'ham) (eql classification 'spam)) 'false-positive)
          ((and (eql type 'spam) (eql classification 'ham)) 'false-negative)
          ((and (eql type 'ham) (eql classification 'unsure)) 'missed-ham)
          ((and (eql type 'spam) (eql classification 'unsure)) 'missed-spam)
          (t (error "bad result")))))

(defun result-type-p (type)
  (lambda (result) (eql (result-type result) type)))

(defun analyze-results (results)
  (let ((counts '((total . 0))))

    ;; collect result-type --> count map, along with total
    (dolist (result results)
      (incf (cdr (assoc 'total counts)))
      (let ((type (result-type result)))
        (if (assoc type counts)
            (incf (cdr (assoc type counts)))
            (push (cons type 1) counts))))

    ;; print map with percentages
    (loop with total = (cdr (assoc 'total counts))
       for (label . count) in counts
       do (format t "~&~@(~a~):~20t~5d~,5t: ~6,2f%~%"
                  label count (* 100 (/ count total))))))
