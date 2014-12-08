(defmacro with-gensyms ((&rest syms) &body body)
  `(let ,(loop for sym in syms collecting `(,sym (gensym)))
     ,@body))


(defvar *test-name* nil)

(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
  result)

(defmacro combine-results (&body forms)
  (with-gensyms (result)
    `(let ((,result t))
       ,@(loop for form in forms collecting `(unless ,form (setf ,result nil)))
       ,result)))

(defmacro check (&rest forms)
  `(combine-results
     ,@(loop for form in forms collect `(report-result ,form ',form))))

(defmacro deftest (name parameters &body body)
  `(defun ,name ,parameters
     (let ((*test-name* (append *test-name* '(,name))))
       ,@body)))



(deftest test-+ ()
  (check
   (= (+ 1 2) 3)
   (= (+ 1 2 3) 6)
   (= (+ -1 -3) -4)))

(deftest test-* ()
  (check
   (= (* 1 2) 2)
   (= (* 1 2 3) 6)
   (= (* -1 -3) 3)))

(deftest test-arithmetic ()
  (combine-results
    (test-+)
    (test-*)))
