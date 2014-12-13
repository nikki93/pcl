(defclass bank-account ()
  ((customer-name
    :initarg :customer-name)
   (balance
    :initarg :balance
    :initform 0)))

(defgeneric withdraw (account amount)
  (:documentation "withdraw the specified amount,
error if insufficient funds"))

(defmethod withdraw ((account bank-account) amount)
  (when (< (balance account) amount)
    (error "account overdrawn"))
  (decf (balance account) amount))

(defmethod withdraw :before ((account checking-account) amount)
  (let ((overdraft (- amount (balance account))))
    (when (plusp overdraft)
      (withdraw (overdraft-account account) overdraft)
      (incf (balance account) overdraft))))
