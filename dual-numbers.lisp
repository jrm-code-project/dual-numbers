;;; -*- Lisp -*-

(in-package "DUAL-NUMBERS")

(defstruct (dual-number
            (:constructor %make-dual (standard-part infinitesimal-part)))
  standard-part
  infinitesimal-part)

(declaim (inline %make-dual-float make-dual-float))

(defstruct (dual-float
            (:include dual-number
                      (standard-part 0.0 :type single-float)
                      (infinitesimal-part 0.0 :type single-float))
            (:constructor %make-dual-float (standard-part infinitesimal-part))))

(defgeneric infinitesimal-part (obj)
  (:documentation "Return the infinitesimal-part of a dual number or number.")
  (:method ((obj number)) 0))

(defgeneric standard-part (obj)
  (:documentation "Return the standard part of a dual number or number.")
  (:method ((obj number)) obj))

(defmethod standard-part ((obj dual-number))
  (dual-number-standard-part obj))

(defmethod infinitesimal-part ((obj dual-number))
  (dual-number-infinitesimal-part obj))

(defmethod standard-part ((obj dual-float))
  (declare (type dual-float obj))
  (dual-float-standard-part obj))

(defmethod infinitesimal-part ((obj dual-float))
  (declare (type dual-float obj))
  (dual-float-infinitesimal-part obj))

(defun make-dual-float (standard infinitesimal)
  "Create a dual-float with single-float standard and infinitesimal parts."
  (declare (type single-float standard infinitesimal))
  (if (cl:zerop infinitesimal)
      standard
      (%make-dual-float standard infinitesimal)))

(defmethod print-object ((obj dual-number) stream)
  (let* ((denom (cl:lcm (cl:denominator (standard-part obj))
                        (cl:denominator (infinitesimal-part obj))))
         (std (cl:* (cl:numerator (standard-part obj)) (/ denom (cl:denominator (standard-part obj)))))
         (inf (cl:* (cl:numerator (infinitesimal-part obj)) (/ denom (cl:denominator (infinitesimal-part obj))))))
    (if (= denom 1)
        (format stream "#<DUAL-NUMBER ~A ~:[+~;-~] ~Aε>" std (minusp inf) (abs inf))
        (format stream "#<DUAL-NUMBER (~A ~:[+~;-~] ~Aε)/~A>" std (minusp inf) (abs inf) denom))))

(defun make-dual (standard infinitesimal)
  "Create a dual number with STANDARD and INFINITESIMAL parts."
  (if (zerop infinitesimal)
      standard
      (%make-dual standard infinitesimal)))

(defmethod =2 ((left dual-number) (right dual-number))
  (and (= (standard-part left) (standard-part right))
       (= (infinitesimal-part left) (infinitesimal-part right))))

(defmethod =2 ((left number) (right dual-number))
  (and (= left (standard-part right))
       (zerop (infinitesimal-part right))))

(defmethod =2 ((left dual-number) (right number))
  (and (= (standard-part left) right)
       (zerop (infinitesimal-part left))))

(defmethod add2 ((left dual-number) (right dual-number))
  (make-dual (cl:+ (standard-part left) (standard-part right))
             (cl:+ (infinitesimal-part left) (infinitesimal-part right))))

(defmethod add2 ((left number) (right dual-number))
  (make-dual (cl:+ left (standard-part right))
             (infinitesimal-part right)))

(defmethod add2 ((left dual-number) (right number))
  (make-dual (cl:+ (standard-part left) right)
             (infinitesimal-part left)))

(defmethod conjugate ((obj dual-number))
  (make-dual (standard-part obj)
             (cl:- (infinitesimal-part obj))))

(defmethod cos ((obj dual-number))
  (make-dual (cos (standard-part obj))
             (- (* (infinitesimal-part obj) (sin (standard-part obj))))))

(defmethod cosh ((obj dual-number))
  (make-dual (cosh (standard-part obj))
             (* (infinitesimal-part obj) (sinh (standard-part obj)))))

(defmethod denominator ((obj dual-number))
  (cl:lcm (cl:denominator (standard-part obj))
          (cl:denominator (infinitesimal-part obj))))

(defun derivative (function)
  "Return a function that computes the derivative of FUNCTION at a point."
  (lambda (var)
    (infinitesimal-part (funcall function (make-dual var 1)))))

(defmethod divide2 (left (right dual-number))
  (let ((conjugate (conjugate right)))
    (/ (* left conjugate)
       (* right conjugate))))

(defmethod divide2 ((left dual-number) (right number))
  (make-dual (cl:/ (standard-part left) right)
             (cl:/ (infinitesimal-part left) right)))

(defmethod exp ((obj dual-number))
  (let ((e (exp (standard-part obj))))
    (make-dual e (* e (infinitesimal-part obj)))))

(defmethod expt ((base dual-number) (power number))
  (let ((p (expt (standard-part base) (1- power))))
    (make-dual (* p (standard-part base))
               (* (infinitesimal-part base) power p))))

(defmethod expt ((base number) (power dual-number))
  (let ((p (expt base (standard-part power))))
    (make-dual p
               (* (infinitesimal-part power) p (log base)))))

(defmethod expt ((base dual-number) (power dual-number))
  (let* ((a (standard-part base))
         (b (infinitesimal-part base))
         (c (standard-part power))
         (d (infinitesimal-part power))
         (p (expt a c)))
    (make-dual p
               (* p (+ (* d (log a))
                       (/ (* b c) a))))))

(defmethod log ((obj dual-number) &optional (base (exp 1)))
  (if (equal base (exp 1))
      (make-dual (log (standard-part obj))
                 (/ (infinitesimal-part obj) (standard-part obj)))
      (make-dual (log (standard-part obj) base)
                 (/ (infinitesimal-part obj)
                    (* (standard-part obj) (log base))))))

(defmethod multiply2 ((left dual-number) (right dual-number))
  (make-dual (cl:* (standard-part left) (standard-part right))
             (cl:+ (cl:* (standard-part left) (infinitesimal-part right))
                   (cl:* (infinitesimal-part left) (standard-part right)))))

(defmethod multiply2 ((left number) (right dual-number))
  (make-dual (cl:* left (standard-part right))
             (cl:* left (infinitesimal-part right))))

(defmethod multiply2 ((left dual-number) (right number))
  (make-dual (cl:* (standard-part left) right)
             (cl:* (infinitesimal-part left) right)))

(defmethod negate ((obj dual-number))
  (make-dual (cl:- (standard-part obj))
             (cl:- (infinitesimal-part obj))))

(defmethod numerator ((obj dual-number))
  (let* ((denom (cl:lcm (cl:denominator (standard-part obj))
                        (cl:denominator (infinitesimal-part obj))))
         (std (cl:* (cl:numerator (standard-part obj)) (/ denom (cl:denominator (standard-part obj)))))
         (inf (cl:* (cl:numerator (infinitesimal-part obj)) (/ denom (cl:denominator (infinitesimal-part obj))))))
    (make-dual std inf)))

(defmethod reciprocal ((obj dual-number))
  (let ((conjugate (conjugate obj)))
    (divide2 conjugate
             (multiply2 obj conjugate))))

(defmethod sin ((obj dual-number))
  (make-dual (sin (standard-part obj))
             (* (infinitesimal-part obj) (cos (standard-part obj)))))

(defmethod sinh ((obj dual-number))
  (make-dual (sinh (standard-part obj))
             (* (infinitesimal-part obj) (cosh (standard-part obj)))))

(defmethod sqrt ((obj dual-number))
  (let ((s (sqrt (standard-part obj))))
    (make-dual s (/ (infinitesimal-part obj) (* 2 s)))))

(defmethod subtract2 ((left dual-number) (right dual-number))
  (make-dual (cl:- (standard-part left) (standard-part right))
             (cl:- (infinitesimal-part left) (infinitesimal-part right))))

(defmethod subtract2 ((left number) (right dual-number))
  (make-dual (cl:- left (standard-part right))
             (cl:- (infinitesimal-part right))))

(defmethod subtract2 ((left dual-number) (right number))
  (make-dual (cl:- (standard-part left) right)
             (infinitesimal-part left)))

(defmethod tan ((obj dual-number))
  (let ((c (cos (standard-part obj))))
    (make-dual (tan (standard-part obj))
               (/ (infinitesimal-part obj) (* c c)))))

(defmethod tanh ((obj dual-number))
  (let ((c (cosh (standard-part obj))))
    (make-dual (tanh (standard-part obj))
               (/ (infinitesimal-part obj) (* c c)))))

(defmethod zerop ((obj dual-number))
  (and (zerop (standard-part obj))
       (zerop (infinitesimal-part obj))))

(defun newton-step (function current-guess)
  "Perform a single iteration of Newton's method using dual numbers."
  (let* ((dual-result (funcall function (make-dual current-guess 1)))
         ;; If your make-dual returns a raw number for zero infinitesimal, 
         ;; we need to handle that or ensure the result is treated as a dual.
         (y (standard-part dual-result))
         (slope (infinitesimal-part dual-result)))
    (if (zerop slope)
        current-guess ; Or handle the 'flat-spot' catastrophe
        (- current-guess (/ y slope)))))

(defun solve-newton (function initial-guess &key (iterations 10) (tolerance 1e-10))
  "Recursively find the root using the dual-number Newton step."
  (let improve ((guess initial-guess)
                (i iterations))
    (if (zerop i)
        guess
        (let ((next-guess (newton-step function guess)))
          (if (< (abs (- next-guess guess)) tolerance)
              next-guess
              (improve next-guess (1- i)))))))
