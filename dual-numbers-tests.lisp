g;;; -*- Lisp -*-

(in-package "DUAL-NUMBERS-TESTS")

(defun test-creation ()
  (let ((d (make-dual 2 3)))
    (assert (= (standard-part d) 2))
    (assert (= (infinitesimal-part d) 3)))
  (let ((d (make-dual 2 0)))
    (assert (= d 2)))
  (assert (typep (make-dual 2 3) 'dual-number))
  (assert (not (typep (make-dual 2 0) 'dual-number))))

(defun test-arithmetic ()
  (let ((d1 (make-dual 2 3))
        (d2 (make-dual 4 5)))
    (assert (= (+ d1 d2) (make-dual 6 8)))
    (assert (= (+ d1 2) (make-dual 4 3)))
    (assert (= (+ 2 d1) (make-dual 4 3)))
    (assert (= (- d1 d2) (make-dual -2 -2)))
    (assert (= (- d1 2) (make-dual 0 3)))
    (assert (= (- 2 d1) (make-dual 0 -3)))
    (assert (= (* d1 d2) (make-dual 8 22)))
    (assert (= (* d1 2) (make-dual 4 6)))
    (assert (= (* 2 d1) (make-dual 4 6)))
    (assert (= (/ d1 2) (make-dual 1 3/2)))
    (assert (= (/ 2 d1) (make-dual 1 -3/2)))))

(defun test-conjugate ()
  (let ((d (make-dual 2 3)))
    (assert (= (conjugate d) (make-dual 2 -3)))))

(defun test-zerop ()
  (assert (zerop (make-dual 0 0)))
  (assert (not (zerop (make-dual 1 0))))
  (assert (not (zerop (make-dual 0 1))))
  (assert (not (zerop (make-dual 1 1)))))

(defun test-transcendental ()
  (let ((d (make-dual 1 1)))
    (assert (= (exp d) (make-dual (exp 1) (exp 1))))
    (assert (= (log d) (make-dual 0 1)))
    (assert (= (log d 10) (make-dual 0 (/ 1 (log 10)))))
    (assert (= (sqrt d) (make-dual 1 1/2)))
    (assert (= (sin d) (make-dual (sin 1) (cos 1))))
    (assert (= (cos d) (make-dual (cos 1) (- (sin 1)))))
    (assert (= (tan d) (make-dual (tan 1) (/ 1 (* (cos 1) (cos 1))))))
    (assert (= (sinh d) (make-dual (sinh 1) (cosh 1))))
    (assert (= (cosh d) (make-dual (cosh 1) (sinh 1))))
    (assert (= (tanh d) (make-dual (tanh 1) (/ 1 (* (cosh 1) (cosh 1))))))
    (assert (= (expt d 2) (make-dual 1 2)))
    (assert (= (expt 2 d) (make-dual 2 (* 2 (log 2)))))
    (assert (= (expt d d) (make-dual 1 1)))))

(defun test-derivative ()
  (assert (= (funcall (derivative (lambda (x) (* x x))) 2) 4))
  (assert (= (funcall (derivative #'exp) 1) (exp 1)))
  (assert (= (funcall (derivative #'log) 1) 1))
  (assert (= (funcall (derivative #'sqrt) 4) 1/4)))

(defun run-tests ()
  (test-creation)
  (test-arithmetic)
  (test-conjugate)
  (test-zerop)
  (test-transcendental)
  (test-derivative)
  (format t "All tests passed!~%"))

(run-tests)
