;;; traverser

;; interesting project
;; https://github.com/vsedach/Vacietis/blob/master/compiler/reader.lisp

(declaim (optimize (safety 2) (debug 3)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:alexandria :draw-cons-tree :fiveam)))

(defpackage :traverser
  (:use :common-lisp)
  (:import-from :fiveam #:test #:run #:run! #:! #:def-suite #:is))

(in-package :traverser)

;;; ==========================================================================80
(defun dt (ls)
  (draw-cons-tree:draw-tree ls))

(defun deep-reverse (ls)
  (cond ((null ls) ls)
        ((atom ls) ls)
        ((consp ls) (cons (deep-reverse (cdr ls))
                          (deep-reverse (car ls))))
        (T (error "type of ~a is not recognised" (type-of ls)))))

;;; ==========================================================================80
(defun parents (obj)
  (loop for cl in (sb-mop:class-precedence-list (class-of obj))
        collect (sb-mop:class-name cl)))

;;; ==========================================================================80
;;; find-if and member-if
(defun first-matching (fn ls)
  (find-if (lambda (x) (handler-case (funcall fn x) (condition () nil))) ls))

(defun has-other-than (cl stack)
  (member-if-not cl stack))

;;; ==========================================================================
(defun eat-while (fn ls)
  (labels ((eat (fn lz)
             (when (cdr lz)
               (if (funcall fn (cadr lz))
                   (eat fn (cdr lz))
                   (setf (cdr lz) nil)))))
    (when (funcall fn (car ls))
      (eat fn ls)
      ls)))

;;; ===================== test examples =============================
;; https://github.com/sionescu/fiveam/blob/master/t/example.lisp
;; https://www.darkchestnut.com/2018/how-to-write-5am-test-fixtures/
;; https://quickref.common-lisp.net/fiveam.html
;; importing functions
;; https://lispcookbook.github.io/cl-cookbook/systems.html

;;; testing ====================================
;; https://gist.github.com/lagagain/1d2e416067f5e32f7b15e7d20e4a72c3

(5am:def-suite first-test-suite)
(5am:in-suite first-test-suite)

(test eat-while
  "test the eat-while function"
  (is (equal '(1 3 5) (eat-while #'oddp '(1 3 5 6 8))))
  (is (equal '(2 4 6) (eat-while (lambda (x) (not (oddp x))) '(2 4 6 7 9)))))

;;; different ways of running single function test

;; (run! 'eat-while)
(5am:run! 'first-test-suite)
;; (5am:run-all-tests)

;; (5am:debug! 'add2) ; with debugging on failure
;;; or repeat the same
;; (!)
;;; all tests
;; (5am:run-all-tests)
