;;; traverser

;; interesting project
;; https://github.com/vsedach/Vacietis/blob/master/compiler/reader.lisp

(declaim (optimize (safety 2) (debug 3)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:alexandria :draw-cons-tree :fiveam) :silent T))  ; suppresses feedback

(defpackage :traverser
  (:use :common-lisp)
  (:import-from :fiveam #:test #:run #:run! #:! #:def-suite #:is))

(in-package :traverser)

;; (format t "OOOOOOOOOOOOOO loaded the package OOOOOOOOOOOOOOOOOOOOOO~%")

;;; ============================================================================
(defun dt (ls)
  (draw-cons-tree:draw-tree ls))

(defun deep-reverse (ls)
  (cond ((null ls) ls)
        ((atom ls) ls)
        ((consp ls) (cons (deep-reverse (cdr ls))
                          (deep-reverse (car ls))))
        (T (error "type of ~a is not recognised" (type-of ls)))))

;;; === objects ================================================================

(defparameter tclass (cadr (sb-mop:compute-class-precedence-list
                            (class-of 'symbol)))
  "find T which is the top class of SBCL object system")

(defun parents (obj)
  (loop for cl in (sb-mop:class-precedence-list (class-of obj))
        collect (sb-mop:class-name cl)))

(defun classify (obj)
  (if (equal 'built-in-class (type-of obj))
      obj
      (class-of obj)))

(defun class-parents (obj)
  (sb-mop:compute-class-precedence-list (classify obj)))

(defun subclasses (obj)
  (sb-mop:class-direct-subclasses
   (classify obj)))

(defun superclasses (obj)
  (sb-mop:class-direct-superclasses
   (classify obj)))

;;; ============================================================================
;;; find-if and member-if
(defun first-matching (fn ls)
  (find-if (lambda (x) (handler-case (funcall fn x) (condition () nil))) ls))

(defun has-other-than (cl stack)
  (member-if-not cl stack))

;;; ============================================================================
(defun eat-while (fn ls)
  (labels ((eat (fn lz)
             (when (cdr lz)
               (if (funcall fn (cadr lz))
                   (eat fn (cdr lz))
                   (setf (cdr lz) nil)))))
    (when (funcall fn (car ls))
      (eat fn ls)
      ls)))

;;; === appending ==============================================================
(defparameter zzz nil)
(setf zzz '(1))
(setf (cdr zzz) '(2))
(setf (cddr zzz) '(3))
(format t "zzz is now ~A~%" zzz)
;; zzz is now (1 2 3)
(setf (cadr zzz) 'two)
(format t "zzz is now ~A~%" zzz)
;; zzz is now (1 TWO 3)
(setf (caddr zzz) 'three)
(format t "zzz is now ~A~%" zzz)
;; zzz is now (1 TWO THREE)

(append '(1 2 3) '(4 5))                ; (1 2 3 4 5)

;;; ================= joining ==================================================
(defun can-join (stack)
  (cond ((and
          (and  (consp (car  stack))
                (eq    (caar stack)
                       's))
          (characterp (cadr stack))))
        (T nil)))
;; (can-join (list (build-struct 's nil "z") #\b))

;;; ================= simple  structures =======================================

(defun build-struct (n p r)
  (list n p r))

(defmacro struct-inst (s)
  `(car ,s))

(defmacro struct-params (s)
  `(cadr ,s))

(defmacro struct-data (s)
  `(caddr ,s))

;;; ============= destructuring ================================================

(defparameter aaa (build-struct 'a (cons 0 2) "abc" ))
;;; AAA

(destructuring-bind (symbol parameter result)
    aaa
  `(,symbol ((,parameter))
            ((,result)
             '(((the-end))))))
;; (A ((PAR)) ((RES) '(((THE-END)))))

;;; ================= tree growing =============================================

(defun sconc (str1 str2 i2)
  "Concatenate string STR1 and I2 element of STR2"
  (concatenate 'string str1 (string (aref str2 i2))))

(defun eat (el s2 i2)
  "consume EL adding I2 element of S2"
  (let ((par (cadr el))
        (res (caddr el)))
    (list (car el)
          (cons (car par) (1+ (cdr par)))
          (sconc res s2 i2))))
;; TRAVERSER> aaa
;; (A (0 . 2) "abc")
;; TRAVERSER> (eat aaa "def" 0)
;; (A (0 . 3) "abcd")

;;; ===================== test examples ========================================
;; https://github.com/sionescu/fiveam/blob/master/t/example.lisp
;; https://www.darkchestnut.com/2018/how-to-write-5am-test-fixtures/
;; https://quickref.common-lisp.net/fiveam.html
;; importing functions
;; https://lispcookbook.github.io/cl-cookbook/systems.html

;;; testing ====================================================================
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
