;;; traverser

;; interesting project
;; https://github.com/vsedach/Vacietis/blob/master/compiler/reader.lisp

(declaim (optimize (safety 2) (debug 3)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:alexandria :draw-cons-tree :fiveam)))

(defpackage :traverser
  (:use :commpn-lisp))

(in-package :traverser)

;;; ==========================================================================
(defun dt (ls)
  (draw-cons-tree:draw-tree ls))

(defun deep-reverse (ls)
  (cond ((null ls) ls)
        ((atom ls) ls)
        ((consp ls) (cons (deep-reverse (cdr ls))
                          (deep-reverse (car ls))))
        (T (error "type of ~a is not recognised" (type-of ls)))))

;;; ==========================================================================
(defun parents (obj)
  (loop for cl in (sb-mop:class-precedence-list (class-of obj))
        collect (sb-mop:class-name cl)))

;;; ==========================================================================
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
