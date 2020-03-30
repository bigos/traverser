;;; traverser

;;; this is an example traversin SBCL class graph allowing you to inspect
;;; the nodes, see help for more options

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

(defun ansicolors-codes (&optional (codes ""))
  "Create ansi-codes fragment. When a code or semicolon separated codes are
supplied use them to create opening fragment. Or create closing fragment when
no codes are supplied."
  (format nil "~c[~am" #\Esc codes))

;;; make sure you have
;; ~/.emacs.d/elpa/slime-xxxxxxxx.yyyy/contrib/slime-repl-ansi-color.el
(defun ansicolors ()
  (loop for x from 30 to 37 do (format t "~A#~a#~A "
                                       (ansicolors-codes (format nil "~s;40" x))
                                       x
                                       (ansicolors-codes))))

(defun deep-reverse (ls)
  (cond ((null ls) ls)
        ((atom ls) ls)
        ((consp ls) (cons (deep-reverse (cdr ls))
                          (deep-reverse (car ls))))
        (T (error "type of ~a is not recognised" (type-of ls)))))

;;; ================== main ====================================================
(defun process (input current-class)
  (format t "~%~%processing > ~A~&" input)

  (cond
    ((equal "help" input)
     (format t "commands:~%")
     (format t "help - this help~%")
     (format t "ii - inspect in debugger~%")
     (format t "i - inspect~%")
     (format t "c - children~%")
     (format t "p - parents~%")
     (format t "t - top of the hierarchy~%"))

    ((equal "ii" input)                 ;inspect in debugger
     (break))

    ((equal "i" input)                  ;inspect
     (progn
       (let ((parents (sb-mop:class-direct-superclasses current-class)))
         (format t "parent path is: ~A~%" (sb-mop:compute-class-precedence-list current-class))
         (format t "parents is: ~A~%" parents)))
     (format t "current class ~A~%" current-class)
     (format t "children ~A~%" (sb-mop:class-direct-subclasses current-class)))

    ((equal "c" input)                  ;children
     (let ((children (loop
                        for n = 1 then (1+ n)
                        and cl in (sb-mop:class-direct-subclasses current-class)
                        collect (list n cl (length (sb-mop:class-direct-superclasses cl))))))
       (if (> (length children) 0)
           (progn
             (format t "~A~%" children)
             (format t "enter child number from ~a to ~a >" (car (first children)) (caar (last children)))
             (let* ((child-id (parse-integer (read-line))))
               (setf current-class (cadr (assoc child-id children)))))
           (format t "no further children~%"))))

    ((equal "p" input)                  ;parents
     (let ((parents (loop
                       for n = 1 then (1+ n)
                       and cl in (sb-mop:class-direct-superclasses current-class)
                       collect (list n cl))))
       (if (> (length parents) 0)
           (progn
             (format t "~A~%" parents)
             (format t "enter parent number ")
             (let ((parent-id (parse-integer (read-line))))
               (setf current-class (cadr (assoc parent-id parents)))))
           (format t "no more parents~%"))))

    ((equal "t" input)                 ;top of the hierarchy
     (format t "going back to the top~%")
     (setf current-class (find-class 't)))


    (T
     (format t "unimplemented command ~A" input)))
  (format t "~%")
  current-class)

(defun main ()
  (let ((input "help")
        (current-class (find-class 'T)))
    (loop until (equal input "quit")
       do
         (unless (equal "" input)
           (setf current-class (process input current-class)))
         (format t "~A > " current-class)
         (setf input (read-line)))))
