;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2025 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.genassem/x86)

(defun reg-stuff-equal (a b)
  ;; only for silencing the DEFINE-CONSTANTs below...
  (and (equal (type-of a)
              (type-of b))
       (etypecase a
         (symbol (eq a b))
         (array  (and (= (length a)
                         (length b))
                      (every 'reg-stuff-equal a b)))
         (cons   (and (reg-stuff-equal (car a) (car b))
                      (reg-stuff-equal (cdr a) (cdr b))))
         (standard-object
          ;; we punt...
          t))))

;;;
;;; Registers and their lookup/decode
;;;


#+nil
(hu.dwim.defclass-star:defclass* register ()
  ((index :type fixnum)
   (max-index 7 :allocation :class)))

(defclass register ()
  ((index :accessor index-of :initarg :index :type fixnum)
   (max-index :initform 7 :accessor max-index-of :initarg :max-index
              :allocation :class)))

(defmethod print-object ((self register) *standard-output*)
  (format t "#<~A ~A>"
          (class-name (class-of self))
          (if (slot-boundp self 'index)
              (index-of self)
              #\?)))

(defclass gr (register)
  ((max-index :allocation :class :initform 15 :accessor max-index-of)))

(defclass vr (register)
  ((max-index :allocation :class :initform 15 :accessor max-index-of)))

(defmacro define-register-class (name &optional (supers '(register)) direct-slots)
  `(defclass ,name ,supers
     ,direct-slots))

(defmacro define-register-instances (class-name names)
  (let ((constant-names ())
        (instances-name (symbolicate class-name '#:/instances)))
    `(progn
       ,@(mapcar (lambda (name index)
                   (push name constant-names)
                   `(define-constant ,name (make-instance ',class-name :index ,index)
                      :test 'reg-stuff-equal))
                 names
                 (alexandria:iota (length names)))
       (define-constant ,instances-name
           (make-array ,(length constant-names)
                       :initial-contents (list ,@(mapcar (lambda (name)
                                                           `(list ',name ,name))
                                                         (reverse constant-names))))
         :test 'reg-stuff-equal)
       ;; TODO optimization: create an index based lookup for numbered registers
       (defun ,class-name (name-or-index &optional (otherwise nil otherwise?))
         (etypecase name-or-index
           (symbol (acond
                     ((find name-or-index ,instances-name :key 'first :test 'eq)
                      (second it))
                     (otherwise? otherwise)
                     (t (unknown-register name-or-index))))
           (integer (second (aref ,instances-name name-or-index)))))
       (export ',class-name)
       (export '(,@constant-names)))))

(defmacro define-register-instances/numbered (class-name name-prefix count &key (start 0))
  `(define-register-instances ,class-name
     (,@(mapcar (lambda (index)
                  `,(symbolicate name-prefix (write-to-string index)))
                (alexandria:iota count :start start)))))

;;;
;;; Generic registers
;;;
(define-register-class gr8  (gr))
(define-register-class gr16 (gr))
(define-register-class gr32 (gr))
(define-register-class gr64 (gr))

;;;
;;; Float, x87, ST(0)-ST(7) registers
;;;
(define-register-class st)

;;;
;;; Vector registers
;;;
(define-register-class vr64  (vr))
(define-register-class vr128 (vr))
(define-register-class vr256 (vr))
(define-register-class vr512 (vr))

;;;
;;; Segment registers
;;;
(define-register-class sr) ; segment_reg

;;;
;;; Control registers
;;;
(define-register-class cr) ; control_reg

;;;
;;; Debug registers
;;;
(define-register-class dr) ; debug_reg
