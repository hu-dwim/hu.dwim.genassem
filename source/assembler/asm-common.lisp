;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2025 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.genassem/asm-common)

(defmacro define-instruction (name args &body body)
  `(defun ,name ,args
     ,@body))

(defvar *asm-context*)

#+nil
(hu.dwim.defclass-star:defclass* asm-context ()
  ((mode :type (member 16 32 64))
   (buffer (make-array 64 :element-type (unsigned-byte 8) :adjustable t)
           :type (vector '(unsigned-byte 8)))))

(defclass asm-context nil
  ((mode :accessor mode-of :initform 64 :initarg :mode :type (member 16 32 64))
   (buffer :initform
           (make-array 64 :element-type '(unsigned-byte 8) :adjustable t
                          :fill-pointer 0)
           :accessor buffer-of :initarg :buffer :type
           (vector '(unsigned-byte 8)))))

(defun current-execution-mode ()
  (mode-of *asm-context*))

(define-instruction bits (bits)
  (assert (member bits '(16 32 64)))
  (setf (mode-of *asm-context*) bits))

(define-condition assembler-error
    (serious-condition)
  ())

(define-condition simple-assembler-error
    (assembler-error simple-error)
  ())

#+nil
(hu.dwim.defclass-star:defcondition* invalid-instruction-error
    (simple-assembler-error)
  ((instruction)))

(define-condition invalid-instruction-error
    (simple-assembler-error)
  ())

(defun invalid-instruction-error (&optional format &rest args)
  (signal 'invalid-instruction-error
          :format-control format
          :format-arguments args))

(define-condition invalid-operand-error
    (invalid-instruction-error)
  ((operand :accessor operand-of :initarg :operand)))

(defun invalid-operand-error (operand &optional format &rest args)
  (signal 'invalid-operand-error
          :operand operand
          :format-control format
          :format-arguments args))

(defmacro with-asm (() &body body)
  `(let ((*asm-context* (make-instance 'asm-context)))
     ,@body
     *asm-context*))

(defun emit-byte (byte)
  (vector-push-extend byte (buffer-of *asm-context*)))

(defun emit-bytes (bytes)
  (map nil 'emit-byte bytes))

(defun emit-bytes-form (var)
  (check-type var (or null (cons fixnum)))
  (if var
      (if (cdr var)
          `((emit-bytes ',var))
          `((emit-byte ',(first var))))
      (values)))
