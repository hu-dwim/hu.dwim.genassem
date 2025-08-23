;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2025 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.genassem/asm-common)

(defvar *current-instruction*)

(defmacro define-instruction (&whole instr-form name args &body body)
  `(defmacro ,name ,args
     (let ((*current-instruction* ',instr-form))
       ,@body)))

(defvar *asm-context*)

#+nil
(hu.dwim.defclass-star:defclass* asm-context ()
  ((mode :type (member 16 32 64))
   (buffer (make-array 64 :element-type (unsigned-byte 8) :adjustable t)
           :type (vector '(unsigned-byte 8)))))

(defclass asm-context nil
  ((mode :accessor mode-of :initarg :mode :type (member 16 32 64))
   (buffer :initform
           (make-array 64 :element-type '(unsigned-byte 8) :adjustable t
                          :fill-pointer 0)
           :accessor buffer-of :initarg :buffer :type
           (vector '(unsigned-byte 8)))))

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
  ((instruction :accessor instruction-of :initarg :instruction)))

(defun invalid-instruction-error (&optional format &rest args)
  (check-type *current-instruction* list)
  (signal 'invalid-instruction-error
          :instruction *current-instruction*
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
