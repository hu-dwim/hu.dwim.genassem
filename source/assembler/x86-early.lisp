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
         (structure-object
          ;; we punt...
          t))))

;;;
;;; Register lookup/decode
;;;

(defmacro define-register-class (name)
  `(defstruct (,name (:constructor ,(symbolicate '#:%make- name) (index)))
     (index 0 :type fixnum)))

(defmacro define-register-instances (class-name names)
  (let ((constant-names ())
        (instances-name (symbolicate class-name '#:/instances)))
    `(progn
       ,@(mapcar (lambda (name index)
                   (push name constant-names)
                   `(define-constant ,name (,(symbolicate '#:%make- class-name) ,index)
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
(define-register-class gr8)
(define-register-class gr16)
(define-register-class gr32)
(define-register-class gr64)

;;;
;;; Float, x87, ST(0)-ST(7) registers
;;;
(define-register-class st)

;;;
;;; Vector registers
;;;
(define-register-class vr64)
(define-register-class vr128)
(define-register-class vr256)
(define-register-class vr512)

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
