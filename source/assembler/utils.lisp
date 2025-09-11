;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2025 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.genassem/asm-common)

(defun fully-qualified-symbol-name (symbol &key separator)
  (let ((symbol-name (symbol-name symbol))
        (package (symbol-package symbol))
        (keyword-package (load-time-value (find-package "KEYWORD"))))
    (if package
        (concatenate
         'string
         (unless (eq package keyword-package)
           (package-name package))
         (or separator
             (if (or (eq package keyword-package)
                     (eq (nth-value 1 (find-symbol symbol-name package)) :external))
                 ":"
                 "::"))
         symbol-name)
        (concatenate 'string "#:" symbol-name))))
