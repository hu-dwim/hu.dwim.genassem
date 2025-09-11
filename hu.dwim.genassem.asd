;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2025 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.genassem
  :description ""
  :author "Attila Lendvai"
  :license "BSD or Bugroff"
  :version "0.1"

  :in-order-to ((develop-op
                 (load-op :hu.dwim.genassem)
                 (load-op :hu.dwim.genassem/asm-common)
                 (load-op :hu.dwim.genassem/asm-x86)
                 (load-op :hu.dwim.genassem/x86.test)))

  :depends-on (:alexandria
               :anaphora
               :hu.dwim.genassem/asm-common
               :com.inuoe.jzon
               :uiop
               :metabang-bind
               :split-sequence)
  :components ((:module "source"
                :components
                ((:module "generator"
                  :components ((:file "package")
                               (:file "parsing" :depends-on ("package"))
                               (:file "x86-generator" :depends-on ("parsing"))))))))

(defsystem :hu.dwim.genassem/asm-common
  :description ""
  :author "Attila Lendvai"
  :license "BSD or Bugroff"
  :version "0.1"

  :depends-on (:alexandria
               :anaphora
               :uiop)
  :components ((:module "source"
                :components
                ((:module "assembler"
                  :components ((:file "package")
                               (:file "utils" :depends-on ("package"))
                               (:file "asm-common" :depends-on ("package" "utils"))
                               ))))))

(defsystem :hu.dwim.genassem/x86
  :description ""
  :author "Attila Lendvai"
  :license "BSD or Bugroff"
  :version "0.1"

  :depends-on (:alexandria
               :anaphora
               :hu.dwim.genassem/asm-common
               :uiop)
  :in-order-to ((test-op (test-op :hu.dwim.genassem/x86.test)))
  :components ((:module "source"
                :components
                ((:module "assembler"
                  :components ((:file "x86-early")
                               (:file "x86" :depends-on ("x86-early"))
                               (:file "x86-instructions" :depends-on ("x86"))
                               ))))))

(defsystem :hu.dwim.genassem/x86.test
  :depends-on (:alexandria
               :anaphora
               :hu.dwim.stefil
               :hu.dwim.genassem/x86
               :uiop
               ;; you probably also want to load :hu.dwim.stefil+swank one way or another
               )
  ;; Unfortunately ASDF swallows the return value (i.e. it cannot be
  ;; inspected in Slime), so we at least print it.
  :perform (test-op (o c) (print (funcall (intern (string '#:test)
                                                  (find-package :hu.dwim.genassem/x86.test)))))
  :components
  ((:module "test"
    :components
    ((:file "package")
     (:file "suite" :depends-on ("package"))
     (:module "x86"
      :components
      ((:file "simple")
       (:file "infra")
       (:file "x87")
       (:file "sse"))
      :depends-on ("package" "suite"))))))
