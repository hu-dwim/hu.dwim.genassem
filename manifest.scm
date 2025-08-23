;; Usage:
;;
;; guix shell
;; guix shell --pure -- make test
;; # if you want to protect the packages from guix gc:
;; guix package --manifest=manifest.scm --profile=.guix-profile
;; guix shell --profile=.guix-profile

(use-modules
 (gnu packages bash)
 (guix packages)
 (gnu packages llvm)
 (gnu packages commencement))

;;;
;;; To protect the LLVM source from guix gc...
;;;
(define (contain-source package)
  (computed-file
   (string-append "source-of-" (package-name package))
   (with-imported-modules '((guix build utils))
     #~(begin
         (use-modules (guix build utils))
         (let ((srcs (string-append #$output "/srcs"))
               (source #$(package-source package)))
           (mkdir-p srcs)
           (symlink source
                    (string-append srcs "/" (basename source))))))))

(define (package-source->manifest-entry package)
  (manifest-entry
    (name (string-append "source-of-" (package-name package)))
    (version (package-version package))
    (item (contain-source package))))

(manifest
 (append
  ;; Protect the LLVM source code from `guix gx`.
  (list (package-source->manifest-entry llvm-20))

  ;; you can pick specific version-classes here
  (map package->manifest-entry
       (list clang-toolchain-20
             ;; gcc-toolchain
             ))

  ;; get the latest from the channels that you have `guix pull`ed
  (manifest-entries
   (specifications->manifest
    '(;;"clang-toolchain"
      "glibc"
      "sbcl"
      ;; "libffi"

      ;;
      ;; the rest is only for convenience
      ;;
      "git"
      "ncurses"        ; for clear, reset, tput (used by the Makefile)
      "coreutils"
      "diffutils"
      "findutils"
      "perf"
      "bash-completion"
      "gawk"
      "sed"
      "git:gui"
      "man-pages"
      "less"
      "gdb"
      "intel-xed" ; for authoritative disassembling
      )))))
