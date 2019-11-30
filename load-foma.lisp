(in-package :cl-foma)


;; adapt to your environment

(define-foreign-library libfoma
  (:darwin
   #+X86-64
   (:default "/usr/local/lib/libfoma"))
  (:unix
   #+X86-64
   (:default "/usr/local/lib/libfoma")))

(use-foreign-library libfoma)

:eof