(in-package :cl-foma)

(defcfun "fdopen" :ulong
  (fd :short)
  (mode :string))

(defcfun "fflush" :short
  (stream :ulong))

(defcfun "fsm_read_binary_file" :pointer
  (filename :string))

(defcfun "apply_init" :pointer
  (fsm :pointer))

(defcfun "apply_down" :string
  (ah :pointer)
  (word :string))

(defcfun "apply_up" :string
  (ah :pointer)
  (word :string))

(defcfun "apply_clear" :void
  (ah :pointer))


:eof
