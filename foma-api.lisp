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

#+test
(defparameter *net1* (fsm-read-binary-file "/Users/paul/lisp/projects/iness/morphology/regex/foma/nob-nouns.fst"))
#+test
(let ((ah (apply-init *net1*)))
  (loop with result = (apply-up ah "fisker")
     while result
     do (print result)
       (setf result (apply-up ah (null-pointer))))
  (apply-clear ah))

:eof
