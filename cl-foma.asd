(in-package :cl-user)

(asdf:defsystem cl-foma
  :depends-on (cffi acl-compat utilities)
  :serial t
  :components
  ((:file "package")
   (:file "load-foma")
   (:file "foma-api")
   (:file "cl-foma")))

:eof