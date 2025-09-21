;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: CL-FOMA; Base: 10 -*-

;; Copyright (c) 2023, Paul Meurer, University of Bergen
;; https://clarino.uib.no
;; All rights reserved.

(in-package :cl-foma)

(defconstant +fst+ :foma)

(defclass fst-net ()
  ((foma-file :initform nil :initarg :file :reader foma-file)
   (net-ptr :initform nil :reader net-ptr)
   (applyer :initform nil :reader applyer)
   (name :initform nil :initarg :name :reader net-name)
   ))

(defclass fst-tokenizer (fst-net)
  ((token-boundary :initform "TB" :initarg :token-boundary :reader token-boundary)
   ;;(fail-token :initform "FAILED_TOKEN" :initarg :fail-token :reader fail-token)
   (escape-p :initform nil :initarg :escape-p :reader escape-p)))

(defclass foma-pattern-matcher (fst-net)
  ())

(defmethod initialize-instance :after ((net fst-net) &key &allow-other-keys)
  (with-slots (foma-file net-ptr applyer) net
    (setf net-ptr (fsm-read-binary-file (namestring (translate-logical-pathname foma-file))))
    (assert (not (null-pointer-p net-ptr)) nil "Could not find ~a" foma-file)
    (setf applyer (apply-init net-ptr))))

(defun apply-to-string (string applyer side)
  (with-output-to-string (stream) ;; we return a single string for compatibility with fst
    (loop with result = (if (eq side :lower)
			    (apply-up applyer string)
			    (apply-down applyer string))
       while result
       do (write-line result stream)
       (setf result
	     (if (eq side :lower)
		 (apply-up applyer (null-pointer))
		 (apply-down applyer (null-pointer)))))))

(defparameter +applyer-lock+ (make-process-lock :name "applyer-lock"))

(defmethod fst-lookup :around (net string function &key &allow-other-keys)
  (declare (ignore net string function))
  (with-process-lock (+applyer-lock+)
    (call-next-method)))

;; fst-lookup() is not thread-safe without locking
(defmethod fst-lookup ((net fst-net) (string string) function &key (side :lower))
  (funcall function string (apply-to-string string (applyer net) side) net))

(defmethod fst-lookup ((net fst-net) (strings list) function &key (side :lower))
  (loop for string in strings
     do (funcall function string (apply-to-string string (applyer net) side) net)))

;; look up using the nets in list until a result is found (equal to priority union of nets)
(defmethod fst-lookup ((nets list) (string string) function &key (side :lower) (mode :priority-union))
  (declare (ignore mode)) ;; not implemented yet
  (loop for net in nets
     for result = (apply-to-string string (applyer net) side)
     until result
     finally (funcall function string (or result "") net)))

(defparameter +tokenizer-lock+ (make-process-lock :name "tokenizer-lock"))

;; have to use locking since tokenizing is not (yet) thread safe
 
;; obs: net should be deterministic; only the first output string is used (TODO: try to check for deterministic)
(defmethod fst-map-tokens ((net fst-tokenizer) (string string) function &key (split-at-nl t))
  (with-process-lock (+tokenizer-lock+)
    (fst-lookup
     net string
     (lambda (string tokens net)
       (declare (ignore string net))
       (if split-at-nl
           (mapc (lambda (token)
                   (let ((token (string-trim (list #\space #\newline) token)))
                     (unless (equal token "")
                       (funcall function token))))
                 (u:split tokens "\\n" ;; #\newline
                          nil nil t))
           (funcall function (string-trim #.(string #\newline) tokens)))
       (return-from fst-map-tokens)))))

:eof
