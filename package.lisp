(in-package :cl-user)

(defpackage #:cl-foma
  (:export ;; #:foma-tokenize
	   #:foma-map-tokens
	   #:foma-net
	   #:foma-tokenizer
	   ;; #:foma-pattern-matcher
	   ;;#:token-boundary
	   #:foma-lookup
	   ;;#:foma-apply-patterns #:set-pattern-match-type
	   )
  (:use #:cl #:cffi  #:acl-compat.mp))

:eof