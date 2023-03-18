(in-package :cl-user)

(defpackage #:cl-foma
  (:export ;; #:foma-tokenize
	   #:fst-map-tokens
	   #:fst-net
	   #:fst-tokenizer
	   ;; #:foma-pattern-matcher
	   ;;#:token-boundary
	   #:fst-lookup
           #:net-name
           #:+fst+
	   ;;#:foma-apply-patterns #:set-pattern-match-type
	   )
  (:use #:cl #:cffi  #:acl-compat.mp))

:eof
