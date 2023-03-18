(in-package :cl-foma)

#+test
(print (libcfsm-version))

#+test
(print (get-default-cfsm-context))

#+test
(defparameter *net*
  (with-foreign-string (cstr "/Users/paul/fst/cfsm_api/src/apply/test/FrenchEnglish.fst")
    (load-net1 cstr *fst-cntxt*)))

#+test ;; +
(defparameter *applyer* (init-apply *net* 0 *fst-cntxt*))

#+test ;; +
(print (apply-to-string "me" *applyer*))


#+test
(let ((cstr (foreign-string-alloc "/Users/paul/fst/cfsm_api/src/apply/test/FrenchEnglish.fst" :null-terminated-p t)))
  (print cstr)
  (defparameter *net* (load-net1 cstr *fst-cntxt*)))

#+test
(defparameter *net* (load-net "/home/paul/quicklisp/local-projects/georgian-morph/regex/georgian-morph-ng.foma" *fst-cntxt*))

#+test
(defparameter *net* (load-net "/Users/paul/fst/cfsm_api/src/apply/test/FrenchEnglish.fst" *fst-cntxt*))
#+test
(defparameter *net* (load-nets "/Users/paul/fst/cfsm_api/src/apply/test/FrenchEnglish.fst" (get-default-cfsm-context)))

#+test
(defparameter *net* (load-net "/Users/paul/fst/cfsm_api/src/apply/test/empty.fst" *fst-cntxt*))

#+test ;; +
(defparameter *net* (read-text "/Users/paul/fst/cfsm_api/src/apply/test/words.txt"))

#+test ;; +
(defparameter *net* (read-prolog "/Users/paul/lisp/projects/iness/cl-fst/prolog.pl"))

#+test ;; -
(print (write-text *net* "/Users/paul/fst/cfsm_api/src/apply/test/words1.txt")) 

#+test ;; -
(print (write-text *net* (null-pointer))) 

#+test
(defparameter *int-parameters* (int-parameters))

#+test
(let* ((ptr (int-parameters))
       (general (foreign-slot-value ptr 'interface-parameters 'general)))
  (setf (foreign-slot-value general 'general 'verbose) 0))

#+test
(let* ((ptr (int-parameters))
       (general (foreign-slot-value ptr 'interface-parameters 'io)))
  (setf (foreign-slot-value general 'io 'print-space) 1))

#+test ;; +
(defparameter *net* (read-regex "{ich}:[{me}|{moi}]"))

#+test ;; +
(setf *net* (invert-net *net* 0))

#+test ;; +
(setf *net* (reverse-net *net* 0))

#+test ;; -
(print (save-net *net* "/Users/paul/fst/cfsm_api/src/apply/test/test.fst" *fst-cntxt*))

#+test
;;(with-foreign-string (cstr "Jeg håper vi klarer det. Det gjør vi sikkert.")
(defparameter *applyer* (new-applyer *net* "seventeen" (null-pointer) 0 *fst-cntxt*))

#+test
(defparameter *applyer* (init-apply *net* 0 (null-pointer))) 


#+test
(print *net*)


#+test
(reclaim-cfsm *fst-cntxt*)

;; gcc -m64 -Wall -bundle -flat_namespace -o libcfsm.dylib cfsm.c

;; INESS


#+test
(print (single-to-id "TB"))

#+test
(print (single-to-id "FAIL\nTOKEN"))

(defparameter *net* (make-instance 'fst-net :file "/Users/paul/lisp/projects/iness/morphology/regex/nob-breaktext-plain-lfg.fst"))


#+test
(defparameter *tok*
  (print (new-tokenizer
	  *net*
	  "\"Dr. Shemtov works at Yahoo!,\" she said. Apples, oranges, etc. I won't eat."
	  ;;"Jeg håper vi klarer det. Det gjør vi sikkert."
	  (null-pointer)
	  (single-to-id "TB")
	  (single-to-id "FAILED_TOKEN")
	  *fst-cntxt*)))

#+test
(defparameter *page*
  (foreign-slot-value
   (foreign-slot-value *fst-cntxt* 'cfsm-context 'temp-bufs)
   'temporary-buffers 'page-buffer))

#+test
(print (foreign-slot-value (foreign-slot-value *fst-cntxt* 'cfsm-context 'label-stats)
			   'label-stats 'max-label))


#+test ;; +++++
(fst-map-tokens *net*
		"Dette var rart. Jeg skjønner det ikke. Men nå virker det."
		#'print
		:token-boundary #.(string #\newline))

#+test ;; +++++
(fst-tokenize *fst-cntxt*
	      "/Users/paul/fst/cfsm_api/src/tokenize/test/token.fst"
	      "\"Dr. Shemtov works at Yahoo!,\" she said. Apples, oranges, etc. I won't eat.")

#+test ;; +++++
(fst-tokenize *fst-cntxt*
	      "/Users/paul/lisp/projects/iness/morphology/regex/nob-breaktext-plain-lfg.fst"
	      "Dette var rart. Jeg skjønner det ikke."
	      :token-boundary #.(string #\newline))


#+test
(with-open-file (stream "projects:iness;cl-fst;test.txt"
			:direction :output :if-exists :supersede)
  (let ((cstream (fdopen (sb-sys::fd-stream-fd stream) "w")))
    (print-page *page* cstream)
    (fflush cstream)))

#+test ;; +++
;;(with-foreign-string (cstr "Jeg håper vi klarer det. Det gjør vi sikkert.")
(let ((cstr (foreign-string-alloc "Jeg håper vi klarer det. Det gjør vi sikkert." :null-terminated-p nil)))
  (let* ((fst-cntxt *fst-cntxt* #+ignore (initialize-cfsm))
	 (tok (make-tokenizer1
	       "/Users/paul/fst/cfsm_api/src/tokenize/test/token.fst"
	       ;;"/Users/paul/lisp/projects/iness/morphology/regex/nob-breaktext-plain-lfg.fst"
	       cstr ;; "Jeg håper vi klarer det. Det gjør vi sikkert."
	       (null-pointer)
	       "TB"
	       "FAILED_TOKEN"
	       fst-cntxt)))
    (reset-page  (context-page-buffer fst-cntxt))
    (loop for tok-net = (next-token-net tok)
       until (null-pointer-p tok-net)
       do
       (words-to-page tok-net 0 0 (context-page-buffer fst-cntxt))
       (free-network tok-net)
       )
    (princ (foreign-slot-value (context-page-buffer fst-cntxt) 'page 'string))
    ;;(free-tokenizer tok)
    #+test
    (reclaim-cfsm fst-cntxt)))

#+test
(with-open-file (stream "/Users/paul/fst/cfsm_api/src/tokenize/test/input.txt")
  (let ((cstream (fdopen (sb-sys::fd-stream-fd stream) "r")))
    (let* ((fst-cntxt *fst-cntxt* #+ignore(initialize-cfsm))
	   (net (load-net "/Users/paul/fst/cfsm_api/src/tokenize/test/token.fst" fst-cntxt))
	   (tok (new-tokenizer net
			       (null-pointer)
			       cstream
			       (single-to-id "TB")
			       (single-to-id "FAILED_TOKEN")
			       fst-cntxt)))
      (print (list :fst-cntxt fst-cntxt :net net :tok tok))
      (loop for tok-net = (next-token-net tok)
	   until (null-pointer-p tok-net)
	   do
	   (words-to-page tok-net 0 0 (context-page-buffer fst-cntxt))
	   ;;(print (foreign-slot-value (context-page-buffer fst-cntxt) 'page 'string))
	   ;;(reset-page (context-page-buffer fst-cntxt))
	   (free-network tok-net)
	   )
      (free-tokenizer tok)
      (princ (foreign-slot-value (context-page-buffer fst-cntxt) 'page 'string))
      #+test
      (reclaim-cfsm fst-cntxt))))


;;#+allegro(excl::stream-input-handle stream)
;;#+sbcl(sb-sys::fd-stream-fd stream)

#+test
(print (sb-sys::fd-stream-fd *standard-output*))

#+test
(print (sb-impl::get-descriptor-for :stream nil :direction :output))

#+copy
(with-slots (graph-address) *xle-graph*
  (with-cstr (mode "w")
    (with-open-file (stream "projects:treebank;parse-selection;test1.fs"
			    :direction :output :if-exists :supersede)
      (let ((cstream (fdopen (excl::stream-output-handle stream) mode)))
	(print-prolog-graph cstream graph-address "")
	(fflush cstream)))))

:eof
