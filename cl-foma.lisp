(in-package :cl-foma)

;; the global foma context record
;; (defparameter *foma-cntxt* (initialize-cfsm)) ;; Lauri: try using one for each thread

(defclass foma-net ()
  (;;(foma-cntxt :initform *foma-cntxt* :initarg :foma-cntxt :reader foma-cntxt)
   (foma-file :initform nil :initarg :file :reader foma-file)
   (net-ptr :initform nil :reader net-ptr)
   (applyer :initform nil :reader applyer)))

(defclass foma-tokenizer (foma-net)
  ((token-boundary :initform "TB" :initarg :token-boundary :reader token-boundary)
   ;;(fail-token :initform "FAILED_TOKEN" :initarg :fail-token :reader fail-token)
   (escape-p :initform nil :initarg :escape-p :reader escape-p)))

(defclass foma-pattern-matcher (foma-net)
  ())

(defmethod initialize-instance :after ((net foma-net) &key &allow-other-keys)
  (with-slots (foma-file net-ptr applyer) net
    (setf net-ptr (fsm-read-binary-file (namestring (translate-logical-pathname foma-file))))
    (assert (not (null-pointer-p net-ptr)))
    (setf applyer (apply-init net-ptr))))

#+ignore
(defun context-page-buffer (foma-cntxt)
  (foreign-slot-value
   (foreign-slot-value foma-cntxt 'cfsm-context 'temp-bufs)
   'temporary-buffers 'page-buffer))

#+ignore
(defun side-to-id (side)
  (ecase side
    (:upper 0)
    (:lower 1)
    (:both-sides 2)))

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

(defmethod foma-lookup :around (net string function &key &allow-other-keys)
  (declare (ignore net string function))
  (with-process-lock (+applyer-lock+)
    (call-next-method)))

;; foma-lookup() is not thread-safe without locking
(defmethod foma-lookup ((net foma-net) (string string) function &key (side :lower))
  (funcall function string (apply-to-string string (applyer net) side) net))

(defmethod foma-lookup ((net foma-net) (strings list) function &key (side :lower))
  (loop for string in strings
     do (funcall function string (apply-to-string string (applyer net) side) net)))

#+test
(defparameter *net* (make-instance 'foma-net :file "projects:iness;morphology;regex;foma;nob-nouns.fst"))

#+test
(foma-lookup *net* "fisker" (lambda (str morph) (print (list str morph))))

;; look up using the nets in list until a result is found (equal to priority union of nets)
(defmethod foma-lookup ((nets list) (string string) function &key (side :lower))
  (loop with found = nil
     for net in nets
     until (let ((result (funcall function string (apply-to-string string (applyer net) side) net)))
	     (unless (zerop (length result))
	       (funcall function string result net)
	       (setf found t)))
     finally (unless found (funcall function string "" net))))


#+test
(defparameter *fst-net* (make-instance 'cl-fst::fst-net :file "projects:iness;morphology;regex;nob-nouns.fst"))
#+test
(time
 (dotimes (i 5000)
   (dolist (word '("fisk" "fisker" "frosk"))
     (foma-lookup *net* word (lambda (x y) (list x y))))))
#+test
(time
 (dotimes (i 5000)
   (dolist (word '("fisk" "fisker" "frosk"))
     (cl-fst::fst-lookup *fst-net* word (lambda (x y) (list x y))))))

#+test
(progn
  (process-run-function
   "test1"
   (lambda ()
     (dotimes (i 10000)
       (dolist (word '("fisk" "fisker" "frosk"))
	 (foma-lookup *net* word (lambda (x y) (list x y)))))
     (print :done1)
     ))
  (process-run-function
   "test2"
   (lambda ()
     (dotimes (i 10000)
       (dolist (word '("fisk" "fisker" "frosk"))
	 (foma-lookup *net* word (lambda (x y) (list x y)))))
     (print :done2)
     )))

#+test
(progn
  (process-run-function
   "fst1"
   (lambda ()
     (dotimes (i 100)
       (dolist (word '("fisk" "fisker" "frosk"))
	 (cl-fst::fst-lookup *fst-net* word (lambda (x y) (list x y)))))
     (print :done1)
     ))
  (process-run-function
   "fst2"
   (lambda ()
     (dotimes (i 100)
       (dolist (word '("fisk" "fisker" "frosk"))
	 (cl-fst::fst-lookup *fst-net* word (lambda (x y) (list x y)))))
     (print :done1)
     )))


#+test
(defparameter *tok-net* (make-instance 'foma-tokenizer :file "projects:iness;morphology;regex;foma;tokenize.fst"))

#+test
(foma-map-tokens *tok-net* "Jeg er bl.a. dr. scient. RIGUS" #'print)

#+test
(foma-lookup *tok-net* "rIGUS" (lambda (str morph) (declare (ignore str)) (print morph)))


#+test
(foma-lookup *tok-net* "(Virkelig mange,)" (lambda (str morph) (print (list str morph))))

#+test
(cl-user::asdf :cl-fst)
#+test
(defparameter *fst-net* (make-instance 'cl-fst::fst-net :file "projects:iness;morphology;regex;tokenize.fst"))

#+test
(cl-fst:fst-lookup *fst-net* "Jeg er bl.a. dr. scient. Rigus" (lambda (str morph) (declare (ignore str)) (print morph)))

(defparameter +tokenizer-lock+ (make-process-lock :name "tokenizer-lock"))

;; have to use locking since tokenizing is not (yet) thread safe
#+not-yet
(defmethod foma-tokenize ((net foma-tokenizer) (string string))
  (with-slots (foma-cntxt net-ptr) net
    (let ((page (new-page))
	  (tok nil)
	  (escape-p (escape-p net)))
      (unwind-protect
	   (with-foreign-string (cstr string)
	     (with-process-lock (+tokenizer-lock+)
	       (reset-page page)
	       (setf tok (new-tokenizer
			  net-ptr
			  cstr
			  0
			  (single-to-id (token-boundary net))
			  (single-to-id (fail-token net))
			  foma-cntxt))
	       (unless (null-pointer-p tok)
		 (loop for tok-net = (next-token-net tok)
		    until (null-pointer-p tok-net)
		    do (words-to-page tok-net (side-to-id :upper) (if escape-p 1 0) page)
		    (free-network tok-net))
		 (prog1 (foreign-slot-value page 'page 'string)
		   (reset-page page)))))
	(unless (null-pointer-p tok)
	  (free-tokenizer tok))
	(unless (null-pointer-p page)
	  (free-page page)))))) 

;; obs: net should be deterministic; only the first output string is used (TODO: try to check for deterministic)
(defmethod foma-map-tokens ((net foma-tokenizer) (string string) function &key (split-at-nl t))
  (foma-lookup
   net string
   (lambda (string tokens)
     (declare (ignore string))
     (if split-at-nl
	 (mapc (lambda (token)
		 (funcall function token))
	       (u:split tokens #\newline nil nil t))
	 (funcall function (string-trim #.(string #\newline) tokens)))
     (return-from foma-map-tokens))))

:eof
