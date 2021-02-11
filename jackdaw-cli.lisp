(cl:defpackage #:jackdaw-cli
  (:use #:common-lisp)
  (:export )
  (:documentation "A toolkit for defining dynamic Bayesian networks 
with congruency constraints."))

(in-package #:jackdaw-cli)

(defparameter *prefix* (format nil "A command-line interface for jackdaw models"))
(defparameter *usage*
  (format nil "Model must be any of your jackdaw models. Its value can be one of: {~{~a~^, ~}}.

Use ./jackdaw-cli.lisp <model> --help for more information about the available
parameters of a specific model." (mapcar #'string-downcase jackdaw::*models*)))

;; Global options

(opts:define-opts 
  (:name :help
   :description "print this help text"
   :short #\h
   :long "help")
  (:name :verbose
   :description "be verbose"
   :short #\v
   :long "verbose")
  (:name :debug
   :description "show backtraces"
   :short #\d
   :long "debug")
  (:name :no-log
   :description "do not work in log space"
   :long "no-log")
  (:name :dataset
   :description "read observations from PATH rather than stdin"
   :short #\d
   :long "dataset"
   :arg-parser #'identity
   :meta-var "PATH")
  (:name :model
   :description "read model from PATH"
   :short #\m
   :long "model"
   :arg-parser #'identity
   :meta-var "PATH")
  (:name :freeze
   :description "freeze model state to PATH after processing observations to path"
   :long "freeze"
   :arg-parser #'identity
   :meta-var "PATH")
  (:name :verbose
   :description "verbose output"
   :short #\v
   :long "verbose")
  (:name :read
   :description "read model state from PATH before processing observations"
   :arg-parser #'identity
   :long "read"
   :meta-var "PATH"))

;; Shared sub-command options

(defun read-into-jackdaw (str)
  (let ((*package* (find-package :jackdaw)))
    (read-from-string str)))

(defparameter *shared-options*
  (opts:make-options
   (list
    (list
     :name :help
     :description "print this help text"
     :short #\h
     :long "help")
    (list
     :name :observe
     :description "variables to observe in input"
     :long "observe"
     :arg-parser #'read-into-jackdaw)
    (list
     :name :output-vars
     :description "variables whose value is printed in the output"
     :short #\p
     :long "output-vars"
     :arg-parser #'read-into-jackdaw))))

(defun make-option (optional)
  (let ((name
	  (string-downcase
	   (symbol-name (if (listp optional) (car optional)
			    optional)))))
    (opts::make-option
     (append
      (list
       :name (intern (string-upcase name) :keyword)
       :arg-parser #'read-into-jackdaw
       :description
       (format nil "model parameter ~a~a" name
	       (if (listp optional) (format nil " (default: ~a)" (cadr optional)) "")))
      (when (listp optional)
	(list :default (cadr optional)))
      (if (eq (length name) 1)
	  (list :short (char name 0))
	  (list :long name))))))

(defun make-options (optionals)
   (mapcar #'make-option optionals))

(defun get-model-options (model-name)
  (let* ((parameters (getf jackdaw::*model-parameters* (jackdaw::find-model model-name)))
	 (positionals (subseq parameters 0 (position '&key parameters)))
	 (optionals (when (member '&key parameters)
		      (subseq parameters (1+ (position '&key parameters))))))
    (values positionals (make-options optionals))))

(defmacro pop-options (variables options &body body)
  `(destructuring-bind (&rest ,options &key ,@variables &allow-other-keys)
       ,options
     ,@(loop for v in variables collect `(remf ,options ,(intern (symbol-name v) :keyword)))
     ,@body))

(defun parse-args (&optional (argv (opts::argv)) 
		      positional-args (defined-options opts::*options*))
  (multiple-value-bind (options free)
      (handler-case
	  (opts:get-opts argv defined-options)
	 (opts:missing-arg (condition)
	   (format t "fatal: option ~s needs an argument~%"
		   (opts:option condition)))
	 (opts:arg-parser-failed (condition)
	   (format t "fatal: cannot parse ~s as argument of ~s~%"
		   (opts:raw-arg condition)
		   (opts:option condition)))
	 (opts:missing-required-option (con)
	   (format t "fatal: ~a~% is required" con)
	   (opts:exit 1)))
    (if (getf options :help)
	(progn
	  (opts:describe
	   :defined-options defined-options
	   :prefix *prefix*
	   :suffix *usage*
	   :usage-of "jackdaw-cli.lisp"
	   :args     (format nil "~{<~a>~^ ~}" (mapcar #'symbol-name positional-args)))
	   (opts:exit))
	(progn
	  (assert (>= (length free) (length positional-args)) ()
		  "You must provide exactly ~a positional arguments" (length positional-args))
	  (let ((positional))
	    (loop for name in positional-args for arg in free do
	      (push (intern (string-upcase name) :keyword) positional)
	      (push (read-into-jackdaw arg) positional))
	    (values (reverse positional) options))))))

(defun run-model (model-name options &key (argv (opts:argv)))
  ;;(format t "In run-model ~a with args: ~a" model-name argv)
  (assert (jackdaw::model-exists? model-name) ()
	  "\"~a\" is not a known model." model-name)
  (pop-options (output-vars)
      options
    (multiple-value-bind (positional optional)
	(if (getf options :read) (values nil nil)
	    (get-model-options model-name))
      (multiple-value-bind (parameters optional-parameters)
	  (parse-args argv positional (append *shared-options* optional))
	(pop-options (observe) optional-parameters
	  (let ((model (apply #'make-instance (jackdaw::find-model model-name)
			      (append parameters optional-parameters
				      (list :output t :output-vars output-vars)))))
	    (apply #'jackdaw::make-observable model observe)
	    (process-input model options)))))))

(defun run-global (&key (argv (opts:argv)))
  (multiple-value-bind (global remaining)
      (consume-options :argv argv :n-positional 1
		       :stop-after-last-positional t)
    ;;(format t "Global: ~a~%Remaining: ~a~%" global remaining)
    (multiple-value-bind (positional options)
	(parse-args global '(model))
      (pop-options (verbose debug model)
	  options
	(when model
	  (let ((*package* (find-package :jackdaw)))
	    (load model)))
;;            (with-open-file (s model :direction :input)
;;	      (with-standard-io-syntax
;;		(eval (read s))))))
	(when verbose
	  (format t "Verbose (this does not do anything yet)~%"))
	(if debug
	    (run-model (getf positional :model) options :argv remaining)
	    (handler-case 
		(run-model (getf positional :model) options :argv remaining)
	      (error (condition)
		(format *error-output* "Something went wrong.~%~a~%" condition))))))))
  

(defun process-input (model options)
  (pop-options (read freeze dataset no-log)
      options
    (unless (null read)
      (with-open-file (s read) (jackdaw::deserialize model s)))
    (let ((probabilities:*log-space* (not no-log)))
      (jackdaw::process-dataset model ;(retrieve-dataset dataset) t)
				(sort (retrieve-dataset dataset) #'string< :key #'car) t))
    (unless (null freeze)
      (with-open-file (s freeze :direction :output :if-exists :supersede
				:if-does-not-exist :create)
	(jackdaw::serialize model s)))))

(defun consume-options (&key
			  (argv (opts:argv))
			  (defined-options opts::*options*)
			  stop-after-last-positional
			  (n-positional 0))
  (do ((tokens (mapcan #'opts::split-short-opts
		       (mapcan #'opts::split-on-= (cdr argv)))
	       (if break tokens (cdr tokens)))
       consume-arg
       consumed
       break)
      ((or (null tokens) break (and stop-after-last-positional (eq n-positional 0)))
       (values (reverse consumed) tokens))
    (let ((item (car tokens)))
      (cond  ((or (and consume-arg (opts::argp item))
		  (opts::optionp item))
	      (if consume-arg
		  (setf consume-arg nil)
		  (when (opts::find-option item defined-options)
		    (when (opts::arg-parser (opts::find-option item defined-options))
		      (setf consume-arg t)))))
	     ((and (opts::argp item) (> n-positional 0))
	      (decf n-positional))
	     (t (setf break t)))
      (unless break
	(push item consumed)))))

(defun parse-token (token)
  "Parse a token ocurring in a CSV file into lisp datatype by 
READing it. If token is empty string, return NIL."
  (unless (string-equal token "")
    (read-into-jackdaw token)))

(defun tabular->sequences (rows)
  "Find all sequence UIDs, collect all events per sequence uid,
ensure events are consecutive, create rows consisting of event
sequences. Each event is a vector, the first elements of which is
UID, and subsequent elements correspond to (cddr columns)."
  (unless (equal (subseq (car rows) 0 2)
		 (list "sequence" "event"))
    (error "First two columns of CSV must be 'sequence' and 'event'."))
  (let* ((columns (cddr (car rows)))
	 (rows (cdr rows))
	 (max-indices (make-hash-table :test #'equal)))
    ;; Find sequence lengths
    (dolist (row rows)
      (let ((uid (car row)) (event-index (parse-integer (cadr row))))
	(when (> event-index (gethash uid max-indices 0))
	  (setf (gethash uid max-indices) event-index))))
    ;; Create sequences
    (let ((dataset (make-hash-table :test #'equal)))
      (dolist (row rows)
	(let* ((uid (car row))
	       (event-index (parse-integer (cadr row)))
	       (events (multiple-value-bind (e found?)
			   (gethash uid dataset (make-array (1+ (gethash uid max-indices))))
			 (unless found? (setf (gethash uid dataset) e))
			 e))
	       (event))
	  ;; Create a PLIST representaiton
	  (loop for token in (cddr row) for column in columns do
	       (setf (getf event (intern (string-upcase column) :keyword))
		     (parse-token token)))
	  ;; Store event in sequence vector
	  (setf (svref events event-index) event)))
      ;; Convert sequences to lists
       (loop for uid being the hash-keys of max-indices collect
	    (let ((events (map 'list #'identity (gethash uid dataset))))
	      (when (find 0 events)
		(warn "Some events not found in composition ~a" uid))
	      (cons uid events))))))

(defun retrieve-dataset (path)
  "Parse CSV data from a file at PATH or from *STANDARD-INPUT* if
PATH is NIL. Convert to sequences after reading."
  (let ((data
	  (fare-csv:with-rfc4180-csv-syntax ()
	    (if (null path)
		(fare-csv:read-csv-stream *standard-input*)
		(fare-csv:read-csv-file path)))))
    (tabular->sequences data)))
