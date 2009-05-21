#lang swindle

(require "../swindle.ss")
(require "../core/types.ss")
(require "../core/interfaces.ss")
(require "../core/exn.ss")
(require (only "../core/dom-implementation.ss" *the-dom-implementation*))
(require (only "../convert.ss" read-dom))
(require "../xml/interfaces.ss")
(require "types.ss")
(require "interfaces.ss")
(require (only "ls-input.ss" input-port))

(defmethod (async? (parser <ls-parser>))
  #f)

(defbeforemethod (parse (parser <ls-parser>) (input <ls-input>))
  (when (busy? parser)
    (raise-exn:dom *invalid-state-err* "parse: parser is busy: ~a" parser)))

(defbeforemethod (parse-with-context
		  (parser <ls-parser>) (input <ls-input>)
		  (context-arg <node>) (action <exact-integer>))
  (when (busy? parser)
    (raise-exn:dom *invalid-state-err*
      "parse-with-context: parser is busy: ~a" parser)))

(defmethod (parse-with-context (parser <ls-parser>) (input <ls-input>)
			       (context-arg <node>) (action <action-type>))
  (raise-exn:dom *not-supported-err*
    "parse-with-context: not supported by parser: ~a" parser))


(defbeforemethod (create-ls-parser (dom <dom-implementation-ls>)
				   (mode <dom-implementation-ls-mode>)
				   &opt schema-type)
  (unless (= mode *mode-synchronous*)
    (raise-exn:dom *not-supported-err*
      "create-ls-parser: unsupported mode: ~a" mode))
  (when schema-type
    (raise-exn:dom *not-supported-err*
      "create-ls-parser: unsupported schema-type: ~a" schema-type)))

(defmethod (create-ls-parser (dom <dom-implementation-ls>)
			     (mode <dom-implementation-ls-mode>)
			     &opt schema-type)
  (make <ls-parser-impl>))

(defclass* <ls-parser-impl> (<ls-parser>)
  (config :initvalue #f)
  (filter :initvalue #f)
  (busy-thread :initvalue #f)
  :autoaccessors :slot)

(defmethod (busy? (parser <ls-parser-impl>))
  (not (not (busy-thread parser))))

(defmethod (abort (parser <ls-parser-impl>))
  (cond ((busy-thread parser) => kill-thread))
  (set! (busy-thread parser) #f))

(defmethod (parse (parser <ls-parser-impl>) (input <ls-input>))
  (set-busy-thread! parser (current-thread))
  (with-handlers ((void (lambda (e) (set-busy-thread! parser #f) (raise e))))
    (begin0 (read-dom *the-dom-implementation*
		      (or (input-port input)
			  ;; TO DO: raise ls-exception
			  (raise-exn:dom *not-supported-err*
			    "parse: unsupported <ls-input>: ~a" input)))
      (set-busy-thread! parser #f))))

(defmethod (parse-uri (parser <ls-parser-impl>) (uri <dom-string>))
  (let ((input (create-ls-input *the-dom-implementation*)))
    (set! (system-id input) uri)
    (parse parser input)))
