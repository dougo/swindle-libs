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
(require net/url)

(defmethod (create-ls-parser (dom <dom-implementation-ls>)
			     (mode <dom-implementation-ls-mode>)
			     &opt schema-type)
  (unless (= mode *mode-synchronous*)
    (raise-exn:dom *not-supported-err*
      "create-ls-parser: unsupported mode: ~a" mode))
  (when schema-type
    (raise-exn:dom *not-supported-err*
      "create-ls-parser: unsupported schema-type: ~a" schema-type))
  (make <ls-parser-impl>))

(defclass* <ls-parser-impl> (<ls-parser>)
  (config :initvalue #f)
  (filter :initvalue #f)
  (busy-thread :initvalue #f)
  :autoaccessors :slot)

(defmethod (busy? (parser <ls-parser-impl>))
  (as <boolean> (busy-thread parser)))

(defmethod (parse (parser <ls-parser-impl>) (input <ls-input>))
  (when (busy? parser)
    (raise-exn:dom *invalid-state-err* "parse: parser is busy: ~a" parser))
  (set-busy-thread! parser (current-thread))
  (begin0 (read-dom *the-dom-implementation*
		    (cond ((character-stream input) => identity)
			  ((byte-stream input) => identity)
			  ((string-data input) => open-input-string)
			  ((system-id input) =>
			   (lambda (system-id)
			     (get-pure-port
			      (combine-url/relative
			       (string->url (or (base-uri input) ""))
			       system-id))))
			  (else
			   ;; TO DO: raise ls-exception
			   (raise-exn:dom *not-supported-err*
			     "parse: unsupported <ls-input>: ~a" input))))
    (set-busy-thread! parser #f)))
