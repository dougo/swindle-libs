#lang swindle

(require "../swindle.ss")
(require "../core/interfaces.ss")
(require "../xml/interfaces.ss")
(require "interfaces.ss")
(require net/url)

(defclass* <ls-input-impl> (<ls-input>)
  (character-stream :initvalue #f)
  (byte-stream :initvalue #f)
  (string-data :initvalue #f)
  (system-id :initvalue #f)
  (public-id :initvalue #f)
  (base-uri :initvalue #f)
  (encoding :initvalue #f)
  (certified-text? :initvalue #f)
  :autoaccessors :slot)

(defmethod (create-ls-input (dom <dom-implementation-ls>))
  (make <ls-input-impl>))

(defmethod* (input-port (input <ls-input>))
  (cond ((character-stream input) => identity)
	((byte-stream input) => identity)
	((string-data input) => open-input-string)
	((system-id input) =>
	 (lambda (system-id)
	   (get-pure-port
	    (combine-url/relative
	     (string->url (or (base-uri input) ""))
	     system-id))))
	(else #f)))
