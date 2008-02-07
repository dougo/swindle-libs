;;; An XMPP client entity.

#lang s-exp "swindle.ss"

(require "error.ss")
(require "logging.ss")
(require "jid.ss")
(require "stream.ss")
(require "stanza.ss")
(provide (all-defined))

;; TO DO: I think some of this could be factored out into an
;; <entity> superclass (for <server> and <gateway>), but I haven't
;; thought very hard about that.

(defclass <client> (<stream-handler>)
  (address :type <jid> :initarg :address)
  (initial-stream :type <output-stream>)
  (response-stream :type <input-stream>)
  (tcp-port :type <integer> :initarg :tcp-port :initvalue 5222)
  (log? :type <boolean> :initarg :log? :initvalue #f)
  (debug? :type <boolean> :initarg :debug? :initvalue #f)
  (debug-raw? :type <boolean> :initarg :debug-raw? :initvalue #f)
  :autoaccessors :slot)

(defmethod (hostname (client <client>))
  (domain-id (address client)))

(defmethod (initialize (client <client>) initargs)
  (call-next-method)
  (unless (full? (address client))
    ;; Add a default resource ID.
    (set! (address client) (full (address client) "PLT")))
  (let* ((host (hostname client))
         ((values in out) (tcp-connect host (tcp-port client))))
    (when (debug-raw? client)
      ;; Copy the input stream to stderr.
      (set! in (log-input-port in)))
    (set! (initial-stream client)
          (make <output-stream> :port out :to host
                :log? (log? client) :debug? (debug? client)))
    (set! (response-stream client)
          (make <input-stream> :port in :handler client
                :log? (log? client) :debug? (debug? client)))))

(defmethod (send (client <client>) (stanza <stanza>))
  (send-element (initial-stream client) stanza))
(defmethod (send (client <client>) (error <stream-error>))
  (send-element (initial-stream client) error))

;; Handle an exception raised while reading a stanza from the response stream.
(defmethod (handle-stream-exn (client <client>) (exn <exn:fail>))
  (call-next-method)
  (send client (make-stream-error exn (initial-stream client)))
  (close (initial-stream client)))

;; Handle an exception raised while handling a stanza received on
;; the response stream.
(defmethod (handle-stanza-exn (client <client>) (exn <exn:fail>)
                              (stanza <stanza>))
  (call-next-method)
  (send client (make-error-stanza exn stanza (initial-stream client))))

;; Handle a stream error received on the response stream.
(defmethod (handle-element (client <client>) (error <stream-error>))
  ;; TO DO: user callback?
  (fprintf (current-error-port) "stream error ~a" (error-condition error))
  (when (error-text error)
    (fprintf (current-error-port) ": ~a" (error-text error)))
  (newline (current-error-port))
  (close (response-stream client))
  (close (initial-stream client)))

(defmethod (handle-close (client <client>))
  (call-next-method)
  (close (initial-stream client)))

(defmethod (shutdown (client <client>))
  (close (initial-stream client)))

