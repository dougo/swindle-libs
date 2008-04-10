;;; Info/Query.

#lang s-exp "swindle.ss"

(require dom/swindle)
(require dom)
(require "client.ss")
(require "dom.ss")
(require "error.ss")
(require "exn.ss")
(require "stream.ss")
(require "stanza.ss")
(provide (all-defined))

;; Send an IQ request of type t (get or set) with payload p to a
;; target (the XMPP server by default) and wait for the response.
;; Return the response payload, or raise an exception if the
;; response is a stanza error.
(defmethod (iq (client <client>) (t <symbol>) (p <jabber-element>)
               &opt target)
  (let ((request (make-iq-request client t p target))
        (channel (make-channel)))
    (set! (iq-response-channel client (id request)) channel)
    (send client request)
    (let ((response (channel-get channel)))
      (when (eq? (type response) 'error)
        (raise-exn:xmpp:iq response))
      (payload response))))


(define *iq-response-channel-tables* (make-hash-table 'weak))

(defmethod (iq-response-channel-table (client <client>))
  (hash-table-get
   *iq-response-channel-tables* client
   (thunk (let ((table (make-hash-table)))
            (hash-table-put! *iq-response-channel-tables* client table)
            table))))

(defmethod (iq-response-channel (client <client>) (id <symbol>))
  (let* ((table (iq-response-channel-table client))
         (channel (hash-table-get (iq-response-channel-table client) id #f)))
    (when channel (hash-table-remove! table id))
    channel))

(defmethod (set-iq-response-channel! (client <client>) (id <symbol>) channel)
  (hash-table-put! (iq-response-channel-table client) id channel))


(defelementclass (<iq> <stanza>) *client-ns* "iq")

(defstruct <exn:xmpp:iq> (<exn:xmpp>) stanza)

(defmethod (raise-exn:xmpp:iq (response <iq>))
  (let* ((error (find-if-iterator
                 (lambda (node) (instance-of? node <stanza-error>))
                 (each-child response)))
         (text (error-text error))
         (condition (local-name (error-condition error))))
    (raise
     (make-exn:xmpp:iq
      (as <immutable-string>
          (echos :s- (if text condition 'iq-error) ": " (or text condition)))
      (current-continuation-marks)
      condition text iq))))


;; The payload of an IQ stanza is the first child element that is
;; not a stanza error.  (There should be at most one such child, but
;; this is not checked.)
(defmethod (payload (iq <iq>))
  (find-if-iterator
   (lambda (node)
     (and (instance-of? node <element>)
          (not (instance-of? node <stanza-error>))))
   (each-child iq)))

(defmethod (set-payload! (iq <iq>) (new-payload <jabber-element>))
  (let ((old-payload (payload iq)))
    (if old-payload
        (replace-child! iq new-payload old-payload)
        (insert-before! iq new-payload (first-child iq)))))


(defmethod (make-iq-request (client <client>) (type <symbol>)
                            (payload <jabber-element>) &opt to)
  (let ((iq (create-element-ns (document (initial-stream client))
                               *client-ns* "iq")))
    (set! (to iq) to
          (type iq) type
          (id iq) (gensym 'iq-)
          (payload iq) payload)
    iq))

;; Handle an IQ stanza received by a client.
(defmethod (handle-element (client <client>) (stanza <iq>))
  (let ((type (type stanza)))
    (case type
      ((get set)
       (handle-iq-request client (from stanza) (payload stanza)))
      ((result error)
       (handle-iq-response client stanza))
      (else
       (raise-exn:xmpp:stanza stanza 'bad-request "unrecognized type")))))

;; Handle an IQ request payload received by a client.  Override this
;; for specific IQ request classes.
(defmethod (handle-iq-request (client <client>) (from <string>)
                              (payload <jabber-element>))
  (raise-exn:xmpp:stanza (parent-node payload) 'service-unavailable))

;; Handle an IQ response recieved by a client by passing it on to
;; the requester thread.
(defmethod (handle-iq-response (client <client>) (iq <iq>))
  (channel-put (iq-response-channel client (id iq)) iq))

