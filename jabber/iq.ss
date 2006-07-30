;;; Info/Query.

(module iq "swindle.ss"
  (require "dom.ss")
  (require "stream.ss")
  (require "stanza.ss")
  (provide (all-defined))

  (defelementclass (<iq> <stanza>) *client-ns* "iq")

  ;; Handle an IQ stanza received by a client.
  (defmethod (handle-element (client <client>) (stanza <iq>))
    (let ((type (type stanza)))
      (case (and type (as <symbol> type))
	((get set)
	 (handle-iq-request client (payload stanza)))
	((result error)
	 (handle-iq-response client stanza))
	(else
	 (raise-stanza-error stanza 'bad-request "unrecognized type")))))

  ;; Handle an IQ request payload (child element) received by a
  ;; client.  Override this for specific IQ request classes.
  (defmethod (handle-iq-request (client <client>) (payload <jabber-element>))
    (raise-stanza-error (parent payload) 'service-unavailable))

  ;; Handle an IQ response recieved by a client by passing it on to
  ;; the requester thread.
  (defmethod (handle-iq-response (client <client>) (stanza <iq>))
    (void))
)
