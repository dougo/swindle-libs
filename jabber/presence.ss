;; XMPP presence stanzas.

(module presence "swindle.ss"
  (require "client.ss")
  (require "dom.ss")
  (require "stream.ss")
  (require "stanza.ss")
  (require (lib "dom.ss" "dom"))
  (provide (all-defined))

  (defmethod (become-available (client <client>) &key to show status priority)
    (send client (make-presence client :to to :show show :status status
                                :priority priority)))

  (defmethod (become-idle (client <client>) &key to (show 'away) status)
    (send client (make-presence client :to to :show show :status status)))

  (defmethod (become-unavailable (client <client>) &key to)
    (send client (make-presence client :to to :type 'unavailable)))


  (defelementclass (<presence> <stanza>) *client-ns* "presence")

  (defmethod (make-presence (client <client>) &key to type show status priority)
    (let ((presence (create-element-ns (document (initial-stream client))
                                       *client-ns* "presence")))
      (set! (to presence) to
            (type presence) type
            (text-field presence 'show) show
            (text-field presence 'status) status
            (text-field presence 'priority) priority)
      presence))

  (defmethod (handle-element (client <client>) (stanza <presence>))
    (when (eq? (type stanza) 'subscribe)
      (handle-subscription-request client stanza))
    ;; TO DO: other handlers, e.g. MUC roster
    )

  (defmethod (handle-subscription-request (client <client>) (req <presence>))
    ;; Accept all subscription requests.
    (send client (make-presence client :to (from req) :type 'subscribed)))
)
