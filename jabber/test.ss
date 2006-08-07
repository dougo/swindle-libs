(module test "swindle.ss"
  (require* "jabber.ss")
  (require* (lib "dom.ss" "dom"))
  (require (prefix xml: (lib "xml.ss" "xml")))
  (provide (all-from (lib "xml.ss" "xml")))
  (provide (all-defined))

  (define client #f)

  (define (test jid password)
    (set! client (make <client> :address (as <jid> jid) :log? #t :debug? #t))
    (login client password)
    (keep-alive client)
    (become-available client)
    )

)
