(module test "swindle.ss"
  (require* "jabber.ss")
  (require* (lib "dom.ss" "dom"))
  (require (prefix xml: (lib "xml.ss" "xml")))
  (provide (all-from (lib "xml.ss" "xml")))
  (provide (all-defined))

  (define client #f)

  (define (test)
    (set! client (make <client> :address (as <jid> "dougo-test@volity.net")
		       :log? #t :debug? #t))
    )

)
