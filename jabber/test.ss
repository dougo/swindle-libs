(module test "swindle.ss"
  (require* "jabber.ss")
  (require* (lib "dom.ss" "dom"))
  (require (prefix xml: (lib "xml.ss" "xml")))
  (provide (all-from (lib "xml.ss" "xml")))
  (provide (all-defined))

  (define test-client #f)
  (define test-muc #f)

  (define (test jid password service)
    (set! test-client (make <client> :address (as <jid> jid)
                            :log? #t :debug? #t))
    (login test-client password)
    (keep-alive test-client)
    (become-available test-client)
    (set! test-muc (make <muc> :client test-client :room-id "swindle"
                         :service service))
    (enter test-muc)
    (exit test-muc)
    (enter test-muc)
    (set! (nickname test-muc) "swindle-test")
    )

)
