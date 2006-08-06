(module test "swindle.ss"
  (require (all-except (lib "dom.ss" "dom") <entity>))
  (provide (all-from (lib "dom.ss" "dom")))
  (require* (lib "jabber.ss" "jabber"))
  (require* "volity.ss")
  (provide (all-defined))

  (define ronin #f)

  (define (test jid password)
    (set! ronin (make <bot>  :address (as <jid> jid) :log? #t :debug? #t))
    (login ronin password)
    (become-available ronin)
    )
)
