(module test "swindle.ss"
  (require (all-except (lib "main.ss" "dom") <entity>))
  (provide (all-from (lib "main.ss" "dom")))
  (require* (lib "main.ss" "jabber"))
  (require* "volity.ss")
  (provide (all-defined))

  (define ronin #f)

  (define (test jid password)
    (set! ronin (make <bot> :address (as <jid> jid) :password password
                      :log? #t :debug? #t))
    )
)
