(module bot "swindle.ss"
  (require "entity.ss")
  (require "player.ss")
  (require (lib "jabber.ss" "jabber"))
  (provide (all-defined))

  (defclass <bot> (<player>))

  (defmethod (role (entity <bot>))
    'bot)
)
