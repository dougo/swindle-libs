(module bot "swindle.ss"
  (require "entity.ss")
  (require "player.ss")
  (require (lib "main.ss" "jabber"))
  (provide (all-defined))

  (defclass <bot> (<player>))

  (defmethod (role (entity <bot>))
    'bot)
)
