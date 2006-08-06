(module player "swindle.ss"
  (require "entity.ss")
  (provide (all-defined))

  (defclass <player> (<entity>))

  (defmethod (role (entity <player>))
    'player)
)
