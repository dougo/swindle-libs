(module referee "swindle.ss"
  (require "entity.ss")
  (provide (all-defined))

  (defclass <referee> (<entity>))

  (defmethod (role (entity <referee>))
    'referee)
)
