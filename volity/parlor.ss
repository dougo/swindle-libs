(module parlor "swindle.ss"
  (require "entity.ss")
  (provide (all-defined))

  (defclass <parlor> (<entity>))

  (defmethod (role (entity <parlor>))
    'parlor)
)
