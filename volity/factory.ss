;; Bot factories.

(module factory "swindle.ss"
  (require "entity.ss")
  (provide (all-defined))

  (defclass <factory> (<entity>))

  (defmethod (role (entity <factory>))
    'factory)
)
