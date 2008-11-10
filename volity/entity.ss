(module entity "swindle.ss"
  (require (lib "main.ss" "jabber"))
  (provide (all-defined))

  (defclass <entity> (<client>)
    (password :type <string> :initarg :password)
    :autoaccessors :slot)

  (defmethod (initialize (entity <entity>) initargs)
    (call-next-method)
    (login entity (password entity))
    (keep-alive entity)
    (become-available entity))

  (defmethod (type-uri (entity <entity>))
    "http://volity.org/protocol/caps")
  (defmethod (version (entity <entity>))
    "1.0")
  (defmethod (feature-bundles (entity <entity>))
    (list (role entity)))

  (defgeneric (role (entity <entity>)))
)
