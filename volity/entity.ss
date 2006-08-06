(module entity "swindle.ss"
  (require (lib "jabber.ss" "jabber"))
  (provide (all-defined))

  (defclass <entity> (<client>))

  (defmethod (type-uri (entity <entity>))
    "http://volity.org/protocol/caps")
  (defmethod (version (entity <entity>))
    "1.0")
  (defmethod (feature-bundles (entity <entity>))
    (list (role entity)))

  (defgeneric (role (entity <entity>)))
)
