(module processing-instruction "../swindle.ss"
  (require "../core/types.ss")
  (require "../core/interfaces.ss")
  (require "interfaces.ss")
  (require (only "bootstrap.ss" <xml-document>))
  (require (only "../core/node.ss" allow-child))
  (require (only "../core/readonly.ss" check-readonly))
  (require (only "../core/named.ss" <named>))
  (require (only "../core/owned.ss" <owned>))
  (require (only "../core/child.ss" <child>))

  (defmethod (create-processing-instruction (document <xml-document>)
					    (target <dom-string>)
					    (data <dom-string>))
    (make <processing-instruction-impl>
      :document document :name target :data data))

  (allow-child <document> <processing-instruction>)
  (allow-child <document-fragment> <processing-instruction>)
  (allow-child <element> <processing-instruction>)

  (defmethod (node-type (node <processing-instruction>))
    *processing-instruction-node*)
  (defmethod (node-value (node <processing-instruction>))
    (data node))
  (defmethod (set-node-value! (node <processing-instruction>)
			      (value <dom-string>))
    (set! (data node) value))

  (defmethod (clone-node (node <processing-instruction>) deep?)
    (create-processing-instruction (owner-document node)
				   (node-name node) (node-value node)))

  (defmethod (target (pi <processing-instruction>))
    (node-name pi))
  (defbeforemethod (set-data! (pi <processing-instruction>) (data <dom-string>))
    (check-readonly pi))

  (defclass* <processing-instruction-impl>
      (<named> <owned> <child> <processing-instruction>)
    (data :accessor data :initarg :data))
)
