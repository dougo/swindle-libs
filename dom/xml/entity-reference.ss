(module entity-reference "../swindle.ss"
  (require "../core/types.ss")
  (require "../core/interfaces.ss")
  (require "interfaces.ss")
  (require (only "bootstrap.ss" <xml-document>))
  (require (only "../core/node.ss" allow-child))
  (require (only "../core/readonly.ss" readonly?))
  (require (only "../core/named.ss" <named>))
  (require (only "../core/owned.ss" <owned>))
  (require (only "../core/child.ss" <child>))
  (require (only "../core/parent.ss" <parent>))
  (require (only "../core/text-content.ss" <text-container>))

  (defmethod (create-entity-reference (document <xml-document>)
				      (name <dom-string>))
    ;; TO DO: if the entity exists, copy its subtree
    (make <entity-reference-impl> :document document :name name))

  (allow-child <element> <entity-reference>)
  (allow-child <entity-reference>
	       <element> <processing-instruction> <comment> <text>
	       <cdata-section> <entity-reference>)

  (defmethod (node-type (node <entity-reference>)) *entity-reference-node*)

  (defmethod (clone-node (node <entity-reference>) deep?)
    (create-entity-reference (owner-document node) (node-name node)))

  (defmethod (readonly? (x <entity-reference>)) #t)


  (defclass* <entity-reference-impl>
      (<named> <owned> <child> <parent> <text-container> <entity-reference>))
)
