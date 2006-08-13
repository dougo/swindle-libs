(module entity "../swindle.ss"
  (require "../core/types.ss")
  (require "../core/interfaces.ss")
  (require "interfaces.ss")
  (require (only "../core/node.ss" allow-child))
  (require (only "../core/named.ss" <named>))
  (require (only "../core/owned.ss" <owned>))
  (require (only "../core/contained.ss" <contained>))
  (require (only "../core/parent.ss" <parent>))
  (require (only "../core/text-content.ss" <text-container>))

  (allow-child <entity>
	       <element> <processing-instruction> <comment> <text>
	       <cdata-section> <entity-reference>)

  (defmethod (node-type (node <entity>)) *entity-node*)

  ;; TO DO: import-node


  (defclass* <entity-impl>
      (<named> <owned> <contained> <parent> <text-container> <entity>)
    (public-id :reader public-id :initarg :public-id :initvalue #f)
    (system-id :reader system-id :initarg :system-id :initvalue #f)
    (notation-name :reader notation-name :initarg :notation :initvalue #f))
)
