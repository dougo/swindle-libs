(module document-fragment "../swindle.ss"
  (require "interfaces.ss")
  (require (only "owned.ss" <owned>))
  (require (only "parent.ss" <parent>))
  (require (only "text-content.ss" <text-container>))
  (require (only "node.ss" add-child! allow-child? allow-child))
  (require (only "extra.ss" child-list))

  (defmethod (create-document-fragment (document <document>))
    (make <document-fragment-impl> :document document))

  (defmethod (node-name (node <document-fragment>)) "#document-fragment")
  (defmethod (node-type (node <document-fragment>)) *document-fragment-node*)

  (defmethod (allow-child? (parent <parent>) (child <document-fragment>)) #t)
  (allow-child <document-fragment> <element> <comment> <text>)

  ;; Add each child of the fragment to the parent.
  (defmethod (add-child! (parent <parent>) (new-child <document-fragment>)
			 prev next)
    ;; TO DO: check that all child nodes are OK to add before starting!
    (dolist (fragment-child (child-list new-child))
      ;; TO DO: this could be optimized a bit, maybe-- we could skip
      ;; most of the error checking, and don't touch the internal
      ;; sibling connections.
      (insert-before! parent fragment-child next))
    new-child)

  (defmethod (clone-node (document-fragment <document-fragment>) deep?)
    (create-document-fragment (owner-document document-fragment)))


  (defclass* <document-fragment-impl>
      (<owned> <parent> <text-container> <document-fragment>))
)
