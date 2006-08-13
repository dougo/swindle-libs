(module attr "../swindle.ss"
  (require "types.ss")
  (require "interfaces.ss")
  (require "exn.ss")
  (require (only "node.ss" allow-child))
  (require (only "named.ss" <named>))
  (require (only "owned.ss" <owned>))
  (require (only "contained.ss" <contained> container))
  (require (only "parent.ss" <parent>))
  (require (only "text-content.ss" <text-container>))
  (require (only "collections.ss" <named-node-map-impl>))
  (require (only "namespaced.ss" <namespaced>))

  (defmethod (create-attribute (document <document>) (name <dom-string>))
    (make <attr-impl> :document document :name name))

  (allow-child <attr> <text>)

  (defmethod (node-type (node <attr>)) *attribute-node*)
  (defmethod (node-value (node <attr>)) (value node))
  (defmethod (set-node-value! (node <attr>) (value <dom-string>))
    (set! (value node) value))

  (defmethod (clone-node (node <attr>) deep?)
    (create-attribute (owner-document node) (node-name node)))

  (defaroundmethod (clone-node (node <attr>) deep?)
    ;; Attributes always clone their children.
    (call-next-method node #t))

  (defmethod (name (attr <attr>)) (node-name attr))
  (defmethod (specified? (attr <attr>)) #t)
  (defmethod (value (attr <attr>)) (text-content attr))
  (defmethod (set-value! (attr <attr>) (value <dom-string>))
    (set! (text-content attr) value))

  (defclass* <attr-impl>
      (<named> <owned> <contained> <parent> <text-container> <attr>))

  (defbeforemethod (set-keyed-item! (nodes <named-node-map-impl>)
				    (new-attr <attr-impl>))
    (let ((old-nodes (container new-attr)))
      (when (and old-nodes (not (eq? old-nodes nodes)))
	(raise-exn:dom *inuse-attribute-err*
	  "~v is already an attribute of ~v"
	  new-attr (container old-nodes)))))

  (defclass* <specified-attr> (<attr-impl>))

  (defmethod (specified? <specified-attr>) #f)


  (defmethod (create-attribute-ns (document <document>)
				  (namespace-uri = #f)
				  (qualified-name <dom-string>))
    (create-attribute document qualified-name))

  (defmethod (create-attribute-ns (document <document>)
				  (namespace-uri <dom-string>)
				  (qualified-name <dom-string>))
    (make <attr-ns> :document document :ns namespace-uri :name qualified-name))


  (defmethod (owner-document (attr <attr>))
    (let ((nodes (container attr)))
      (and nodes (container nodes))))


  (defclass* <attr-ns> (<namespaced> <attr-impl>))

  (defmethod (clone-node (node <attr-ns>) deep?)
    (create-attribute-ns (owner-document node)
			 (namespace-uri node) (node-name node)))

  (defmethod (set-attribute-node! (element <element>) (new-attr <attr-ns>))
    (set-attribute-node-ns! element new-attr))

  (defmethod (set-attribute-node-ns! (element <element>) (new-attr <attr-ns>))
    (let* ((name (local-name new-attr))
	   (ns (namespace-uri new-attr))
	   (old-attr (attribute-node-ns element ns name)))
      (when old-attr (remove-attribute-node! element old-attr))
      (set-named-item-ns! (attributes element) new-attr)
      old-attr))
)
