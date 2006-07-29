(module document-type "../swindle.ss"
  (require "../core/types.ss")
  (require "../core/interfaces.ss")
  (require "interfaces.ss")
  (require (only "../core/node.ss" allow-child? add-child!))
  (require (only "../core/readonly.ss" readonly?)) 
  (require (only "../core/named.ss" <named>))
  (require (only "../core/owned.ss" <owned> set-owner-document!))
  (require (only "../core/child.ss" <child>))
  (require (only "../core/collections.ss" <named-node-hash-table>))
  (require (only "../core/exn.ss" raise-exn:dom))
  (require (only "../core/namespaced.ss" check-qname))

  (defaftermethod (add-child! (parent <document>) (child <document-type>)
			      prev next)
    (set-owner-document! child parent))

  (defmethod (allow-child? (parent <document>) (child <document-type>))
    (not (doctype parent)))

  (defmethod (doctype (document <document>))
    (find-if-sequence (lambda (node) (instance-of? node <document-type>))
		      (child-nodes document)))

  (defaftermethod (remove-child! (parent <document>) (child <document-type>))
    (set-owner-document! child #f))


  (defmethod (node-type (node <document-type>)) *document-type-node*)

  (defmethod (readonly? (x <document-type>)) #t)

  (defmethod (name (doctype <document-type>)) (node-name doctype))


  (defclass* <document-type-impl> (<named> <owned> <child> <document-type>)
    (entities :reader entities)
    (notations :reader notations))
  (defmethod (initialize (doctype <document-type-impl>) initargs)
    (call-next-method)
    (let ((document (getarg initargs :document)))
      (slot-set! doctype 'entities (make <named-node-hash-table>
				     :container doctype :document document))
      (slot-set! doctype 'notations (make <named-node-hash-table>
				      :container doctype :document document))))


  (defmethod (import-node (document <document>)
			  (imported-node <document-type>) deep?)
    (raise-exn:dom *not-supported-err*
      "~v cannot be imported" imported-node))

  (defmethod (create-document-type (dom <dom-implementation>)
				   (qualified-name <dom-string>)
				   public-id system-id)
    (make <xml-document-type>
      :name qualified-name
      :public-id public-id
      :system-id system-id))

  (defbeforemethod (create-document-type (dom <dom-implementation>)
					 (qualified-name <dom-string>)
					 public-id system-id)
    (check-qname qualified-name))


  (defclass* <xml-document-type> (<document-type-impl>)
    ;; TO DO: factor these from <entity-impl>
    (public-id :reader public-id :initarg :public-id :initvalue #f)
    (system-id :reader system-id :initarg :system-id :initvalue #f)
    (internal-subset :reader internal-subset
		     :initarg :internal-subset :initvalue #f))
)
