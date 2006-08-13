(module node "../swindle.ss"
  (require "types.ss")
  (require "interfaces.ss")
  (require "exn.ss")
  (require (only "collections.ss" <node-list-impl> *the-empty-node-list*))
  (require (only "readonly.ss" readonly? check-readonly))
  (require (only "owned.ss" check-ownership))
  (require (only "extra.ss" normalize-children! has-child? each-ancestor))

  (defmethod (attributes (node <node>)) #f)
  (defmethod (child-nodes (node <node>)) *the-empty-node-list*)
  (defmethod (first-child (node <node>)) #f)
  (defmethod (last-child (node <node>)) #f)
  (defmethod (next-sibling (node <node>)) #f)
  (defmethod (node-value (node <node>)) #f)
  (defmethod (set-node-value! (node <node>) (value <dom-string>)) (void))
  (defmethod (owner-document (node <node>)) #f)
  (defmethod (parent-node (node <node>)) #f)
  (defmethod (previous-sibling (node <node>)) #f)
  (defmethod (text-content (node <node>)) #f)
  (defmethod (set-text-content! (node <node>) (value <dom-string>)) (void))


  ;; Common functionality for append-child! and insert-before!.  This
  ;; inserts new-child as a child of parent in between prev and next.
  ;; Either or both of prev and next might be #f to indicate the left
  ;; and right ends.  The function returns new-child.
  (defmethod* (add-child! (parent <node>) new-child prev next)
    (error 'add-child! "abstract method not implemented for ~v"
	   (class-of parent)))

  (defbeforemethod (add-child! (parent <node>) (new-child <node>) prev next)
    (check-allow-child parent new-child)
    (check-ownership parent new-child)
    (check-ancestry parent new-child)
    (check-readonly parent))

  (defmethod* (allow-child? (parent <node>) (child <node>)) #f)
  (defsubst* (allow-child parent-type child-type ...)
    (begin (defmethod (allow-child? (parent parent-type) (child child-type)) #t)
	   ...))

  (defmethod (append-child! (parent <node>) (new-child <node>))
    (add-child! parent new-child (last-child parent) #f))

  (defmethod* (check-allow-child (parent <node>) (child <node>))
    (unless (allow-child? parent child)
      (raise-exn:dom *hierarchy-request-err*
	"~v does not allow child ~v" parent child)))

  (defmethod* (check-ancestry (parent <node>) (child <node>))
    (do-iterator (ancestor (each-ancestor parent))
      (when (eq? ancestor child)
	(raise-exn:dom *hierarchy-request-err*
	  "~v is one of ~v's ancestors" parent child))))

  (defmethod* (check-has-child (parent <node>) (child <node>))
    (unless (has-child? parent child)
      (raise-exn:dom *not-found-err*
      "~v is not a child of ~v" child parent)))

  (defmethod (clone-node (node <node>) deep?)
    (raise-exn:dom *not-supported-err*
      "the implementation does not support cloning ~v"
      node))

  (defaroundmethod (clone-node (node <node>) deep?)
    (let* ((clone (call-next-method))
	   (attrs (attributes node)))
      (when attrs
	(do-sequence (attr attrs)
	  (set-attribute-node! clone (clone-node attr #t))))
      (when deep?
	(do-sequence (child (child-nodes node))
	  (append-child! clone (clone-node child #t))))
      clone))

  (defmethod (has-child-nodes? (node <node>)) #f)

  (defmethod (insert-before! (parent <node>)
			     (new-child <node>) (ref-child = #f))
    (append-child! parent new-child))

  (defmethod (insert-before! (parent <node>)
			     (new-child <node>) (ref-child <node>))
    (add-child! parent new-child (previous-sibling ref-child) ref-child))

  (defbeforemethod (insert-before! (parent <node>)
				   (new-child <node>) (ref-child <node>))
    (check-has-child parent ref-child))

  (defmethod (readonly? (x <node>))
    (let ((parent (parent-node x)))
      (and parent (readonly? parent))))

  (defmethod (replace-child! (parent <node>)
			     (new-child <node>) (old-child <node>))
    ;; TO DO: check error conditions for insert before removing old-child!
    (let ((ref-child (next-sibling old-child)))
      (remove-child! parent old-child)
      (insert-before! parent new-child ref-child))
    old-child)

  (defbeforemethod (remove-child! (parent <node>) (old-child <node>))
    (check-has-child parent old-child)
    (check-readonly parent))


  (defmethod (local-name (node <node>)) #f)
  (defmethod (namespace-uri (node <node>)) #f)
  (defmethod (prefix (node <node>)) #f)
  (defmethod (set-prefix! (node <node>) prefix)
    (raise-exn:dom *namespace-err*
      "~v has no namespace URI" node))
    

  (defmethod (has-attributes? (node <node>))
    (let ((attrs (attributes node)))
      (and attrs (positive? (len attrs)))))

  (defmethod (normalize! (node <node>))
    (normalize-children! node))

  (defmethod (supported? (node <node>) (feature <dom-string>) &opt version)
    (let ((doc (owner-document node)))
      (and doc (has-feature? (implementation doc) feature version))))
)
