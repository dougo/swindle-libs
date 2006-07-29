(module element "../swindle.ss"
  (require "types.ss")
  (require "interfaces.ss")
  (require "exn.ss")
  (require (only "node.ss" allow-child))
  (require (only "named.ss" <named>))
  (require (only "owned.ss" <owned> check-ownership))
  (require (only "child.ss" <child>))
  (require (only "parent.ss" <parent>))
  (require (only "collections.ss" <named-node-hash-table> <node-list-impl>))
  (require (only "extra.ss" each-descendant next-pre-order parse-qname
                 normalize-children!))
  (require (only "namespaced.ss" <namespaced>))

  (defmethod (create-element (document <document>)
			     (tag-name <dom-string>))
    ;; TO DO: add default attributes
    (make <element-impl> :document document :name tag-name))

  (allow-child <element> <element> <text> <comment>)

  (defmethod (node-type (node <element>)) *element-node*)

  (defmethod (clone-node (node <element>) deep?)
    (create-element (owner-document node) (node-name node)))


  (defmethod (tag-name (element <element>)) (node-name element))

  (defmethod (attribute (element <element>) (name <dom-string>))
    (cond ((attribute-node element name) => value)
	  (else "")))

  (defmethod (attribute-node (element <element>) (name <dom-string>))
    (named-item (attributes element) name))

  (defmethod (elements-by-tag-name (root <element>) (name <dom-string>))
    (make <elements-by-tag-name> :root root :name name))

  (defmethod (normalize! (element <element>))
    (for-each-sequence normalize-children! (attributes element))
    (normalize-children! element))

  (defmethod (remove-attribute! (element <element>) (name <dom-string>))
    (when (named-item (attributes element) name)
      (remove-named-item! (attributes element) name))
    (void))

  (defmethod (remove-attribute-node! (element <element>) (old-attr <attr>))
    (remove-named-item! (attributes element) (node-name old-attr)))

  (defmethod (set-attribute! (element <element>) (name <dom-string>)
			     (value <dom-string>))
    (let ((attr (attribute-node element name)))
      (unless attr
	(set! attr (create-attribute (owner-document element) name))
	(set-attribute-node! element attr))
      (set! (value attr) value)))

  (defmethod (set-attribute-node! (element <element>) (new-attr <attr>))
    (let* ((name (node-name new-attr))
	   (old-attr (attribute-node element name)))
      (when old-attr (remove-attribute-node! element old-attr))
      (set-named-item! (attributes element) new-attr)
      old-attr))


  (defclass* <element-impl> (<named> <owned> <child> <parent> <element>)
    (attributes :reader attributes))
  (defmethod (initialize (element <element-impl>) initargs)
    (call-next-method)
    (slot-set! element 'attributes
	       (make <named-node-hash-table>
		 :container element :document (getarg initargs :document))))


  (defclass* <elements-by-tag-name> (<node-list-impl>)
    (root :reader root :initarg :root)
    (name :reader name :initarg :name))

  (defmethod (each-elt (elements <elements-by-tag-name>))
    ;; TO DO: abstract this into filter-iterator?
    (define (next-element element)
      (find-if-iterator
       (lambda (node) (matches? elements node))
       (each-descendant (next-pre-order element))))
    (list (next-element (root elements)) next-element not identity))

  ;; matching? is not a generic :(
  (defmethod* (matches? (matcher <elements-by-tag-name>) (value <node>)) #f)
  (defmethod (matches? (matcher <elements-by-tag-name>) (value <element>))
    (or (equals? (name matcher) "*")
	(equals? (name matcher) (tag-name value))))


  (defmethod (create-element-ns (document <document>)
				(namespace-uri = #f)
				(qualified-name <dom-string>))
    (create-element document qualified-name))

  (defmethod (create-element-ns (document <document>)
				(namespace-uri <dom-string>)
				(qualified-name <dom-string>))
    (make <element-ns>
      :document document :ns namespace-uri :name qualified-name))


  (defmethod (attribute-ns (element <element>) ns (name <dom-string>))
    (cond ((named-item-ns (attributes element) ns name) => value)
	  (else "")))

  (defmethod (attribute-node-ns (element <element>) ns (name <dom-string>))
    (named-item-ns (attributes element) ns name))

  (defmethod (elements-by-tag-name-ns (root <element>) ns (name <dom-string>))
    (make <elements-by-tag-name-ns> :root root :ns ns :name name))

  (defmethod (has-attribute? (element <element>) (name <dom-string>))
    (named-item (attributes element) name))

  (defmethod (has-attribute-ns? (element <element>) ns (name <dom-string>))
    (named-item-ns (attributes element) ns name))

  (defmethod (remove-attribute-ns! (element <element>) ns (name <dom-string>))
    (when (named-item-ns (attributes element) ns name)
      (remove-named-item-ns! (attributes element) ns name))
    (void))

  ;; TO DO: do we need to override remove-attribute-node! ?

  (defmethod (set-attribute-ns! (element <element>) ns (qname <dom-string>)
				(value <dom-string>))
    (let* (((values prefix name) (parse-qname qname))
	   (attr (attribute-node-ns element ns name)))
      (cond (attr
	     (set! (prefix attr) prefix))
	    (else
	     (set! attr (create-attribute-ns (owner-document element) ns qname))
	     (set-attribute-node-ns! element attr)))
      (set! (value attr) value)))

  ;; This is overridden for namespaced attributes in attr.ss.
  (defmethod (set-attribute-node-ns! (element <element>) (new-attr <attr>))
    (set-attribute-node! element new-attr))


  (defclass* <element-ns> (<namespaced> <element-impl>))

  (defmethod (clone-node (node <element-ns>) deep?)
    (create-element-ns (owner-document node)
		       (namespace-uri node) (node-name node)))


  (defclass* <elements-by-tag-name-ns> (<elements-by-tag-name>)
    (namespace-uri :reader namespace-uri :initarg :ns))

  (defmethod (matches? (matcher <elements-by-tag-name-ns>) (value <element>))
    (and (or (equals? (namespace-uri matcher) "*")
	     (equals? (namespace-uri matcher) (namespace-uri value)))
	 (or (equals? (name matcher) "*")
	     (equals? (name matcher) (local-name value)))))

)
