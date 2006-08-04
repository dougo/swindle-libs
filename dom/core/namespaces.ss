(module namespaces "../swindle.ss"
  (require "types.ss")
  (require "interfaces.ss")
  (require (only (lib "13.ss" "srfi") string-null?))
  (require (only "extra.ss" ancestor-element *xmlns-ns*))

  ;; B.2 Namespace Prefix Lookup
  (defmethod (lookup-prefix (node <node>) (namespace-uri = #f)) #f)

  (defmethod (lookup-prefix (element <element>) (namespace-uri <dom-string>))
    (lookup-namespace-prefix element namespace-uri element))

  (defmethod (lookup-prefix (doc <document>) (namespace-uri <dom-string>))
    (lookup-prefix (document-element doc) namespace-uri))

  (defmethod (lookup-prefix (attr <attr>) (namespace-uri <dom-string>))
    (let ((owner (owner-element attr)))
      (and owner (lookup-prefix owner namespace-uri))))

  (defmethod (lookup-prefix (node <node>) (namespace-uri <dom-string>))
    (let ((ancestor (ancestor-element node)))
      (and ancestor (lookup-prefix ancestor namespace-uri))))


  (defmethod* (lookup-namespace-prefix (element <element>) (ns-uri <dom-string>)
				       (original-element <element>))
    (let/ec return
      (when (string-null? ns-uri) (return #f))
      (let ((prefix (prefix element)))
	(when (and (equals? (namespace-uri element) ns-uri)
		   prefix
		   (equals? (lookup-namespace-uri original-element prefix)
			    ns-uri))
	  (return prefix)))
      (when (has-attributes? element)
	(loop-for (attr <- each-elt (attributes element))
	  (when (and (equals? (prefix attr) "xmlns")
		     (equals? (value attr) ns-uri)
		     (equals? (lookup-namespace-uri original-element
						    (local-name attr))
			      ns-uri))
	    (return (local-name attr)))))
      (let ((ancestor (ancestor-element element)))
	(and ancestor (lookup-namespace-prefix ancestor ns-uri
					       original-element)))))
		 

  ;; B.3 Default Namespace Lookup
  (defmethod (default-namespace? (element <element>) (ns-uri <dom-string>))
    (let/ec return
      (unless (prefix element)
	(return (equals? (namespace-uri element) ns-uri)))
      (let ((attr (attribute-ns element *xmlns-ns* "xmlns")))
	(when attr
	  (return (equals? (value attr) ns-uri))))
      (call-next-method)))

  (defmethod (default-namespace? (doc <document>) (ns-uri <dom-string>))
    (default-namespace? (document-element doc) ns-uri))

  (defmethod (default-namespace? (attr <attr>) (ns-uri <dom-string>))
    (let ((owner (owner-element attr)))
      (and owner (default-namespace? owner ns-uri))))

  (defmethod (default-namespace? (node <node>) (ns-uri <dom-string>))
    (let ((ancestor (ancestor-element node)))
      (and ancestor-element (default-namespace? ancestor-element ns-uri))))
    

  ;; B.4 Namespace URI Lookup
  (defmethod (lookup-namespace-uri (element <element>) prefx)
    (let/ec return
      (let ((ns-uri (namespace-uri element)))
	(when (and ns-uri (equals? (prefix element) prefx))
	  ;; Note: prefix could be #f, in which case we are looking for the
	  ;; default namespace.
	  (return ns-uri)))
      (when (has-attributes? element)
	(loop-for (attr <- each-elt (attributes element))
	  (cond ((and (equals? (prefix attr) "xmlns")
		      (equals? (local-name attr) prefx))
		 ;; non default namespace
		 (return (value attr)))
                ((and (equals? (local-name attr) "xmlns")
		      (not prefx))
		 ;; default namespace
		 (return (value attr))))))
      (call-next-method)))

  (defmethod (lookup-namespace-uri (doc <document>) prefix)
    (let ((element (document-element doc)))
      (and element (lookup-namespace-uri element prefix))))

  (defmethod (lookup-namespace-uri (attr <attr>) prefix)
    (let ((owner (owner-element attr)))
      (and owner (lookup-namespace-uri owner prefix))))

  (defmethod (lookup-namespace-uri (node <node>) prefix)
    (let ((ancestor (ancestor-element node)))
      (and ancestor (lookup-namespace-uri ancestor prefix))))
)
