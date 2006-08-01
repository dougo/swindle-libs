(module error "swindle.ss"
  (require (lib "dom.ss" "dom"))
  (require "dom.ss")
  (require "stream.ss")
  (require "exn.ss")
  (provide (all-defined))

  (defclass <jabber-error> (<jabber-element>))
  (defelementclass (<stream-error> <jabber-error>) *streams-ns* "error")
  (defelementclass (<stanza-error> <jabber-error>) *client-ns* "error")
  
  (define *stream-error-ns* "urn:ietf:params:xml:ns:xmpp-streams")
  (define *stanza-error-ns* "urn:ietf:params:xml:ns:xmpp-stanzas")

  (defclass <error-text> (<jabber-element>))
  (defelementclass (<stream-error-text> <error-text>) *stream-error-ns* "text")
  (defelementclass (<stanza-error-text> <error-text>) *stanza-error-ns* "text")

  (defclass <error-condition> (<jabber-element>))
  (defsyntax (deferrorcondition stx)
    (syntax-case stx ()
      ((_ name)
       (with-syntax ((class-name
		      (datum->syntax-object
		       #'name (symbol-append '< (syntax-e #'name) '>)  #'name))
		     (condition
		      (datum->syntax-object
		       #'name (symbol->string (syntax-e #'name)) #'name)))
	 #'(defelementclass (class-name <error-condition>)
	     *stream-error-ns* condition)))))

  (deferrorcondition bad-format)
  (deferrorcondition bad-namespace-prefix)
  (deferrorcondition conflict)
  (deferrorcondition connection-timeout)
  (deferrorcondition host-gone)
  (deferrorcondition host-unknown)
  (deferrorcondition improper-addressing)
  (deferrorcondition internal-server-error)
  (deferrorcondition invalid-from)
  (deferrorcondition invalid-id)
  (deferrorcondition invalid-namespace)
  (deferrorcondition invalid-xml)
  (deferrorcondition not-authorized)
  (deferrorcondition policy-violation)
  (deferrorcondition remote-connection-failed)
  (deferrorcondition resource-constraint)
  (deferrorcondition restricted-xml)
  (deferrorcondition see-other-host)
  (deferrorcondition system-shutdown)
  (deferrorcondition undefined-condition)
  (deferrorcondition unsupported-encoding)
  (deferrorcondition unsupported-stanza-type)
  (deferrorcondition unsupported-version)
  (deferrorcondition xml-not-well-formed)

  ;; TO DO: stanza error conditions

  (defmethod (error-condition (error <jabber-error>))
    (first-child error))

  (defmethod (error-text (error <jabber-error>))
    (let ((node (error-text-node)))
      ;; TO DO: factor into text-content (DOM Level 3)
      (and node (data (first-child node)))))

  (defmethod (error-text-node (error <jabber-error>))
    (next-sibling (first-child error)))
  ;; TO DO: xml:lang

  (defmethod (make-stream-error (exn <exn:fail>) (stream <xml-stream>))
    (let* ((doc (document stream))
	   (prefix (lookup-prefix doc *streams-ns*))
	   (error (create-element-ns doc *streams-ns*
				     (make-qname prefix "error")))
	   (condition (condition exn))
	   (text (text exn)))
      (set-attribute-ns!
       (append-child! error (create-element-ns doc *stream-error-ns*
                                               (as <dom-string> condition)))
       *xmlns-ns* "xmlns" *stream-error-ns*)
      (when text
	(let ((text-elt (create-element-ns doc *stream-error-ns* "text")))
	  (append-child! error text-elt)
	  (set-attribute-ns! text-elt *xmlns-ns* "xmlns" *stream-error-ns*)
	  ;; TO DO: xml:lang
	  (append-child! text-elt (create-text-node doc text))))
      error))

  ;; TO DO: refactor these!
  (defmethod (make-stanza-error (exn <exn:fail>) (stream <xml-stream>))
    (let* ((doc (document stream))
	   (prefix (lookup-prefix doc *client-ns*))
	   (error (create-element-ns doc *client-ns*
				     (make-qname prefix "error")))
	   (condition (condition exn))
	   (text (text exn)))
      (set-attribute-ns!
       (append-child! error (create-element-ns doc *stanza-error-ns*
                                               (as <dom-string> condition)))
       *xmlns-ns* "xmlns" *stanza-error-ns*)
      (when text
	(let ((text-elt (create-element-ns doc *stanza-error-ns* "text")))
	  (append-child! error text-elt)
	  (set-attribute-ns! text-elt *xmlns-ns* "xmlns" *stanza-error-ns*)
	  ;; TO DO: xml:lang
	  (append-child! text-elt (create-text-node doc text))))
      error))
)
