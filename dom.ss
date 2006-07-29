;;; A layer on top of DOM for Jabber elements.

(module dom "swindle.ss"
  (require (lib "dom.ss" "dom"))
  (require (only (lib "dom-implementation.ss" "dom" "level-1" "core")
		 <dom-implementation-impl>))
  (require (only (lib "bootstrap.ss" "dom" "level-1" "xml") <xml-document>))
  (require (only (lib "element.ss" "dom" "level-2" "core") <element-ns>))
  (provide (all-defined))

  (defclass <jabber-dom-implementation> (<dom-implementation-impl>))

  (define *the-jabber-dom-implementation* (make <jabber-dom-implementation>))

  (defmethod (create-document (dom <jabber-dom-implementation>)
			      ns (qname <dom-string>) doctype)
    (make <jabber-document> :ns ns :qname qname :doctype doctype))

  (defclass <jabber-document> (<xml-document>))

  (defmethod (implementation (doc <jabber-document>))
    *the-jabber-dom-implementation*)

  (defclass <jabber-element> (<element-ns>))

  ;; Return the class for elements that have the given namespace URI
  ;; and local name.
  (defmethod (element-class (ns <dom-string>) (name <dom-string>))
    (element-class (as <symbol> ns) (as <symbol> name)))

  (defmethod (element-class (ns <symbol>) (name <symbol>))
    <jabber-element>)

  ;; Register a new class for elements that have the given namespace
  ;; URI and local name.  This will be used when creating an element
  ;; in a <jabber-document>.
  (defmethod (set-element-class! (ns <dom-string>) (name <dom-string>)
				 (class <class>))
    (defmethod (element-class (ns = (as <symbol> ns))
			      (name = (as <symbol> name)))
      class))


  (defmethod (create-element-ns (doc <jabber-document>)
				(ns <dom-string>) (qname <dom-string>))
    (make (element-class ns (local-name qname))
      :document doc :ns ns :name qname))


  ;; Define a subclass (or descendant) of <jabber-element> for
  ;; elements that have the given namespace URI and local name, as
  ;; well as a set of attribute getter and setter methods.
  (defsyntax defelementclass
    (syntax-rules ()
      ((_ (class-name super) ns name attr ...)
       (begin (define class-name
		(let ((class-name (class class-name (super))))
		  (set! (element-class ns name) class-name)
		  class-name))
	      (defattrs class-name attr ...)))
      ;; TO DO: initialize method
      ((_ class-name ns name attr ...)
       (defelementclass (class-name <jabber-element>) ns name attr ...))))

  (defsyntax defattrs
    (syntax-rules ()
      ((_ class attr ...)
       (begin (defattr attr class) ...))))

  ;; Define getter and setter methods for an element attribute.
  (defsyntax (defattr stx)
    (syntax-case stx ()
      ((_ (name ns) class)
       (let* ((name-sym (syntax-e #'name))
	      (name (symbol->string name-sym))
	      (set-name! (datum->syntax-object
			  #'name (symbol-append 'set- name-sym '!) #'name))
	      ((values prefix local-name) (parse-qname name)))
	 #`(begin
	     (defmethod (name (x class))
	       (attribute-ns x ns #,local-name))
	     (defmethod (#,set-name! (x class) value)
	       (set-attribute-ns! x ns #,name (as <dom-string> value)))
	     (defmethod (#,set-name! (x class) (value = #f))
	       (remove-attribute-ns! x ns #,name))
	     )))
      ((_ name . rest)
       #'(_ (name #f #f) . rest))))
)
