;;; A layer on top of DOM for Jabber elements.

(module dom "swindle.ss"
  (require (lib "dom.ss" "dom"))
  (require (only (lib "private.ss" "dom")
                 <dom-implementation-impl> <xml-document> <element-ns>))
  (require (only (lib "13.ss" "srfi") string-null?))
  (require-for-syntax (only (lib "13.ss" "srfi") string-index string-drop))
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
  ;; TO DO: make a metaclass!
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
    (define (local-name qname)
      (let ((index (string-index qname #\:)))
        (if index
            (string->immutable-string
             (string-drop qname (1+ index)))
            qname)))
    (syntax-case stx ()
      ((_ (name . opts) class)
       (let* ((name-sym (syntax-e #'name))
	      (name-str (string->immutable-string (symbol->string name-sym)))
	      (local-name-str (local-name name-str))
	      (set-name! (datum->syntax-object
			  #'name (symbol-append 'set- name-sym '!) #'name))
              (opts-list (syntax-object->datum #'opts))
              (type (getarg opts-list :type '<dom-string>))
              (ns (getarg opts-list :ns)))
	 #`(begin
	     (defmethod (name (x class))
	       (let ((val (attribute-ns x #,ns #,local-name-str)))
                 (and (not (string-null? val)) (as #,type val))))
	     (defmethod (#,set-name! (x class) value)
	       (set-attribute-ns! x #,ns #,name-str (as <dom-string> value)))
	     (defmethod (#,set-name! (x class) (value = #f))
	       (remove-attribute-ns! x #,ns #,name-str))
	     )))
      ((_ name . rest)
       #'(_ (name) . rest))))

  ;; Text fields are like attributes, but are stored as child elements.

  (defmethod (text-field-node (element <element>) (name <symbol>))
    (find-if-iterator
     (lambda (child)
       (and (instance-of? child <element>)
            ;; TO DO: check namespace
            (eq? name (as <symbol> (node-name child)))))
     (each-child element)))

  (defmethod (text-field (element <element>) (name <symbol>))
    (cond ((text-field-node element name)
           => text-content)
          (else #f)))

  (defmethod (set-text-field! (element <element>) (name <symbol>) value)
    (let ((node (or (text-field-node element name)
                    (append-child!/xexpr element `(,name)))))
      (set! (text-content node) (and value (as <dom-string> value)))))


  ;; Find the first child node of a particular type.
  (defmethod (first-child-instance (elt <jabber-element>) (type <class>))
    (find-if-iterator (lambda (child) (instance-of? child type))
                      (each-child elt)))

  ;; A list of children of a particular type.
  (defmethod (child-instances (elt <jabber-element>) (type <class>))
    (list-of child
      (child <- each-child elt)
      when (instance-of? child type)))
)
