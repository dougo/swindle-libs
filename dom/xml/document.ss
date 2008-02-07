#lang swindle

(require "../swindle.ss")
(require "../core/core.ss")
(require "interfaces.ss")
(require (only "bootstrap.ss" <xml-document>))

(defmethod (create-document (dom <dom-implementation>)
                            namespace-uri (qualified-name <dom-string>)
                            doctype)
  (make <xml-document> :ns namespace-uri :qname qualified-name
        :doctype doctype))

(defmethod (initialize (doc <document>) initargs)
  (call-next-method)
  (let ((doctype (getarg initargs :doctype))
        (ns (getarg initargs :ns))
        (qname (getarg initargs :qname)))
    (when doctype (append-child! doc doctype))
    (append-child! doc (create-element-ns doc ns qname))))

(defaroundmethod (import-node (document <document>)
                              (import-node <entity-reference>) deep?)
  (call-next-method document import-node #f))

