#lang swindle

(require "../swindle.ss")
(require "types.ss")
(require "interfaces.ss")
(require "exn.ss")
(require (only "extra.ss" each-descendant))
(require (only "node.ss" allow-child allow-child?))
(require (only "parent.ss" <parent>))
(require (only "dom-implementation.ss" *the-dom-implementation*))
(require (only "element.ss" <elements-by-tag-name> <elements-by-tag-name-ns>))
(require (only "exn.ss" raise-exn:dom))
(require (only "owned.ss" <owned> set-owner-document!))

(defmethod (allow-child? (parent <document>) (child <element>))
  (not (document-element parent)))
(allow-child <document> <comment>)

(defmethod (node-name (node <document>)) "document")
(defmethod (node-type (node <document>)) *document-node*)
(defmethod (doctype (node <document>)) #f)
(defmethod (document-element (document <document>))
  (find-if-sequence (lambda (node) (instance-of? node <element>))
                    (child-nodes document)))
(defmethod (implementation (document <document>)) *the-dom-implementation*)

(defmethod (create-cdata-section (document <document>) (data <dom-string>))
  (raise-exn:dom *not-supported-err*
    "~v does not support CDATA sections" document))

(defmethod (create-entity-reference (document <document>) (name <dom-string>))
  (raise-exn:dom *not-supported-err*
    "~v does not support entity references" document))

(defmethod (create-processing-instruction (document <document>)
                                          (target <dom-string>)
                                          (data <dom-string>))
  (raise-exn:dom *not-supported-err*
    "~v does not support processing instructions" document))

(defmethod (elements-by-tag-name (root <document>) (name <dom-string>))
  (make <elements-by-tag-name> :root root :name name))


;; TO DO: clone-node

(defmethod (element-by-id (document <document>) (element-id <dom-string>))
  #f)

(defmethod (elements-by-tag-name-ns (root <document>) ns (name <dom-string>))
  (make <elements-by-tag-name-ns> :root root :ns ns :name name))

(defmethod (import-node (document <document>) (imported-node <node>) deep?)
  (raise-exn:dom *not-supported-err*
    "~v cannot be imported" imported-node))

(defmethod (import-node (document <document>) (imported-node <owned>) deep?)
  (let ((node (clone-node imported-node deep?)))
    (import-node! document node)
    node))

(defmethod* (import-node! (document <document>) (imported-node <owned>))
  (let ((attrs (attributes imported-node)))
    (when attrs (do-sequence (attr attrs) (import-node! document attr))))
  (do-iterator (node (each-descendant imported-node))
    (set-owner-document! node document)))


(defclass* <document-impl> (<parent> <document>))
