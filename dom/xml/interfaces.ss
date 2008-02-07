#lang swindle

(require "../swindle.ss")
(require "../core/types.ss")
(require "../core/interfaces.ss")
(provide (all-defined))

;; Proprietary way of bootstrapping <dom-implementation>.
(defgeneric (make-xml-dom-implementation))

;; interface CDATASection : Text
(definterface <cdata-section> (<text>))

;; interface DocumentType : Node
(definterface <document-type> (<node>)
  ;; readonly attribute DOMString name
  ;; [Defined in <attr>.]
  ;; readonly attribute NamedNodeMap entities
  (entities)
  ;; readonly attribute NamedNodeMap notations
  (notations)
  ;; readonly attribute DOMString publicId
  (public-id)
  ;; readonly attribute DOMString systemId
  (system-id)
  ;; readonly attribute DOMString internalSubset
  (internal-subset)
  )

;; interface Notation : Node
(definterface <notation> (<node>)
  ;; readonly attribute DOMString publicId
  ;; [Defined in <document-type>.]
  ;; readonly attribute DOMString systemId
  ;; [Defined in <document-type>.]
  )

;; interface Entity : Node
(definterface <entity> (<node>)
  ;; readonly attribute DOMString publicId
  ;; [Defined in <document-type>.]
  ;; readonly attribute DOMString systemId
  ;; [Defined in <document-type>.]
  )

;; interface EntityReference : Node
(definterface <entity-reference> (<node>))

;; interface ProcessingInstruction : Node
(definterface <processing-instruction> (<node>)
  ;; readonly attribute DOMString target
  (target)
  ;; attribute DOMString data
  ;; [Defined in <character-data>].
  )

