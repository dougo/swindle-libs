#lang swindle

(require "../swindle.ss")
(require "../core/types.ss")
(require "../core/interfaces.ss")
(require "interfaces.ss")
(require (only "bootstrap.ss" <xml-document>))
(require (only "../core/character-data.ss" <text-impl>))

(defmethod (node-name (node <cdata-section>)) "#cdata-section")
(defmethod (node-type (node <cdata-section>)) *cdata-section-node*)

(defmethod (create-cdata-section (document <xml-document>)
                                 (data <dom-string>))
  (make <cdata-section-impl> :document document :data data))

(defmethod (clone-node (node <cdata-section>) deep?)
  (create-cdata-section (owner-document node) (node-value node)))

(defclass* <cdata-section-impl> (<text-impl> <cdata-section>))

