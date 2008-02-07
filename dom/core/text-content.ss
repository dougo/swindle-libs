#lang swindle

(require "../swindle.ss")
(require "types.ss")
(require "interfaces.ss")
(require "exn.ss")
(require (only "extra.ss" remove-children!))
(require (only srfi/13 string-null?))

(defmethod* (text-content-for-parent (node <node>))
  (text-content node))

;; A mixin for classes whose text content is determined by their children.
(defclass* <text-container> ())

(defmethod (text-content (node <text-container>))
  (as <dom-string>
      (apply concat
             (map-sequence text-content-for-parent (child-nodes node)))))

(defmethod (set-text-content! (node <text-container>) (value = #f))
  (set-text-content! node ""))
(defmethod (set-text-content! (node <text-container>) (value <dom-string>))
  (remove-children! node)
  (unless (string-null? value)
    (append-child! node (create-text-node (owner-document node) value)))
  (void))

