#lang swindle

(require "../swindle.ss")
(require "interfaces.ss")
(require (only "node.ss" add-child!))
(require (only "child.ss" <child> set-siblings!))
(require (only "extra.ss" each-next-sibling))
(require (only "collections.ss" <node-list-impl>))

;; A mixin class for nodes that can have children.
(defclass* <parent> ()
  (num-child-nodes :initvalue 0)
  (first-child :initvalue #f)
  (last-child :initvalue #f)
  :autoaccessors :slot)

(defmethod (child-nodes (parent <parent>))
  (make <child-node-list> :parent parent))

(defmethod (add-child! (parent <parent>) (new-child <child>) prev next)
  (when (parent-node new-child)
    (remove-child! (parent-node new-child) new-child))
  (unless (and prev (first-child parent))
    (set! (first-child parent) new-child))
  (unless (and next (last-child parent))
    (set! (last-child parent) new-child))
  (inc! (num-child-nodes parent))
  (set-siblings! parent prev new-child next)
  new-child)

(defmethod (has-child-nodes? (node <parent>))
  (positive? (num-child-nodes node)))

(defmethod (remove-child! (parent <parent>) (old-child <child>))
  (let ((prev-child (previous-sibling old-child))
        (next-child (next-sibling old-child)))
    (set-siblings! parent prev-child next-child)
    (unless prev-child (set! (first-child parent) next-child))
    (unless next-child (set! (last-child parent) prev-child)))
  (set-siblings! #f #f old-child #f)
  old-child)


(defclass <child-node-list> (<node-list-impl>)
  (parent-node :reader parent-node :initarg :parent))

(defmethod (len (nodes <child-node-list>))
  (num-child-nodes (parent-node nodes)))

;; This iterator is much faster than using item (which is O(n)), but
;; be careful when modifying the next-sibling of the current node!
(defmethod (each-elt (nodes <child-node-list>))
  (each-next-sibling (first-child (parent-node nodes))))

