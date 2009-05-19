#lang swindle

(require "../swindle.ss")
(require "types.ss")
(require "interfaces.ss")
(require "exn.ss")
(require (only "readonly.ss" check-readonly))
(require (only "owned.ss" <owned> check-ownership))
(require (only "contained.ss" <contained> set-container!))

(definterface* <node-collection> ())

(defmethod (len (nodes <node-collection>))
  (count-of (elt <- (each-elt nodes))))
;; NOTE: descendants of <node-collection> MUST override either len
;; or each-elt.  Otherwise these form a cycle!

(defaroundmethod (item (nodes <node-collection>) (index <exact-integer>))
  (and (< -1 index (len nodes)) (call-next-method)))

(defmethod (item (nodes <node-collection>) (index <exact-integer>))
  ;; This is O(n), because the collection is live.
  ;; TO DO: cache last access
  (let/ec return
    (do-sequence (node nodes)
      (when (zero? index) (return node))
      (dec! index))
    #f))
;; NOTE: non-empty descendants of <node-collection> MUST override
;; either item, for-each-sequence, or each-elt.  Otherwise these
;; form a cycle!


(defclass* <node-list-impl> (<node-collection> <node-list>))

(define* *the-empty-node-list* (make <node-list-impl>))
(defmethod (len (nodes = *the-empty-node-list*)) 0)


(defmethod* (check-had-item (nodes <named-node-map>) key node)
  (unless node
    (raise-exn:dom *not-found-err*
      "~v has no node named ~v" nodes key)))

(definterface* <named-node-map-impl>
  (<owned> <contained> <node-collection> <named-node-map>)
  (keyed-item key)
  (remove-keyed-item! key)
  (set-keyed-item! key (node <node>)))

(defmethod (named-item (nodes <named-node-map-impl>) (name <dom-string>))
  (keyed-item nodes name))
(defmethod (remove-named-item! (nodes <named-node-map-impl>)
                               (name <dom-string>))
  (remove-keyed-item! nodes name))
(defmethod (set-named-item! (nodes <named-node-map-impl>) (arg <node>))
  (set-keyed-item! nodes (node-name arg) arg))

(defaroundmethod (remove-keyed-item! (nodes <named-node-map-impl>) key)
  ;; TO DO: add default attribute
  (let ((node (call-next-method)))
    (check-had-item nodes key node)
    (set-container! node #f)
    node))

(defbeforemethod (remove-keyed-item! (nodes <named-node-map-impl>) key)
  (check-readonly nodes))

(defbeforemethod (set-keyed-item! (nodes <named-node-map-impl>)
                                  key (arg <node>))
  (check-readonly nodes)
  (check-ownership nodes arg))

(defaftermethod (set-keyed-item! (nodes <named-node-map-impl>)
                                 key (arg <node>))
  (set-container! arg nodes))


(defclass* <named-node-hash-table> (<named-node-map-impl>)
  (table :reader table :initializer (thunk (make-hash-table 'equal))))

(defmethod (len (nodes <named-node-hash-table>))
  (hash-table-count (table nodes)))

(defmethod (keyed-item (nodes <named-node-hash-table>) key)
  (hash-table-get (table nodes) key (thunk #f)))

(defmethod (remove-keyed-item! (nodes <named-node-hash-table>) key)
  (begin0 (keyed-item nodes key)
          (hash-table-remove! (table nodes) key)))

(defmethod (set-keyed-item! (nodes <named-node-hash-table>) key (node <node>))
  (hash-table-put! (table nodes) key node))


(defmethod (map-sequence (func <function>) (nodes <named-node-hash-table>))
  (hash-table-map (table nodes) (lambda (name node) (func node))))

(defmethod (for-each-sequence (func <function>)
                              (nodes <named-node-hash-table>))
  (hash-table-for-each (table nodes) (lambda (name node) (func node))))


;; TO DO: this won't let named-item return a namespaced node!

(defmethod* (key-ns (nodes <named-node-map-impl>)
                    (ns = #f) (name <dom-string>))
  name)
(defmethod (key-ns (nodes <named-node-map-impl>)
                   (ns <dom-string>) (name <dom-string>))
  (list ns name))

(defmethod (named-item-ns (nodes <named-node-hash-table>)
                          ns (name <dom-string>))
  (keyed-item nodes (key-ns nodes ns name)))

(defmethod (remove-named-item-ns! (nodes <named-node-hash-table>)
                                  ns (name <dom-string>))
  (remove-keyed-item! nodes (key-ns nodes ns name)))

(defmethod (set-named-item-ns! (nodes <named-node-hash-table>) (node <node>))
  (set-keyed-item! nodes (key-ns nodes (namespace-uri node)
                                 (or (local-name node) (node-name node)))
                   node))


(defclass* <dom-implementation-list-impl> (<dom-implementation-list>)
  vec :autoaccessors :slot)

(defmethod (item (impls <dom-implementation-list-impl>) (index <exact-integer>))
  (vector-ref (vec impls) index))

(defmethod (len (impls <dom-implementation-list-impl>))
  (vector-length (vec impls)))
