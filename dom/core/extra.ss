#lang swindle

(require "../swindle.ss")
(require "types.ss")
(require "interfaces.ss")
(require (only (lib "13.ss" "srfi")
               string-null? string-take string-drop
               string-index string-index-right))

;; Extra utility functions and syntax built on top of the core interfaces.

(defmethod* (has-child? (parent <node>) (child <node>))
  (eq? parent (parent-node child)))

(defmethod* (valid-name? (string <dom-string>))
  (and (not (string-null? string))
       (valid-name-start? (ref string 0))
       (every-sequence valid-name-char? string)))

(defmethod* (valid-name-start? (char <char>))
  (or (char-alphabetic? char)
      (memv char '(#\_ #\:))))

(defmethod* (valid-name-char? (char <char>))
  (or (valid-name-start? char)
      (char-numeric? char)
      (memv char '(#\. #\-))))

;; Normalization helpers.

(defmethod* (normalize-children! (parent <node>))
  (let loop ((child (first-child parent)))
    (when child
      (loop (normalize-next-sibling! child (next-sibling child))))))

;; Normalize a node and its next sibling and return the next node to
;; be normalized.
(defmethod* (normalize-next-sibling! (node <node>) next)
  next)
(defmethod (normalize-next-sibling! (node <element>) next)
  (normalize! node)
  next)
(defmethod (normalize-next-sibling! (node <text>) (next <text>))
  (append-data! node (data next))
  (remove-child! (parent-node next) next)
  node)
(defmethod (normalize-next-sibling! (text <text>) next)
  ;; DOM Level 2 says that empty text nodes should be removed, which
  ;; was not true in DOM Level 1.  But this isn't listed in the
  ;; Changes section, so maybe it's an erratum?
  (cond ((zero? (len text))
         (remove-child! (parent-node text) text)
         next)
        (else
         (call-next-method))))

;; Iterator value constructors.

(defmethod* (each-next-sibling (node-or-false = #f))
  (list #f identity not identity))
(defmethod (each-next-sibling (node-or-false <node>))
  (list node-or-false next-sibling not identity))

(defmethod* (each-previous-sibling (node-or-false = #f))
  (list #f identity not identity))
(defmethod (each-previous-sibling (node-or-false <node>))
  (list node-or-false previous-sibling not identity))

(defmethod* (each-child (parent <node>))
  (each-elt (child-nodes parent)))

;; Each ancestor of child, beginning with child and traversing
;; parent-node links.
(defmethod* (each-ancestor (child <node>))
  (list child parent-node not identity))

;; Each descendant of parent, beginning with parent and traversing
;; its children in pre-order.
(defmethod* (each-descendant (parent <node>))
  (list parent next-pre-order not identity))

;; The next node in pre-order.
(defmethod* (next-pre-order (node <node>))
  (or (first-child node)
      (next-sibling node)
      (some-iterator next-sibling (each-ancestor node))))


;; A non-live list of child nodes.
(defmethod* (child-list (node <node>))
  (as <list> (child-nodes node)))


;; Remove all child nodes of a node.
(defmethod* (remove-children! (node <node>))
  (dolist (child (child-list node)) (remove-child! node child)))


;; This makes node collections into sequences:
(add-ref-method <node-list> item)
(add-ref-method <named-node-map> item)


;; TO DO: (add node ...) -> document-fragment ?

(defmethod (print-object (node <character-data>) esc? port)
  (fprintf port "#<~a:~e>"
           (name-sans-<> (class-name (interface-of node)))
           (data node)))

(defmethod* (make-qname (prefix <string>) (local-name <string>))
  (as <dom-string> (concat prefix ":" local-name)))

(defmethod (make-qname (prefix = #f) (local-name <string>))
  (as <dom-string> local-name))

;; Parse a QName into prefix and local-name (returned as multiple
;; values).
(defmethod* (parse-qname (qname <dom-string>))
  (let ((index (string-index qname #\:)))
    (if index
        (values (as <dom-string> (string-take qname index))
                (as <dom-string> (string-drop qname (1+ index))))
        (values #f qname))))

(defmethod (prefix (qname <dom-string>))
  (let ((index (string-index qname #\:)))
    (and index (as <dom-string> (string-take qname index)))))

(defmethod (local-name (qname <dom-string>))
  (let ((index (string-index qname #\:)))
    (if index (as <dom-string> (string-drop qname (1+ index))) qname)))

(defmethod* (well-formed-qname? (string <dom-string>))
  (and (valid-name? string)
       (let ((index (string-index string #\:)))
         (or (not index)
             (and (positive? index)
                  (= index (string-index-right string #\:)))))))

(defmethod* (well-formed-ncname? (string <dom-string>))
  (and (valid-name? string)
       (not (string-index string #\:))))

(defmethod* (namespace-attribute-name (prefix = #f))
  "xmlns")
(defmethod (namespace-attribute-name (prefix <dom-string>))
  (concat "xmlns:" prefix))

(define* *xml-ns* "http://www.w3.org/XML/1998/namespace")
(define* *xmlns-ns* "http://www.w3.org/2000/xmlns/")

(add-equals?-method <node-list> sequence-equals?)
(add-equals?-method <named-node-map> sequence-equals?)

(defmethod (sequence-equals? (seq1 <named-node-map>)
                             (seq2 <named-node-map>))
  (and (= (len seq1) (len seq2))
       (let/ec return
         (loop-for (node1 <- each-elt seq1)
           ;; TO DO: this is probably wrong!
           (unless (or (equals? node1 (named-item seq2 (node-name node1)))
                       (equals? node1 (named-item-ns seq2
                                                     (namespace-uri node1)
                                                     (local-name node1))))
             (return #f))))
       #t))

(defmethod (equals? (node1 <node>) (node2 <node>))
  (and (equals? (node-type node1) (node-type node2))
       (equals? (node-name node1) (node-name node2))
       (equals? (local-name node1) (local-name node2))
       (equals? (namespace-uri node1) (namespace-uri node2))
       (equals? (prefix node1) (prefix node2))
       (equals? (node-value node1) (node-value node2))
       (equals? (attributes node1) (attributes node2))
       (equals? (child-nodes node1) (child-nodes node2))))

;; Find the first ancestor of node (not including node) that is an
;; element.  Used by the namespaces algorithms in namespaces.ss.
(defmethod* (ancestor-element (node <node>))
  (let ((parent (parent-node node)))
    (and parent (find-if-iterator
                 (lambda (ancestor) (instance-of? ancestor <element>))
                 (each-ancestor parent)))))
