;;; Converting between DOM objects and MzScheme's built-in XML structures.

#lang swindle

(require "swindle.ss")
(require (prefix xml: xml/xml))
(provide xml:read-comments xml:empty-tag-shorthand
         xml:xexpr-drop-empty-attributes)
(require "core/core.ss")
(require "xml/xml.ss")
(require (only html/html read-html-as-xml))

;; TO DO: implement DOM Level 3 Load/Save

(defmethod* (read-dom &optional dom port)
  (xml->dom (xml:read-xml port) dom))
(defmethod* (read-dom/element (parent <node>) . port)
  (xml->dom (xml:read-xml/element . port) parent))
(defmethod* (write-dom (doc <document>) . port)
  (xml:write-xml (dom->xml doc) . port))
(defmethod (write-dom (node <node>) . port)
  (xml:write-xml/content (dom->xml node) . port))
(defmethod* (display-dom (doc <document>) . port)
  (xml:display-xml (dom->xml doc) . port))
(defmethod (display-dom (node <node>) . port)
  (xml:display-xml/content (dom->xml node) . port))
(defmethod* (xexpr->dom xexpr (parent <node>))
  (xml->dom (xml:xexpr->xml xexpr) parent))
(defmethod* (dom->xexpr (node <node>))
  (xml:xml->xexpr (dom->xml node)))

(defmethod* (string->dom (s <string>) &optional dom)
  (read-dom dom (open-input-string s)))
(defmethod* (string->dom/element (s <string>) (parent <node>))
  (read-dom/element parent (open-input-string s)))
(defmethod* (dom->string (node <node>))
  (with-output-to-string (thunk (write-dom node))))

(defmethod* (read-html-as-dom (parent <node>) &opt port)
  (let ((frag (create-document-fragment (or (owner-document parent) parent))))
    (dolist (xml (read-html-as-xml port))
      (append-child! frag (xml->dom xml frag)))
    frag))

(defmethod* (append-child!/xexpr (parent <node>) xexpr)
  (append-child! parent (xexpr->dom xexpr parent)))
(defmethod (append-child!/xexpr (parent <node>) (xexpr = #f))
  (void))

;; TO DO: split this into several methods
(defmethod* (xml->dom xml &opt parent-or-dom)
  (let ((doc (and (instance-of? parent-or-dom <node>)
                  (or (owner-document parent-or-dom) parent-or-dom)))
        (dom (if (instance-of? parent-or-dom <dom-implementation>)
                 parent-or-dom
                 (dom-implementation *the-dom-implementation-registry* "XML"))))
    (cond ((xml:document? xml)
           (xml:document->dom xml dom))
          ((xml:document-type? xml)
           (xml:document-type->dom xml dom))
          ((xml:element? xml)
           (xml:element->dom xml parent-or-dom))
          ((xml:attribute? xml)
           (xml:attribute->dom xml parent-or-dom))
          ((xml:pcdata? xml)
           (xml:pcdata->dom xml doc))
          ((xml:cdata? xml)
           (xml:cdata->dom xml doc))
          ((xml:entity? xml)
           (xml:entity->dom xml doc))
          ((xml:p-i? xml)
           (xml:p-i->dom xml doc))
          ((xml:comment? xml)
           (xml:comment->dom xml doc))
          (else
           (error 'xml->dom "unknown XML value: ~v" xml)))))

(defmethod* (xml:document->dom xml (dom <dom-implementation>))
  (let* ((doc (create-document dom #f "dummy" #f))
         (prolog (xml:document-prolog xml))
         (dtd (xml:prolog-dtd prolog)))
    (remove-child! doc (document-element doc))
    (dolist (xml (xml:prolog-misc prolog))
      (append-child! doc (xml->dom xml doc)))
    (when dtd (append-child! doc (xml->dom dtd dom)))
    (dolist (xml (xml:prolog-misc2 prolog))
      (append-child! doc (xml->dom xml doc)))
    (append-child! doc (xml->dom (xml:document-element xml) doc))
    (dolist (xml (xml:document-misc xml))
      (append-child! doc (xml->dom xml doc)))
    doc))

(defmethod* (xml:document-type->dom xml (dom <dom-implementation>))
  (let ((dtd (xml:document-type-external xml)))
    (create-document-type
     dom
     (as <dom-string> (xml:document-type-name xml))
     (and dtd (xml:external-dtd/public? dtd)
          (xml:external-dtd/public-public dtd))
     (and dtd (xml:external-dtd-system dtd)))))

(defmethod* (xml:element->dom xml (parent <node>))
  ;; TO DO: I think there's a bug here, the namespace is looked up
  ;; before the parent has been added to the tree.  Check this!
  (let* ((doc (or (owner-document parent) parent))
         (qname (as <dom-string> (xml:element-name xml)))
         (ns (xml:node-ns xml parent qname))
         (element (create-element-ns doc ns qname)))
    (dolist (attr (xml:element-attributes xml))
      (set-attribute-node! element (xml->dom attr element)))
    (dolist (content (xml:normalize-text (xml:element-content xml)))
      ;; TO DO: use replace-whole-text! in case of entities
      (append-child! element (xml->dom content element)))
    element))

(defmethod* (xml:attribute->dom xml (element <element>))
  (let* ((qname (as <dom-string> (xml:attribute-name xml)))
         (ns (xml:node-ns xml element qname))
         (attr (create-attribute-ns (owner-document element) ns qname)))
    (set-value! attr (as <dom-string> (xml:attribute-value xml)))
    attr))

(defmethod* (xml:pcdata->dom xml (doc <document>))
  (create-text-node doc (as <dom-string> (xml:pcdata-string xml))))

(defmethod* (xml:cdata->dom xml (doc <document>))
  (create-cdata-section doc (as <dom-string> (xml:cdata-data xml))))

(defmethod* (xml:entity->dom xml (doc <document>))
  (let* ((text (xml:entity-text xml))
         (string (xml:entity-text->string text)))
    (if string
        (create-text-node doc (as <dom-string> string))
        (create-entity-reference doc (as <dom-string> text)))))

(defmethod* (xml:p-i->dom xml (doc <document>))
  (create-processing-instruction
   doc (as <dom-string> (xml:p-i-target-name xml))
   (as <dom-string> (xml:p-i-instruction xml))))

(defmethod* (xml:comment->dom xml (doc <document>))
  (create-comment doc (as <dom-string> (xml:comment-text xml))))


(defmethod* (dom->xml (node <document>))
  (let ((misc null) (dtd #f) (misc2 null) (element #f) (misc3 null))
    (loop-for (node <- each-child node)
      (xml is (dom->xml node))
      (cond (element (push! xml misc3))
            ((xml:element? xml) (set! element xml))
            (dtd (push! xml misc2))
            ((xml:document-type? xml) (set! dtd xml))
            (else (push! xml misc))))
    (xml:make-document (xml:make-prolog (reverse misc) dtd (reverse misc2))
                       element (reverse misc3))))

(defmethod (dom->xml (node <document-fragment>))
  (map-sequence dom->xml (child-nodes node)))

(defmethod (dom->xml (node <element>))
  (xml:make-element
   'dom 'dom
   (as <symbol> (node-name node))
   (map-sequence dom->xml (attributes node))
   (map-sequence dom->xml (child-nodes node))))

(defmethod (dom->xml (node <attr>))
  (xml:make-attribute
   'dom 'dom
   (as <symbol> (node-name node))
   (node-value node)))

(defmethod (dom->xml (node <text>))
  (xml:make-pcdata 'dom 'dom (node-value node)))

(defmethod (dom->xml (node <comment>))
  (xml:make-comment (node-value node)))

(defmethod (dom->xml (node <cdata-section>))
  (xml:make-cdata 'dom 'dom (format "<![CDATA[~a]]>" (data node))))

(defmethod (dom->xml (node <document-type>))
  (let ((public-id (public-id node))
        (system-id (system-id node)))
    (xml:make-document-type
     (as <symbol> (node-name node))
     (cond (public-id (xml:make-external-dtd/public system-id public-id))
           (system-id (xml:make-external-dtd/system system-id))
           (else #f))
     #f)))

(defmethod (dom->xml (node <entity-reference>))
  (xml:make-entity 'dom 'dom (as <symbol> (node-name node))))

(defmethod (dom->xml (node <processing-instruction>))
  (xml:make-p-i 'dom 'dom (as <symbol> (target node)) (data node)))


(defmethod (as (type = <dom-string>) (symbol <symbol>))
  (as <dom-string> (as <string> symbol)))

(defmethod* (xml:cdata-data xml)
  (let ((s (xml:cdata-string xml)))
    (substring s (len "<![CDATA[") (- (len s) (len "]]>")))))

(defmethod* (xml:entity-text->string (text <symbol>))
  (case text
    ((lt) "<") ((gt) ">") ((amp) "&") ((apos) "'") ((quot) "\"")
    (else #f)))
(defmethod (xml:entity-text->string (text <number>))
  (string (integer->char text)))

(defmethod* (xml:normalize-text content)
  (if (null? content)
      null
      (let loop ((content content))
        (let ((first (car content)) (rest (cdr content)))
          (if (null? rest)
              content
              (let ((next (car rest)))
                (if (and (string? first) (string? next))
                    (loop (cons (concat first next) (cdr rest)))
                    (cons first (loop rest)))))))))

(defmethod* (xml:node-ns node (parent <node>) (qname <dom-string>))
  (let/ec return
    (unless (well-formed-qname? qname) (return #f))
    (let ((prefix (prefix qname)))
      (when (equals? prefix "xml") (return *xml-ns*))
      (when (xml:attribute? node)
        (cond ((equals? qname "xmlns") (return *xmlns-ns*))
              ((not prefix) (return #f))
              ((equals? prefix "xmlns") (return *xmlns-ns*))))
      (when (xml:element? node)
        (let* ((name (as <symbol> (namespace-attribute-name prefix)))
               (attr (find-if (lambda (attr)
                                (eq? (xml:attribute-name attr) name))
                              (xml:element-attributes node))))
          (when attr (return (as <dom-string> (xml:attribute-value attr))))))
      (lookup-namespace-uri parent prefix))))

