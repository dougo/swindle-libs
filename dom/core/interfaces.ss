#lang swindle

(require "../swindle.ss")
(require "types.ss")
(provide (all-defined))

;; exception DOMException
(defgeneric (exn:dom? v))
;; unsigned short code
(defgeneric (exn:dom-code exn))
;; // ExceptionCode
(define <exception-code> <exact-integer>)
;; const unsigned short INDEX_SIZE_ERR                 = 1
(define *index-size-err* 1)
;; const unsigned short DOMSTRING_SIZE_ERR             = 2
(define *domstring-size-err* 2)
;; const unsigned short HIERARCHY_REQUEST_ERR          = 3
(define *hierarchy-request-err* 3)
;; const unsigned short WRONG_DOCUMENT_ERR             = 4
(define *wrong-document-err* 4)
;; const unsigned short INVALID_CHARACTER_ERR          = 5
(define *invalid-character-err* 5)
;; const unsigned short NO_DATA_ALLOWED_ERR            = 6
(define *no-data-allowed-err* 6)
;; const unsigned short NO_MODIFICATION_ALLOWED_ERR    = 7
(define *no-modification-allowed-err* 7)
;; const unsigned short NOT_FOUND_ERR                  = 8
(define *not-found-err* 8)
;; const unsigned short NOT_SUPPORTED_ERR              = 9
(define *not-supported-err* 9)
;; const unsigned short INUSE_ATTRIBUTE_ERR            = 10
(define *inuse-attribute-err* 10)
;; const unsigned short INVALID_STATE_ERR              = 11
(define *invalid-state-err* 11)
;; const unsigned short SYNTAX_ERR                     = 12
(define *syntax-err* 12)
;; const unsigned short INVALID_MODIFICATION_ERR       = 13
(define *invalid-modification-err* 13)
;; const unsigned short NAMESPACE_ERR                  = 14
(define *namespace-err* 14)
;; const unsigned short INVALID_ACCESS_ERR             = 15
(define *invalid-access-err* 15)
;; const unsigned short VALIDATION_ERR                 = 16
(define *validation-err* 16)
;; const unsigned short TYPE_MISMATCH_ERR              = 17
(define *type-mismatch-err* 17)

;; interface DOMStringList
(definterface <dom-string-list> ()
  ;; DOMString item(in unsigned long index)
  (item (index <exact-integer>))
  ;; readonly attribute unsigned long length
  ;; [Use Swindle's len generic.]
  ;; boolean contains(in DOMString str)
  (contains? (str <dom-string>))
  )

;; interface NameList
(definterface <name-list> ()
  ;; TO DO: make <multiple-arity-generic> or something, so that
  ;; get-name and get-namespace-uri can be called name and
  ;; namespace-uri.  (They are defined as one-argument generics, on
  ;; <attr> and <node> respectively.)
  ;; DOMString getName(in unsigned long index)
  (get-name &opt (index <exact-integer>))
  ;; DOMString getNamespaceURI(in unsigned long index)
  (get-namespace-uri &opt (index <exact-integer>))
  ;; readonly attribute unsigned long length
  ;; [Use Swindle's len generic.]
  ;; boolean contains(in DOMString str)
  ;; [Defined in <dom-string-list>.]
  ;; boolean containsNS(in DOMString namespaceURI, in DOMString name)
  (contains-ns? (namespace-uri <dom-string>) (name <dom-string>))
  )

;; interface DOMImplementationList
(definterface <dom-implementation-list> ()
  ;; DOMImplementation item(in unsigned long index)
  ;; [Defined in <dom-string-list>.]
  ;; readonly attribute unsigned long length
  ;; [Use Swindle's len generic.]
  )

;; interface DOMImplementationSource
(definterface <dom-implementation-source> ()
  ;; DOMImplementation getDOMImplementation(in DOMString features)
  (dom-implementation (features <dom-string>))
  ;; DOMImplementationList getDOMImplementationList(in DOMString features)
  (dom-implementation-list (features <dom-string>))
  )

;; interface DOMImplementation
(definterface <dom-implementation> ()
  ;; boolean hasFeature(in DOMString feature, in DOMString version)
  (has-feature? (feature <dom-string>) &opt version)
  ;; DocumentType createDocumentType(in DOMString qualifiedName,
  ;;                                 in DOMString publicId,
  ;;                                 in DOMString systemId)
  ;;              raises(DOMException)
  (create-document-type (qualified-name <dom-string>)
                        (public-id <dom-string>)
                        (system-id <dom-string>))
  ;; Document createDocument(in DOMString namespaceURI,
  ;;                         in DOMString qualifiedName,
  ;;                         in DocumentType doctype)
  ;;          raises(DOMException)
  (create-document namespace-uri	;may be #f
                   (qualified-name <dom-string>)
                   doctype)		;may be #f
  ;; DOMObject getFeature(in DOMString feature, in DOMString version)
  (feature (feature <dom-string>) &opt version)
  )

;; // NodeType
(define <node-type> <exact-integer>)
;; const unsigned short ELEMENT_NODE                   = 1
(define *element-node* 1)
;; const unsigned short ATTRIBUTE_NODE                 = 2
(define *attribute-node* 2)
;; const unsigned short TEXT_NODE                      = 3
(define *text-node* 3)
;; const unsigned short CDATA_SECTION_NODE             = 4
(define *cdata-section-node* 4)
;; const unsigned short ENTITY_REFERENCE_NODE          = 5
(define *entity-reference-node* 5)
;; const unsigned short ENTITY_NODE                    = 6
(define *entity-node* 6)
;; const unsigned short PROCESSING_INSTRUCTION_NODE    = 7
(define *processing-instruction-node* 7)
;; const unsigned short COMMENT_NODE                   = 8
(define *comment-node* 8)
;; const unsigned short DOCUMENT_NODE                  = 9
(define *document-node* 9)
;; const unsigned short DOCUMENT_TYPE_NODE             = 10
(define *document-type-node* 10)
;; const unsigned short DOCUMENT_FRAGMENT_NODE         = 11
(define *document-fragment-node* 11)
;; const unsigned short NOTATION_NODE                  = 12
(define *notation-node* 12)

;; interface Node
(definterface <node> ()
  ;; readonly attribute DOMString nodeName
  (node-name)
  ;; attribute DOMString nodeValue // raises(DOMException) on setting
  ;;                               // raises(DOMException) on retrieval
  (node-value)
  (set-node-value! (new-value <dom-string>))
  ;; readonly attribute unsigned short nodeType
  (node-type)
  ;; readonly attribute Node parentNode
  (parent-node)
  ;; readonly attribute NodeList childNodes
  (child-nodes)
  ;; readonly attribute Node firstChild
  (first-child)
  ;; readonly attribute Node lastChild
  (last-child)
  ;; readonly attribute Node previousSibling
  (previous-sibling)
  ;; readonly attribute Node nextSibling
  (next-sibling)
  ;; readonly attribute NamedNodeMap attributes
  (attributes)
  ;; readonly attribute Document ownerDocument
  (owner-document)
  ;; Node insertBefore(in Node newChild, in Node refChild)
  ;;      raises(DOMException)
  (insert-before! (new-child <node>) ref-child) ;ref-child may be #f
  ;; Node replaceChild(in Node newChild, in Node oldChild)
  ;;      raises(DOMException)
  (replace-child! (new-child <node>) (old-child <node>))
  ;; Node removeChild(in Node oldChild) raises(DOMException)
  (remove-child! (old-child <node>))
  ;; Node appendChild(in Node newChild) raises(DOMException)
  (append-child! (new-child <node>))
  ;; boolean hasChildNodes()
  (has-child-nodes?)
  ;; Node cloneNode(in boolean deep) raises(DOMException)
  (clone-node deep?)
  ;; void normalize()
  (normalize!)
  ;; boolean isSupported(in DOMString feature, in DOMString version)
  (supported? (feature <dom-string>) &opt version)
  ;; readonly attribute DOMString namespaceURI
  (namespace-uri)
  ;; attribute DOMString prefix // raises(DOMException) on setting
  (prefix)
  (set-prefix! value)			;may be #f
  ;; readonly attribute DOMString localName
  (local-name)
  ;; boolean hasAttributes()
  (has-attributes?)
  ;; readonly attribute DOMString baseURI
  (base-uri)
  ;; unsigned short compareDocumentPosition(in Node other)
  ;;                raises(DOMException)
  (compare-document-position (other <node>))
  ;; attribute DOMString textContent // raises(DOMException) on setting
  ;;                                 // raises(DOMException) on retrieval
  (text-content)
  (set-text-content! value)		;<dom-string> or #f
  ;; boolean isSameNode(in Node other)
  (same-node? (other <node>))
  ;; DOMString lookupPrefix(in DOMString namespaceURI)
  (lookup-prefix (namespace-uri <dom-string>))
  ;; boolean isDefaultNamespace(in DOMString namespaceURI)
  (default-namespace? prefix)		;<dom-string> or #f
  ;; boolean lookupNamespaceURI(in DOMString prefix)
  (lookup-namespace-uri prefix)	;<dom-string> or #f
  ;; boolean isEqualNode(in Node arg)
  (equal-node? (arg <node>))
  ;; DOMObject getFeature(in DOMString feature, in DOMString version)
  ;; [Defined in <dom-implementation>.]
  ;; DOMUserData setUserData(in DOMString key, in DOMUserData data,
  ;;                         in UserDataHandler handler)
  (set-user-data! (key <dom-string>) (data <dom-user-data>)
                  (handler <user-data-handler>))
  ;; DOMUserData getUserData(in DOMString key)
  (user-data (key <dom-string>))
  )

;; // DocumentPosition
(define <document-position> <exact-integer>)
;; const unsigned short DOCUMENT_POSITION_DISCONNECTED            = 0x01
(define *document-position-disconnected* #x01)
;; const unsigned short DOCUMENT_POSITION_PRECEDING               = 0x02
(define *document-position-preceding* #x02)
;; const unsigned short DOCUMENT_POSITION_FOLLOWING               = 0x04
(define *document-position-following* #x04)
;; const unsigned short DOCUMENT_POSITION_CONTAINS                = 0x08
(define *document-position-contains* #x08)
;; const unsigned short DOCUMENT_POSITION_CONTAINED_BY            = 0x10
(define *document-position-contained-by* #x10)
;; const unsigned short DOCUMENT_POSITION_IMPLEMENTATION_SPECIFIC = 0x20
(define *document-position-implementation-specific* #x20)

;; interface NodeList
(definterface <node-list> ()
  ;; Node item(in unsigned long index)
  ;; [Defined in <dom-string-list>.]
  ;; readonly attribute unsigned long length
  ;; [Use Swindle's len generic.]
  )

;; interface NamedNodeMap
(definterface <named-node-map> ()
  ;; Node getNamedItem(in DOMString name)
  (named-item (name <dom-string>))
  ;; Node setNamedItem(in Node arg) raises(DOMException)
  (set-named-item! (arg <node>))
  ;; Node removeNamedItem(in DOMString name) raises(DOMException)
  (remove-named-item! (name <dom-string>))
  ;; Node item(in unsigned long index)
  ;; [Defined in <dom-string-list>.]
  ;; readonly attribute unsigned long length
  ;; [Use Swindle's len generic.]
  ;; Node getNamedItemNS(in DOMString namespaceURI, in DOMString localName)
  (named-item-ns namespace-uri	;may be #f
                 (local-name <dom-string>))
  ;; Node setNamedItemNS(in Node arg) raises(DOMException)
  (set-named-item-ns! (arg <node>))
  ;; Node removeNamedItemNS(in DOMString namespaceURI, in DOMString localName)
  ;;      raises(DOMException)
  (remove-named-item-ns! namespace-uri ;may be #f
                         (local-name <dom-string>))
  )

;; interface CharacterData : Node
(definterface <character-data> (<node>)
  ;; attribute DOMString data // raises(DOMException) on setting
  ;;                          // raises(DOMException) on retrieval
  (data)
  (set-data! (value <dom-string>))
  ;; readonly attribute unsigned long length
  ;; [Use Swindle's len generic.]
  ;; DOMString substringData(in unsigned long offset, in unsigned long count)
  ;;           raises(DOMException)
  (substring-data (offset <exact-integer>) (count <exact-integer>))
  ;; void appendData(in DOMString arg) raises(DOMException)
  (append-data! (arg <dom-string>))
  ;; void insertData(in unsigned long offset, in DOMString arg)
  ;;      raises(DOMException)
  (insert-data! (offset <exact-integer>) (arg <dom-string>))
  ;; void deleteData(in unsigned long offset, in unsigned long count)
  ;;      raises(DOMException)
  (delete-data! (offset <exact-integer>) (count <exact-integer>))
  ;; void replaceData(in unsigned long offset, in unsigned long count,
  ;;                  in DOMString arg)
  ;;      raises(DOMException)
  (replace-data! (offset <exact-integer>) (count <exact-integer>)
                 (arg <dom-string>))
  )

;; interface Attr : Node
(definterface <attr> (<node>)
  ;; readonly attribute DOMString name
  (name)
  ;; readonly attribute boolean specified
  (specified?)
  ;; attribute DOMString value // raises(DOMException) on setting
  (value)
  (set-value! (value <dom-string>))
  ;; readonly attribute Element ownerElement
  (owner-element)
  ;; readonly attribute TypeInfo schemaTypeInfo
  (schema-type-info)
  ;; readonly attribute boolean isId
  (id?)
  )

;; interface Element : Node
(definterface <element> (<node>)
  ;; readonly attribute DOMString tagName
  (tag-name)
  ;; DOMString getAttribute(in DOMString name)
  (attribute (name <dom-string>))
  ;; void setAttribute(in DOMString name, in DOMString value)
  ;;      raises(DOMException)
  (set-attribute! (name <dom-string>) (value <dom-string>))
  ;; void removeAttribute(in DOMString name) raises(DOMException)
  (remove-attribute! (name <dom-string>))
  ;; Attr getAttributeNode(in DOMString name)
  (attribute-node (name <dom-string>))
  ;; Attr setAttributeNode(in Attr newAttr) raises(DOMException)
  (set-attribute-node! (new-attr <attr>))
  ;; Attr removeAttributeNode(in Attr oldAttr) raises(DOMException)
  (remove-attribute-node! (old-attr <attr>))
  ;; NodeList getElementsByTagName(in DOMString name)
  (elements-by-tag-name (tagname <dom-string>))
  ;; DOMString getAttributeNS(in DOMString namespaceURI,
  ;;                          in DOMString localName)
  (attribute-ns namespace-uri		;may be #f
                (local-name <dom-string>))
  ;; void setAttributeNS(in DOMString namespaceURI,
  ;;                     in DOMString qualifiedName,
  ;;                     in DOMString value)
  ;;      raises(DOMException)
  (set-attribute-ns! namespace-uri	;may be #f
                     (qualified-name <dom-string>)
                     (value <dom-string>))
  ;; void removeAttributeNS(in DOMString namespaceURI,
  ;;                        in DOMString localName)
  ;;      raises(DOMException)
  (remove-attribute-ns! namespace-uri ;may be #f
                        (local-name <dom-string>))
  ;; Attr getAttributeNodeNS(in DOMString namespaceURI,
  ;;                         in DOMString localName)
  (attribute-node-ns namespace-uri	;may be #f
                     (local-name <dom-string>))
  ;; Attr setAttributeNodeNS(in Attr newAttr)
  ;;      raises(DOMException)
  (set-attribute-node-ns! (new-attr <attr>))
  ;; NodeList getElementsByTagNameNS(in DOMString namespaceURI,
  ;;                                 in DOMString localName)
  (elements-by-tag-name-ns namespace-uri ;may be #f
                           (local-name <dom-string>))
  ;; boolean hasAttribute(in DOMString name)
  (has-attribute? (name <dom-string>))
  ;; boolean hasAttributeNS(in DOMString namespaceURI,
  ;;                        in DOMString localName)
  (has-attribute-ns? namespace-uri	;may be #f
                     (local-name <dom-string>))
  ;; readonly attribute TypeInfo schemaTypeInfo
  ;; [Defined in <attr>.]
  ;; void setIdAttribute(in DOMString name, in boolean isId)
  ;;      raises(DOMException)
  (set-id-attribute! (name <dom-string>) id?)
  ;; void setIdAttributeNS(in DOMString namespaceURI, in DOMString localName, 
  ;;                       in boolean isId)
  ;;      raises(DOMException)
  (set-id-attribute-ns! namespace-uri	;may be #f
			(local-name <dom-string>) id?)
  ;; void setIdAttributeNode(in Attr idAttr, in boolean isId)
  ;;      raises(DOMException)
  (set-id-attribute-node! (id-attr <attr>) id?)
  )

;; interface Text : CharacterData
(definterface <text> (<character-data>)
  ;; Text splitText(in unsigned long offset) raises(DOMException)
  (split-text! (offset <exact-integer>))
  ;; readonly attribute boolean isElementContentWhitespace
  (element-content-whitespace?)
  ;; readonly attribute DOMString wholeText
  (whole-text)
  ;; Text replaceWholeText(in DOMString content) raises(DOMException)
  (replace-whole-text! (content <dom-string>))
  )

;; interface Comment : CharacterData
(definterface <comment> (<character-data>))

;; interface TypeInfo
(definterface <type-info> ()
  ;; readonly attribute DOMString typeName
  (type-name)
  ;; readonly attribute DOMString typeNamespace
  (type-namespace)
  ;; boolean isDerivedFrom(in DOMString typeNamespaceArg,
  ;;                       in DOMString typeNameArg,
  ;;                       in unsigned long derivationMethod)
  (derived-from? type-namespace-arg	;may be #f
		 (type-name-arg <dom-string>)
		 (derivation-method <exact-integer>))
  )

;; // DerivationMethods
(define <derivation-methods> <exact-integer>)
;; const unsigned long DERIVATION_RESTRICTION         = 0x00000001
(define *derivation-restriction* #x00000001)
;; const unsigned long DERIVATION_EXTENSION           = 0x00000002
(define *derivation-extension* #x00000002)
;; const unsigned long DERIVATION_UNION               = 0x00000004
(define *derivation-union* #x00000004)
;; const unsigned long DERIVATION_LIST                = 0x00000008
(define *derivation-list* #x00000008)

;; interface UserDataHandler
(definterface <user-data-handler> ()
  ;; void handle (in unsigned short operation, in DOMString key,
  ;;              in DOMUserData data, in Node src, in Node dst)
  (handle (operation <operation-type>) (key <dom-string>)
          (data <dom-user-data>) (src <node>) (dst <node>))
  )

;; // OperationType
(define <operation-type> <exact-integer>)
;; const unsigned short NODE_CLONED   = 1
(define *node-cloned* 1)
;; const unsigned short NODE_IMPORTED = 2
(define *node-imported* 2)
;; const unsigned short NODE_DELETED  = 3
(define *node-deleted* 3)
;; const unsigned short NODE_RENAMED  = 4
(define *node-renamed* 4)
;; const unsigned short NODE_ADOPTED  = 5
(define *node-adopted* 5)

;; interface DOMError
(definterface <dom-error> ()
  ;; readonly attribute unsigned short severity
  (severity)
  ;; readonly attribute DOMString message
  (message)
  ;; readonly attribute DOMString type
  (type)
  ;; readonly attribute DOMObject relatedException
  (related-exception)
  ;; readonly attribute DOMObject relatedData
  (related-data)
  ;; readonly attribute DOMLocator location
  (location)
  )

;; // ErrorSeverity
(define <error-severity> <exact-integer>)
;; const unsigned short SEVERITY_WARNING               = 1
(define *severity-warning* 1)
;; const unsigned short SEVERITY_ERROR                 = 2
(define *severity-error* 2)
;; const unsigned short SEVERITY_FATAL_ERROR           = 3
(define *severity-fatal-error* 3)

;; interface DOMErrorHandler
(definterface <dom-error-handler> ()
  ;; boolean handleError(in DOMError error)
  (handle-error (error <dom-error>))
  )

;; interface DOMLocator
(definterface <dom-locator> ()
  ;; readonly attribute long lineNumber
  (line-number)
  ;; readonly attribute long columnNumber
  (column-number)
  ;; readonly attribute long byteOffset
  (byte-offset)
  ;; readonly attribute long utf16Offset
  (utf-16-offset)
  ;; readonly attribute Node relatedNode
  (related-node)
  ;; readonly attribute DOMString uri
  (uri)
  )

;; interface DOMConfiguration
(definterface <dom-configuration> ()
  ;; void setParameter(in DOMString name, in DOMUserData value)
  ;;      raises(DOMException)
  (set-parameter! (name <dom-string>) value) ;<dom-user-data> or #f
  ;; DOMUserData getParameter(in DOMString name) raises(DOMException)
  (parameter (name <dom-string>))
  ;; boolean canSetParameter(in DOMString name, in DOMUserData value)
  (can-set-parameter? (name <dom-string>) value) ;<dom-user-data> or #f
  ;; readonly attribute DOMStringList parameterNames
  (parameter-names)
  )

;; CDataSection, DocumentType, Notation, Entity, EntityReference, and
;; ProcessingInstruction are in ../xml/interfaces.ss.

;; interface DocumentFragment : Node
(definterface <document-fragment> (<node>))

;; interface Document : Node
(definterface <document> (<node>)
  ;; readonly attribute DocumentType doctype
  (doctype)
  ;; readonly attribute DOMImplementation implementation
  (implementation)
  ;; readonly attribute Element documentElement
  (document-element)
  ;; Element createElement(in DOMString tagName) raises(DOMException)
  (create-element (tag-name <dom-string>))
  ;; DocumentFragment createDocumentFragment()
  (create-document-fragment)
  ;; Text createTextNode(in DOMString data)
  (create-text-node (data <dom-string>))
  ;; Comment createComment(in DOMString data)
  (create-comment (data <dom-string>))
  ;; CDATASection createCDATASection(in DOMString data)
  (create-cdata-section (data <dom-string>))
  ;; ProcessingInstruction createProcessingInstruction(in DOMString target,
  ;;                                                   in DOMString data)
  ;;                       raises(DOMException)
  (create-processing-instruction (target <dom-string>) (data <dom-string>))
  ;; Attr createAttribute(in DOMString name) raises(DOMException)
  (create-attribute (name <dom-string>))
  ;; EntityReference createEntityReference(in DOMString name)
  ;;                 raises(DOMException)
  (create-entity-reference (name <dom-string>))
  ;; NodeList getElementsByTagName(in DOMString tagname)
  ;; [Defined in <element>.]
  ;; Node importNode(in Node importedNode, in boolean deep)
  ;;      raises(DOMException)
  (import-node (imported-node <node>) deep?)
  ;; Element createElementNS(in DOMString namespaceURI,
  ;;                         in DOMString qualifiedName)
  ;;         raises(DOMException)
  (create-element-ns namespace-uri	;may be #f
                     (qualified-name <dom-string>))
  ;; Attr createAttributeNS(in DOMString namespaceURI,
  ;;                        in DOMString qualified-name)
  ;;         raises(DOMException)
  (create-attribute-ns namespace-uri	;may be #f
                       (qualified-name <dom-string>))
  ;; NodeList getElementsByTagNameNS(in DOMString namespaceURI,
  ;;                                 in DOMString localName)
  ;; [Defined in <element>.]
  ;; Element getElementById(in DOMString elementId)
  (element-by-id (element-id <dom-string>))
  ;; readonly attribute DOMString inputEncoding
  (input-encoding)
  ;; readonly attribute DOMString xmlEncoding
  (xml-encoding)
  ;; attribute boolean xmlStandalone // raises(DOMException) on setting
  (xml-standalone?)
  (set-xml-standalone?! value)
  ;; attribute DOMString xmlVersion // raises(DOMException) on setting
  (xml-version)
  (set-xml-version! value)		;<dom-string> or #f
  ;; attribute boolean strictErrorChecking
  (strict-error-checking?)
  (set-strict-error-checking?! value)
  ;; attribute DOMString documentURI
  (document-uri)
  (set-document-uri! value)		;<dom-string> or #f
  ;; Node adoptNode(in Node source) raises(DOMException)
  (adopt-node! (source <node>))
  ;; readonly attribute DOMConfiguration domConfig
  (dom-config)
  ;; void normalizeDocument()
  (normalize-document!)
  ;; Node renameNode(in Node n, in DOMString namespaceURI,
  ;;                 in DOMString qualifiedName)
  ;;      raises(DOMException)
  (rename-node! (n <node>) namespace-uri ;may be #f
		(qualified-name <dom-string>))
  )
