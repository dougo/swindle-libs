#lang swindle

(require "../swindle.ss")
(require "../core/types.ss")
(require "../core/interfaces.ss")
(require "../xml/interfaces.ss")
(require "types.ss")
(provide (all-defined))

;; exception LSException
(defgeneric (exn:ls? v))
;; unsigned short code
(defgeneric (exn:ls-code exn))
;; // LSExceptionCode
(define <ls-exception-code> <exact-integer>)
;; const unsigned short PARSE_ERR                      = 81
(define *parse-err* 81)
;; const unsigned short SERIALIZE_ERR                  = 82
(define *serialize-err* 82)

;; // DOMImplementationLSMode
(define <dom-implementation-ls-mode> <exact-integer>)
;; const unsigned short MODE_SYNCHRONOUS               = 1
(define *mode-synchronous* 1)
;; const unsigned short MODE_ASYNCHRONOUS              = 2
(define *mode-asynchronous* 2)

;; interface DOMImplementationLS
(definterface <dom-implementation-ls> ()
  ;; LSParser createLSParser(in unsigned short mode, in DOMString schemaType)
  ;;          raises(dom::DOMException);
  (create-ls-parser (mode <exact-integer>) &opt schema-type) ;<dom-string>
  ;; LSSerializer createLSSerializer()
  (create-ls-serializer)
  ;; LSInput createLSInput()
  (create-ls-input)
  ;; LSOutput createLSOutput()
  (create-ls-output)
  )

;; // ACTION_TYPES
(define <action-type> <exact-integer>)
;; const unsigned short ACTION_APPEND_AS_CHILDREN      = 1
(define *action-append-as-children* 1)
;; const unsigned short ACTION_REPLACE_CHILDREN        = 2
(define *action-replace-children* 2)
;; const unsigned short ACTION_INSERT_BEFORE           = 3
(define *action-insert-before* 3)
;; const unsigned short ACTION_INSERT_AFTER            = 4
(define *action-insert-after* 4)
;; const unsigned short ACTION_REPLACE                 = 5
(define *action-replace* 5)

;; interface LSParser
(definterface <ls-parser> ()
  ;; readonly attribute DOMConfiguration domConfig
  ;; [Defined in <document>.]
  ;; attribute LSParserFilter filter
  (filter)
  (set-filter! (filter <ls-parser-filter>))
  ;; readonly attribute boolean async
  (async?)
  ;; readonly attribute boolean busy
  (busy?)
  ;; Document parse(in LSInput input) raises(dom::DOMException, LSException)
  (parse (input <ls-input>))
  ;; Document parseURI(in DOMString uri) raises(dom::DOMException, LSException)
  (parse-uri (uri <dom-string>))
  ;; Node parseWithContext(in LSInput input, in Node contextArg,
  ;;                       in unsigned short action)
  ;;      raises(dom::DOMException, LSException)
  (parse-with-context (input <ls-input>) (context-arg <node>)
		      (action <exact-integer>))
  ;; void abort()
  (abort)
  )

;; interface LSInput
(definterface <ls-input> ()
  ;; All these attributes may be #f.
  ;; attribute LSReader characterStream
  (character-stream)
  (set-character-stream! character-stream)
  ;; attribute LSInputStream byteStream
  (byte-stream)
  (set-byte-stream! byte-stream)
  ;; attribute DOMString stringData
  (string-data)
  (set-string-data! string-data)
  ;; attribute DOMString systemId
  ;; [Getter defined in <document-type>.]
  (set-system-id! system-id)
  ;; attribute DOMString publicId
  ;; [Getter defined in <document-type>.]
  (set-public-id! public-id)
  ;; attribute DOMString baseURI
  ;; [Getter defined in <node>.]
  (set-base-uri! base-uri)
  ;; attribute DOMString encoding
  (encoding)
  (set-encoding! encoding)
  ;; attribute boolean certifiedText
  (certified-text?)
  (set-certified-text?!)
  )

;; interface LSResourceResolver
(definterface <ls-resource-resolver> ()
  ;; LSInput resolveResource(in DOMString type, in DOMString namespaceURI,
  ;;                         in DOMString publicId, in DOMString systemId, 
  ;;                         in DOMString baseURI)
  (resolve-resource (type <dom-string>)
		    ;; These all may be #f.
		    namespace-uri public-id system-id base-uri)
  )

;; interface LSParserFilter
(definterface <ls-parser-filter> ()
  ;; unsigned short startElement(in Element elementArg)
  (start-element (element-arg <element>))
  ;; unsigned short acceptNode(in Node nodeArg)
  (accept-node (node-arg <node>))
  ;; readonly attribute unsigned long whatToShow
  (what-to-show)
  )

;; // Constants returned by startElement and acceptNode
;; const short FILTER_ACCEPT                  = 1
(define *filter-accept* 1)
;; const short FILTER_REJECT                  = 2
(define *filter-reject* 2)
;; const short FILTER_SKIP                    = 3
(define *filter-skip* 3)
;; const short FILTER_INTERRUPT               = 4
(define *filter-interrupt* 4)

;; interface LSSerializer
(definterface <ls-serializer> ()
  ;; readonly attribute DOMConfiguration domConfig
  ;; [Defined in <document>.]
  ;; attribute DOMString newLine
  (new-line)
  (set-new-line! new-line)		;may be #f
  ;; attribute LSSerializerFilter filter
  ;; [Defined in <ls-parser>.]
  ;; boolean write(in Node nodeArg, in LSOutput destination) raises(LSException)
  ;; NOTE: renamed to avoid conflicting with primitive write
  (write-to-ls-output (node-arg <node>) (destination <ls-output>))
  ;; boolean writeToURI(in Node nodeArg, in DOMString uri) raises(LSException)
  (write-to-uri (node-arg <node>) (url <dom-string>))
  ;; DOMString writeToString(in Node nodeArg)
  ;;           raises(dom::DOMException, LSException)
  (write-to-string (node-arg <node>))
  )

;; interface LSOutput
(definterface <ls-output> ()
  ;; attribute LSWriter characterStream
  ;; [Defined in <ls-input>.]
  ;; attribute LSOutputStream byteStream
  ;; [Defined in <ls-input>.]
  ;; attribute DOMString systemId
  ;; [Defined in <ls-input>.]
  ;; attribute DOMString encoding
  ;; [Defined in <ls-input>.]
  )

#| TO DO

  interface LSProgressEvent : events::Event {
    readonly attribute LSInput         input;
    readonly attribute unsigned long   position;
    readonly attribute unsigned long   totalSize;
  };

  interface LSLoadEvent : events::Event {
    readonly attribute Document        newDocument;
    readonly attribute LSInput         input;
  };

  interface LSSerializerFilter : traversal::NodeFilter {
    readonly attribute unsigned long   whatToShow;
  };

|#
