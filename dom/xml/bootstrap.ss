(module bootstrap "../swindle.ss"
  (require "../core/interfaces.ss")
  (require "../core/types.ss")
  (require "interfaces.ss")
  (require (only "../core/document.ss" <document-impl>))
  (require (only "../core/dom-implementation.ss" *the-dom-implementation*))
  (require (only "document-type.ss" <document-type-impl>))

  (defmethod (make-xml-dom-implementation)
    *the-dom-implementation*)

  (defclass* <xml-document> (<document-impl>))

)
