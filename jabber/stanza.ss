(module stanza "swindle.ss"
  (require (lib "dom.ss" "dom"))
  (require "dom.ss")
  (require "error.ss")
  (require "stream.ss")
  (provide (all-defined))

  ;; Abstract superclass for XML stanzas.
  (defclass <stanza> (<jabber-element>))
  (defattrs <stanza>
    to
    from
    (id :type <symbol>)
    (type :type <symbol>)
    (xml:lang :ns *xml-ns* :type <symbol>))

  ;; Make an error stanza in response to a stanza that caused an exception.
  (defmethod (make-error-stanza (exn <exn:fail>) (stanza <stanza>)
                                (stream <output-stream>))
    (let ((error-stanza (import-node (document stream) stanza #t)))
      (set! (type error-stanza) 'error
            (to error-stanza) (from stanza)
            (from error-stanza) #f)
      (append-child! error-stanza (make-stanza-error exn stream))
      error-stanza))
)
