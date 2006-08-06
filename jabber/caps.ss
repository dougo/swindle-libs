;;; JEP-0115: Entity Capabilities

(module caps "swindle.ss"
  (require "client.ss")
  (require "dom.ss")
  (require "presence.ss")
  (require "stream.ss")
  (require (lib "dom.ss" "dom"))
  (provide (all-defined))

  (define *caps-ns* "http://jabber.org/protocol/caps")

  (defaroundmethod (make-presence (client <client>) &key type)
    (let ((presence (call-next-method)))
      (unless (eq? type 'unavailable)
        (append-child! presence (make-caps client)))
      presence))

  (defelementclass <caps> *caps-ns* "c")

  (defmethod (make-caps (client <client>))
    (let ((caps (create-element-ns (document (initial-stream client))
                                   *caps-ns* "c"))
          (feature-bundles (feature-bundles client)))
      (set! (attribute caps "xmlns") *caps-ns*
            (attribute caps "node") (as <dom-string> (type-uri client))
            (attribute caps "ver") (as <dom-string> (version client)))
      (unless (null? feature-bundles)
        (set! (attribute caps "ext") (as <dom-string>
                                         (apply echos feature-bundles))))
      caps))

  ;; Override these for specific clients...

  (defmethod (type-uri (client <client>))
    "http://code.google.com/p/swindle-libs/")
  (defmethod (version (client <client>))
    "0")
  (defmethod (feature-bundles (client <client>))
    null)
)
