(module stanza "swindle.ss"
  (require (lib "dom.ss" "dom"))
  (require "dom.ss")
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
)
