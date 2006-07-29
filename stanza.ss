(module stanza "swindle.ss"
  (require (lib "dom.ss" "dom"))
  (require "dom.ss")
  (require "stream.ss")
  (provide (all-defined))

  (defclass <stanza> (<jabber-element>))
  (defattrs <stanza> to from id type (xml:lang *xml-ns*))
)
