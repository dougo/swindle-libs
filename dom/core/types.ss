(module types "../swindle.ss"
  (provide (all-defined))

  ;; valuetype DOMString sequence<unsigned short>
  (define <dom-string> <immutable-string>)

  ;; typedef unsigned long long DOMTimeStamp
  (define <dom-time-stamp> <exact-integer>)

  ;; typedef any DOMUserData
  (define <dom-user-data> <top>)

  ;; typedef Object DOMObject
  (define <dom-object> <object>)
)
