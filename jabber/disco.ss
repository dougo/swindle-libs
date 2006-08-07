;; JEP-0030: Service Discovery

(module disco "swindle.ss"
  (require "client.ss")
  (require "dom.ss")
  (require "iq.ss")
  (require (lib "dom.ss" "dom"))
  (provide (all-defined))

  (define *disco-info-ns* "http://jabber.org/protocol/disco#info")
  (define *disco-items-ns* "http://jabber.org/protocol/disco#items")

  (defelementclass <disco-info> *disco-info-ns* "query")
  (defelementclass <disco-items> *disco-items-ns* "query")

  (defmethod (handle-iq-request (client <client>) (payload <disco-info>))
    (append-child!/xexpr payload '(identity ((category "client" "bot"))))
    payload)
)
