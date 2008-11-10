;; JEP-0030: Service Discovery

#lang s-exp "swindle.ss"

(require "client.ss")
(require "dom.ss")
(require "iq.ss")
(require-dom)
(provide (all-defined))

(define *disco-info-ns* "http://jabber.org/protocol/disco#info")
(define *disco-items-ns* "http://jabber.org/protocol/disco#items")

(defelementclass <disco-info> *disco-info-ns* "query")
(defelementclass <disco-items> *disco-items-ns* "query")

(defmethod (handle-iq-request (client <client>) (from <string>)
                              (payload <disco-info>))
  (append-child!/xexpr payload '(identity ((category "client")
					   (type "bot"))))
  payload)

