;;; JEP-0078: Non-SASL authentication

#lang s-exp "swindle.ss"

(require "client.ss")
(require "dom.ss")
(require "exn.ss")
(require "iq.ss")
(require "jid.ss")
(require "stream.ss")
(require dom)
(provide (all-defined))

(define *auth-ns* "jabber:iq:auth")

(defelementclass <auth-query> *auth-ns* "query")

(defstruct <exn:xmpp:auth> (<exn:xmpp>))
(defstruct <exn:xmpp:auth:unsupported> (<exn:xmpp:auth>))

(defmethod (login (client <client>) (password <string>))
  (let ((fields (auth-fields client))
        (jid (address client)))
    (unless (text-field fields 'password)
      (raise (make-exn:xmpp:auth:unsupported
              "login: server doesn't support plaintext authentication"
              (current-continuation-marks))))
    (iq client 'set (make-auth-query client
                                     :username (node-id jid)
                                     :password password 
                                     :resource (resource-id jid)))))

(defmethod (make-auth-query (client <client>)
                            &key username password digest resource)
  (let ((query (create-element-ns (document (initial-stream client))
                                  *auth-ns* "query")))
    (set! (attribute-ns query *xmlns-ns* "xmlns") *auth-ns*
          (text-field query 'username) username
          (text-field query 'password) password
          (text-field query 'digest) digest
          (text-field query 'resource) resource)
    query))

(defmethod (auth-fields (client <client>))
  (iq client 'get (make-auth-query client)))

