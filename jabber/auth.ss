;;; JEP-0078: Non-SASL authentication

(module auth "swindle.ss"
  (require "client.ss")
  (require "dom.ss")
  (require "iq.ss")
  (require "stream.ss")
  (require (lib "dom.ss" "dom"))
  (provide (all-defined))

  (define *auth-ns* "jabber:iq:auth")

  (defelementclass <auth-query> *auth-ns* "query")

  (defmethod (auth-fields (client <client>))
    (auth-fields (iq client 'get (make-auth-query client))))

  (defmethod (make-auth-query (client <client>)
                              &key username password digest resource)
    (let ((query (create-element-ns (document (initial-stream client))
                                    *auth-ns* "query")))
      (set! (attribute-ns query *xmlns-ns* "xmlns") *auth-ns*)
      query))
                       
  (defmethod (auth-fields (query <auth-query>))
    ;; TO DO: skip non-element child nodes
    (list-of (as <symbol> (local-name child)) (child <- each-child query)))
)
