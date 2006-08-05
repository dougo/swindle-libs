;;; JEP-0078: Non-SASL authentication

(module auth "swindle.ss"
  (require "client.ss")
  (require "dom.ss")
  (require "exn.ss")
  (require "iq.ss")
  (require "jid.ss")
  (require "stream.ss")
  (require (lib "dom.ss" "dom"))
  (provide (all-defined))

  (define *auth-ns* "jabber:iq:auth")

  (defelementclass <auth-query> *auth-ns* "query")

  (defstruct <exn:xmpp:auth> (<exn:xmpp>))
  (defstruct <exn:xmpp:auth:unsupported> (<exn:xmpp:auth>))

  (defmethod (login (client <client>) (password <string>))
    (let ((fields (auth-fields client))
          (jid (address client)))
      (unless (auth-field fields 'password)
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
      (set! (attribute-ns query *xmlns-ns* "xmlns") *auth-ns*)
      (set! (auth-field query 'username) username)
      (set! (auth-field query 'password) password)
      (set! (auth-field query 'digest) digest)
      (set! (auth-field query 'resource) resource)
      query))
                       
  (defmethod (auth-fields (client <client>))
    (iq client 'get (make-auth-query client)))

  (defmethod (auth-field (query <auth-query>) (field-name <symbol>))
    (cond ((auth-field-node query field-name)
           => (lambda (node)
                (if (has-child-nodes? node)
                    (node-value (first-child node))
                    "")))
          (else #f)))

  (defmethod (auth-field-node (query <auth-query>) (field-name <symbol>))
    (find-if-iterator
     (lambda (child)
       (and (instance-of? child <element>)
            (eq? field-name (as <symbol> (local-name child)))))
     (each-child query)))

  (defmethod (set-auth-field! (query <auth-query>) (field-name <symbol>)
                              (field-value <string>))
    (cond ((auth-field-node query field-name)
           => (lambda (node)
                (set! (data (first-child node)) (as <dom-string> field-value))
;                (replace-whole-text! (first-child node)
;                                     (as <dom-string> field-value))
                ))
          (else
           (let ((node (create-element-ns
                        (owner-document query)
                        *auth-ns* (as <dom-string> field-name)))
                 (content (create-text-node
                           (owner-document query)
                           (as <dom-string> field-value))))
             (append-child! node content)
             (append-child! query node)))))

  (defmethod (set-auth-field! (query <auth-query>) (field-name <symbol>)
                              (field-value = #f))
    (cond ((auth-field-node query field-name)
           => (lambda (node) (remove-child! query node)))))
)
