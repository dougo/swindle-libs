;;; Jabber IDs.

(module jid "swindle.ss"
  (provide (all-defined))

  (defclass <jid> ()
    (node-id :reader node-id :initarg :node :initvalue #f)
    (domain-id :reader domain-id :type <string> :initarg :domain)
    (resource-id :reader resource-id :initarg :resource :initvalue #f))

  (make-equals?-compare-class+slots <jid>)

  (add-as-method <jid> <string>
    (lambda (jid)
      (concat (if (node-id jid) (concat (node-id jid) "@") "")
	      (domain-id jid)
	      (if (resource-id jid) (concat "/" (resource-id jid)) ""))))

  (add-as-method <string> <jid>
    (lambda (s)
      (regexp-case s
	(("(?:(.+)@)?([^/]+)(?:/(.+))?" node domain resource)
	 (make <jid> :node node :domain domain :resource resource)))))

  (defmethod (same-entity? (j1 <jid>) (j2 <jid>))
    (and (equals? (node-id j1) (node-id j2))
	 (equals? (domain-id j1) (domain-id j2))))

  (defmethod (full? (jid <jid>))
    (resource-id jid))
  (defmethod (full (jid <jid>) (resource <string>))
    (make <jid> :node (node-id jid) :domain (domain-id jid) :resource resource))
  (defmethod (bare? (jid <jid>))
    (not (full? jid)))
)
