;;; JEP-0045: Multi-User Chat

(module muc "swindle.ss"
  (require "client.ss")
  (require "dom.ss")
  (require "jid.ss")
  (require "presence.ss")
  (require (lib "dom.ss" "dom"))
  (provide (all-defined))

  (define *muc-ns* "http://jabber.org/protocol/muc")

  (defclass <muc> ()
    (client :type <client>)
    (room-id :type <string>)
    (service :type <string>)
    (nickname :type <string>
              :initializer (lambda args
                             ;; Default to the node-id of the user's JID.
                             (node-id (address (getarg args :client)))))
    :autoinitargs #t :autoaccessors :slot)

  (defmethod (address (muc <muc>))
    (make <jid> :node (room-id muc) :domain (service muc)
          :resource (nickname muc)))

  ;; The list of mucs in which the client is currently an occupant.
  (define *mucs* (make-hash-table 'weak))
  (defmethod (mucs (client <client>))
    (hash-table-get *mucs* client null))
  (defmethod (set-mucs! (client <client>) (mucs <list>))
    (hash-table-put! *mucs* client mucs))
  (defmethod (add-muc! (client <client>) (muc <muc>))
    (set-mucs! client (cons muc (mucs client))))
  (defmethod (remove-muc! (client <client>) (muc <muc>))
    (set-mucs! client (remove muc (mucs client))))

  (defmethod (occupant? (client <client>) (muc <muc>))
    (memq muc (mucs client)))
  (defmethod (occupied? (muc <muc>))
    (occupant? (client muc) muc))

  (defmethod (make-presence (muc <muc>) &key type)
    (let ((presence (make-presence (client muc) :to (address muc) :type type)))
      (append-child!/xexpr presence `(x ((xmlns ,*muc-ns*))))
      presence))

  (defmethod (enter (muc <muc>))
    (unless (occupied? muc)
      (send (client muc) (make-presence muc))
      (add-muc! (client muc) muc)))

  (defmethod (exit (muc <muc>))
    (when (occupied? muc)
      (send (client muc) (make-presence muc :type 'unavailable))
      (remove-muc! (client muc) muc)))

  ;; TO DO: handle presence, wait for own presence before adding/removing

  (defaftermethod (set-nickname! (muc <muc>) (nickname <string>))
    (when (occupied? muc)
      (send (client muc) (make-presence muc))))

  (defbeforemethod (shutdown (client <client>))
    (for-each exit (mucs client)))
)
