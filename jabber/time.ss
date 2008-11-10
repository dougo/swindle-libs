;; JEP-0090: Entity Time

#lang s-exp "swindle.ss"

(require "client.ss")
(require "dom.ss")
(require "iq.ss")
(require "stream.ss")
(require-dom)
(provide (all-defined))

(define *time-ns* "jabber:iq:time")

;; Ask a Jabber entity what it thinks the time is.  The return value
;; is a <time-query> object; use the utc function to access the UTC
;; time string.
(defmethod (query-time (client <client>) &opt (entity (hostname client)))
  (iq client 'get (make-time-query client) entity))

;; Keep an XMPP connection alive by asking it for the time every n
;; seconds (10 minutes by default).
(defmethod (keep-alive (client <client>) &opt (n 600))
  (thread (rec loop
            (thunk
             (sleep n)
             ;; TO DO: check that client is still connected
             (query-time client)
             (loop)))))

(defelementclass <time-query> *time-ns* "query")

(defmethod (make-time-query (client <client>))
  (xexpr->dom `(query ((xmlns ,*time-ns*)))
              (document (initial-stream client))))

(defmethod (utc (reply <time-query>))
  (text-field reply 'utc))

;; These are optional:

(defmethod (tz (reply <time-query>))
  (text-field reply 'tz))
(defmethod (time-display (reply <time-query>))
  (text-field reply 'display))

