(module stream "swindle.ss"
  (require (lib "dom.ss" "dom"))
  (require "dom.ss")
  (require "exn.ss")
  (require "read.ss")
  (provide (all-defined))

  (define *streams-ns* "http://etherx.jabber.org/streams")
  (define *client-ns* "jabber:client")

  (defclass <xml-stream> ()
    (port :accessor port :initarg :port)
    (document :accessor document)
    (log? :accessor log? :initarg :log? :initvalue #f))

  (defclass <output-stream> (<xml-stream>)
    (semaphore :reader semaphore :initializer (thunk (make-semaphore 1))))

  (defmethod (initialize (stream <output-stream>) initargs)
    (call-next-method)
    (set! (document stream) (make-xmpp-document . initargs))
    (let ((s (dom->string (document stream)))
	  (out (port stream)))
      ;; Replace the ending "/>" with ">".
      (display (substring s 0 (- (len s) 2)) out)
      (display ">" out)
      (flush-output out)))

  (defelementclass <stream> *streams-ns* "stream"
    to
    from
    (id :type <symbol>)
    (xml:lang :ns *xml-ns* :type <symbol>)
    version
    )

  (defmethod (make-xmpp-document
	      &key (stream-prefix "stream") (default-ns *client-ns*)
	      to from id xml:lang)
    (let* ((doc (create-document
		 *the-jabber-dom-implementation*
		 *streams-ns* (make-qname stream-prefix "stream") #f))
	   (stream (document-element doc)))
      (insert-before! doc (create-processing-instruction
			   doc "xml" "version='1.0'")
		      (first-child doc))
      (set! (attribute-ns stream *xmlns-ns* (make-qname "xmlns" stream-prefix))
	    *streams-ns*)
      (set! (attribute-ns stream *xmlns-ns* "xmlns")
            (as <dom-string> default-ns))
      (set! (to stream) to
	    (from stream) from
	    (id stream) id
	    (xml:lang stream) xml:lang)
      ;; TO DO: version (requires TLS/SASL)
      doc))

  (defmethod (send-element (stream <output-stream>) (element <element>))
    (call-with-semaphore
     (semaphore stream)
     (thunk
      (let ((out (port stream)))
	(when out
	  (write-dom element out)
	  (flush-output out)
	  (when (log? stream)
	    (append-child! (document-element (document stream)) element)))))))

  (defmethod (close (stream <output-stream>))
    (call-with-semaphore
     (semaphore stream)
     (thunk
      (let ((out (port stream)))
	(when out
	  (fprintf out "</~a>" (node-name (document-element (document stream))))
	  (flush-output out)
	  (close-output-port out)
	  (set! (port stream) #f))))))


  (defclass <stream-handler> ())
  (defmethod (handle-stream-exn (handler <stream-handler>) (exn <exn:fail>))
    ((error-display-handler) (exn-message exn) exn))
  (defmethod (handle-stanza-exn (handler <stream-handler>) (exn <exn:fail>)
                                (stanza <jabber-element>))
    ((error-display-handler) (exn-message exn) exn))
  (defmethod (handle-element (handler <stream-handler>) (elt <jabber-element>))
    (void))
  (defmethod (handle-close (handler <stream-handler>))
    (void))


  (defclass <input-stream> (<xml-stream>)
    (handler :accessor handler :initarg :handler :type <stream-handler>))

  (defmethod (initialize (stream <input-stream>) initargs)
    (call-next-method)
    (set! (document stream) (read-xmpp-document (port stream)))
    (let ((ns (namespace-uri (document-element (document stream)))))
      (unless (equals? ns *streams-ns*)
	(raise-exn:xmpp:stream
	 'invalid-namespace "invalid streams namespace: ~a" ns)))
    (thread (thunk (serve stream))))

  (defmethod (serve (stream <input-stream>))
    (when (port stream)
      (let ((element (receive-element stream)))
	(cond (element
	       (when (log? stream)
		 (append-child! (document-element (document stream)) element))
	       (with-handlers ((exn:fail?
				(lambda (exn)
				  (handle-stanza-exn
                                   (handler stream) exn element))))
		 (handle-element (handler stream) element))
	       (serve stream))
	      (else
	       (handle-close (handler stream)))))))

  (defmethod (receive-element (stream <input-stream>))
    (with-handlers ((exn:fail? 
		     (lambda (exn)
		       (close stream)
		       (handle-stream-exn (handler stream) exn))))
      (read-xmpp-element (port stream) (document stream))))

  (defmethod (close (stream <input-stream>))
    (close-input-port (port stream))
    (set! (port stream) #f))
)
