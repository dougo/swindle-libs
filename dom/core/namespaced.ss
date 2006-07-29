(module namespaced "../swindle.ss"
  (require "types.ss")
  (require "interfaces.ss")
  (require (only (lib "13.ss" "srfi") string-null? string-index))
  (require (only "exn.ss" raise-exn:dom))
  (require (only "named.ss" check-name <named> set-node-name!))
  (require (only "extra.ss" parse-qname well-formed-qname? well-formed-ncname?
		 *xml-ns* *xmlns-ns*))

  ;; A mixin class for nodes with a namespace.
  (defclass* <namespaced> ()
    (namespace-uri :reader namespace-uri :initarg :ns :type <dom-string>))

  (defmethod (prefix (x <namespaced>))
    (prefix (node-name x)))

  (defbeforemethod (set-prefix! (x <namespaced>) prefix)
    (check-prefix x prefix))

  (defaftermethod (set-prefix! (x <named>) prefix)
    (set! (node-name x) (if prefix
			    (as <dom-string> (concat prefix ":" (local-name x)))
			    (local-name x))))

  (defmethod (local-name (x <namespaced>))
    (local-name (node-name x)))


  (defmethod* (check-qname (qname <dom-string>))
    (check-name qname)
    (unless (well-formed-qname? qname)
      (raise-exn:dom *namespace-err*
	"~v is a malformed XML qualified name" qname)))

  (defmethod* (check-prefix (x <namespaced>) (prefix = #f)) (void))
  (defmethod (check-prefix (x <namespaced>) (prefix <dom-string>))
    (check-name prefix)
    (unless (well-formed-ncname? prefix)
      (raise-exn:dom *namespace-err*
	"~v is a malformed prefix" prefix))
    (let ((ns (namespace-uri x)))
      (when (and (equals? prefix "xml") (not (equals? ns *xml-ns*)))
	(raise-exn:dom *namespace-err*
	  "prefix ~v cannot have namespace URI ~v" prefix ns))))
  (defmethod (check-prefix (x <attr>) (prefix <dom-string>))
    (call-next-method)
    (let ((ns (namespace-uri x)))
      (when (and (equals? prefix "xmlns") (not (equals? ns *xmlns-ns*)))
	(raise-exn:dom *namespace-err*
	  "prefix ~v cannot have namespace URI ~v" prefix ns))
      (when (equals? (node-name x) "xmlns")
	(raise-exn:dom *namespace-err*
	  "cannot set prefix ~v for qualified name ~v" prefix "xmlns"))))

  (defmethod* (check-local-name (x <namespaced>) (local-name <dom-string>))
    (unless (well-formed-ncname? local-name)
      (raise-exn:dom *namespace-err*
	"~v is a malformed local name" local-name)))
)
