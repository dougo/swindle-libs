#lang swindle

(require "../swindle.ss")
(require "types.ss")
(require "interfaces.ss")
(require (only "../ls/interfaces.ss" <dom-implementation-ls>))

(defmethod (has-feature? (impl <dom-implementation>)
                         (feature <dom-string>) &opt version)
  #f)

(defclass* <dom-implementation-impl> (<dom-implementation>
				      <dom-implementation-ls>)
  (features :reader features :allocation :class
            :initializer (thunk (make-hash-table 'equal))))

(define* *the-dom-implementation* (make <dom-implementation-impl>))

(defmethod (has-feature? (impl <dom-implementation-impl>)
                         (feature <dom-string>) &opt version)
  (let ((feature (as <dom-string> (regexp-replace "^\\+" feature ""))))
    (hash-table-get (features impl)
		    (if (and version (not (string=? "" version)))
			(list feature version)
			feature)
		    (thunk #f))))

(defmethod (feature (impl <dom-implementation>) (feature <dom-string>)
		    &opt version)
  (and (has-feature? impl feature version) impl))

(defmethod* (register-feature! (impl <dom-implementation-impl>)
                               (feature <dom-string>) (version <dom-string>))
  (hash-table-put! (features impl) (list feature version) #t)
  (hash-table-put! (features impl) feature #t))

(defmethod* (declare-feature (feature <dom-string>) (version <dom-string>))
  (register-feature! *the-dom-implementation* feature version))
