#lang swindle

(require "../swindle.ss")
(require "types.ss")
(require "interfaces.ss")
(require (only (lib "13.ss" "srfi") string-null?))

(defmethod (has-feature? (impl <dom-implementation>)
                         (feature <dom-string>) &opt version)
  #f)

(defclass* <dom-implementation-impl> (<dom-implementation>)
  (features :reader features :allocation :class
            :initializer (thunk (make-hash-table 'equal))))

(define* *the-dom-implementation* (make <dom-implementation-impl>))

(defmethod (has-feature? (impl <dom-implementation-impl>)
                         (feature <dom-string>) &opt version)
  (hash-table-get (features impl) 
                  (if (and version (not (string-null? version)))
                      (list feature version)
                      feature)
                  (thunk #f)))

(defmethod* (register-feature! (impl <dom-implementation-impl>)
                               (feature <dom-string>) (version <dom-string>))
  (hash-table-put! (features impl) (list feature version) #t)
  (hash-table-put! (features impl) feature #t))

(defmethod* (declare-feature (feature <dom-string>) (version <dom-string>))
  (register-feature! *the-dom-implementation* feature version))
