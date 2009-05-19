#lang swindle

(require (only scheme curryr))
(require "../swindle.ss")
(require "types.ss")
(require "interfaces.ss")
(require (only "collections.ss" <dom-implementation-list-impl>))

(defclass* <dom-implementation-registry> ()
  (sources :initvalue null)
  :autoaccessors :slot)

(defmethod* (add-source! (registry <dom-implementation-registry>)
			 (source <dom-implementation-source>))
  (unless (memq source (sources registry))
    (set! (sources registry) (cons source (sources registry)))))

(define* *the-dom-implementation-registry* (make <dom-implementation-registry>))

(defmethod (dom-implementation (registry <dom-implementation-registry>)
			       (features <dom-string>))
  (some (curryr dom-implementation features) (sources registry)))

(defmethod (dom-implementation-list (registry <dom-implementation-registry>)
				    (features <dom-string>))
  (make <dom-implementation-list-impl>
    (as <vector>
	(list-of impl
	  (source <- (sources registry))
	  (impl <- (each-elt (dom-implementation-list source features)))))))
