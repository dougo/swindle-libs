#lang swindle

(require "../swindle.ss")
(require (only "readonly.ss" readonly?))

;; Mixin class for objects that can be contained in containers, such
;; as a <named-node-map> (which itself is contained in a <node>).
(defclass* <contained> ()
  (container :accessor container :initarg :container :initvalue #f))
(provide container set-container!)

(defmethod (readonly? (x <contained>))
  (readonly? (container x)))

