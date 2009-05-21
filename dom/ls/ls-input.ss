#lang swindle

(require "../core/interfaces.ss")
(require "../xml/interfaces.ss")
(require "interfaces.ss")

(defclass* <ls-input-impl> (<ls-input>)
  (character-stream :initvalue #f)
  (byte-stream :initvalue #f)
  (string-data :initvalue #f)
  (system-id :initvalue #f)
  (public-id :initvalue #f)
  (base-uri :initvalue #f)
  (encoding :initvalue #f)
  (certified-text? :initvalue #f)
  :autoaccessors :slot)

(defmethod (create-ls-input (dom <dom-implementation-ls>))
  (make <ls-input-impl>))
