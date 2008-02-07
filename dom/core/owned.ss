#lang swindle

(require "../swindle.ss")
(require "interfaces.ss")
(require "exn.ss")

;; A mixin class for nodes that are owned by a document.
(defclass* <owned> ()
  (owner-document :accessor owner-document :initarg :document :initvalue #f))
(provide set-owner-document!)

(defmethod* (check-ownership (x1 <owned>) (x2 <owned>))
  (unless (eq? (owner-document x1) (owner-document x2))
    (raise-exn:dom *wrong-document-err*
      "~v and ~v were created from different documents" x1 x2)))

(defmethod (check-ownership (document <document>) (x <owned>))
  (when (owner-document x)
    (unless (eq? document (owner-document x))
      (raise-exn:dom *wrong-document-err*
        "~v was not created from ~v" x document))))

