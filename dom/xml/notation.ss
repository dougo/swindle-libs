(module notation "../swindle.ss"
  (require "../core/types.ss")
  (require "../core/interfaces.ss")
  (require "interfaces.ss")
  (require (only "../core/named.ss" <named>))
  (require (only "../core/owned.ss" <owned>))
  (require (only "../core/contained.ss" <contained>))

  (defmethod (node-type (node <notation>)) *notation-node*)

  ;; TO DO: import-node


  (defclass* <notation-impl> (<named> <owned> <contained> <notation>)
    (public-id :reader public-id :initarg :public-id :initvalue #f)
    (system-id :reader system-id :initarg :system-id :initvalue #f))
)
