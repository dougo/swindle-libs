(module named "../swindle.ss"
  (require "types.ss")
  (require "interfaces.ss")
  (require (only "exn.ss" raise-exn:dom))
  (require (only "extra.ss" valid-name?))

  (defmethod* (check-name (string <dom-string>))
    (unless (valid-name? string)
      (raise-exn:dom *invalid-character-err*
	"~v is not a valid XML name" string)))


  (defclass* <named> ()
    (node-name :accessor node-name :initarg :name))
  (provide set-node-name!)

  (defbeforemethod (initialize (named <named>) initargs)
    (check-name (getarg initargs :name)))

  (defmethod (print-object (node <named>) esc? port)
    (fprintf port "#<~a:~a>"
	     (name-sans-<> (class-name (interface-of node)))
	     (node-name node)))
)
