(module child "../swindle.ss"
  (require "interfaces.ss")

  ;; A mixin class for nodes that can have a parent and siblings.
  (defclass* <child> ()
    (parent-node :initvalue #f)
    (previous-sibling :initvalue #f)
    (next-sibling :initvalue #f)
    :autoaccessors :slot)

  (defmethod* (set-siblings! parent prev . nexts)
    (when prev (set! (parent-node prev) parent))
    (unless (null? nexts)
      (let ((next (car nexts)))
	(when prev (set! (next-sibling prev) next))
	(when next (set! (previous-sibling next) prev))
	(apply set-siblings! parent next (cdr nexts)))))
)
