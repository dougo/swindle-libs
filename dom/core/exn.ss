(module exn "../swindle.ss"
  (require "interfaces.ss")

  (defgeneric* (raise-exn:dom code message . args))
  (defgeneric* (exn:dom-args exn))

  (let ((exn? exn:dom?) (get-code exn:dom-code) (get-args exn:dom-args))
    (define-struct (exn:dom exn:fail) (code args))
    (defmethod (exn? exn)
      (exn:dom? exn))
    (defmethod (get-code exn)
      (exn:dom-code exn))
    (defmethod (get-args exn)
      (exn:dom-args exn))
    (defmethod (raise-exn:dom (code <exception-code>) (message <string>) . args)
      (raise (make-exn:dom
	      (as <immutable-string>
		  (format "DOM exception ~a: ~a" code
			  (format message . args)))
	      (current-continuation-marks)
	      code args))))
)
