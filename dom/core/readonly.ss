(module readonly "../swindle.ss"
  (require "interfaces.ss")
  (require "exn.ss")

  (defmethod* (check-readonly x)
    (when (readonly? x)
      (raise-exn:dom *no-modification-allowed-err*
	"~v is readonly" x)))

  (defmethod* (readonly? x) #f)
)
