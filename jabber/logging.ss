;;; logging.ss -- Logging an input-port.

#lang s-exp "swindle.ss"

(require (only mzlib/port copy-port))
(provide (all-defined))

(define *logging-output-port* (current-error-port))
(defmethod (log-input-port (in <input-port>))
  (let-values (((pipe-in pipe-out) (make-pipe)))
    (thread (lambda ()
              (copy-port in *logging-output-port* pipe-out)
              (close-output-port pipe-out)
              (close-input-port in)))
    pipe-in))
;; TO DO: log to a buffer
;; TO DO: enable/disable switch



