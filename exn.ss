;;; Exceptions for stream and stanza errors.

(module exn "swindle.ss"
  (provide (all-defined))

  (define-struct (exn:xmpp exn:fail) (condition text))
  (define-struct (exn:xmpp:stream exn:xmpp) ())
  (define-struct (exn:xmpp:stanza exn:xmpp) (stanza))

  (defmethod (raise-stream-error (condition <symbol>) &opt format-string . args)
    (raise (make-stream-error condition format-string . args)))

  (defmethod (make-stream-error (condition <symbol>) &opt format-string . args)
    (let ((text (and format-string (format format-string . args))))
      (make-exn:xmpp:stream
       (as <immutable-string>
	   (echos :s- (if text condition 'stream-error) ": "
		  (or text condition)))
       (current-continuation-marks)
       condition text)))

  (defmethod (raise-stanza-error stanza (condition <symbol>)
				 &opt format-string . args)
    (raise (make-stanza-error stanza condition format-string . args)))

  (defmethod (make-stanza-error stanza (condition <symbol>)
				&opt format-string . args)
    (let ((text (and format-string (format format-string . args))))
      (make-exn:xmpp:stanza
       (as <immutable-string>
	   (echos :s- (if text condition 'stanza-error) ": "
		  (or text condition)))
       (current-continuation-marks)
       condition text stanza)))
)
