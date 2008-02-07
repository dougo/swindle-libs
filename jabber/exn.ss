;;; Exceptions for stream and stanza errors.

#lang s-exp "swindle.ss"

(provide (all-defined))

(defstruct <exn:xmpp> (<exn:fail>) condition text)
(defstruct <exn:xmpp:stream> (<exn:xmpp>))
(defstruct <exn:xmpp:stanza> (<exn:xmpp>) stanza)

(defmethod (condition (exn <exn:xmpp>))
  (exn:xmpp-condition exn))
(defmethod (condition (exn <exn:fail>))
  'internal-server-error)

(defmethod (text (exn <exn:xmpp>))
  (exn:xmpp-text exn))
(defmethod (text (exn <exn:fail>))
  (exn-message exn))

(defmethod (raise-exn:xmpp:stream (condition <symbol>)
                                  &opt format-string . args)
  (let ((text (and format-string (format format-string . args))))
    (raise 
     (make-exn:xmpp:stream
      (as <immutable-string>
          (echos :s- (if text condition 'stream-error) ": "
                 (or text condition)))
      (current-continuation-marks)
      condition text))))

(defmethod (raise-exn:xmpp:stanza stanza (condition <symbol>)
                                  &opt format-string . args)
  (let ((text (and format-string (format format-string . args))))
    (raise
     (make-exn:xmpp:stanza
      (as <immutable-string>
          (echos :s- (if text condition 'stanza-error) ": "
                 (or text condition)))
      (current-continuation-marks)
      condition text stanza))))

