;;; Reading from XML streams as defined by XMPP.

#lang s-exp "swindle.ss"

(require (only srfi/13 string-prefix?))
(require "dom.ss")
(require "exn.ss")
(require-dom)
(provide (all-defined))

;; Read from an input port until the first start tag.  Return a
;; document, as if the tag were empty, including any nodes that show
;; up before it.
(defmethod (read-xmpp-document (in <input-port>))
  (let ((prolog (open-output-string)))
    (let/ec return
      (loop-for (tag <- (thunk (read-tag in)))
        (when (start-tag? tag)
          (display (concat "<" tag "/>") prolog)
          (return (parameterize ((xml:read-comments #t))
                    (string->dom (get-output-string prolog)
                                 *the-jabber-dom-implementation*))))
        (display (concat "<" tag ">") prolog))
      ;; loop ends at EOF
      (raise-exn:xmpp:stream
       'xml-not-well-formed "stream ended prematurely"))))

;; Try to read an element from an input port (after any whitespace)
;; and return it as an <element> owned by the given document.  If
;; the stream end tag is read, return #f.  Otherwise raise a stream
;; exception.
(defmethod (read-xmpp-element (in <input-port>) (doc <document>))
  (skip-whitespace in)
  (cond ((equal? (peek-string 2 0 in) "</")
         (unless (equals? (substring (read-tag in) 1)
                          (node-name (document-element doc)))
           (raise-exn:xmpp:stream
            'xml-not-well-formed "end tag doesn't match"))
         ;; TO DO: read until eof
         (close-input-port in)
         #f)
        ((eof-object? (peek-char in))
         (close-input-port in)
         (raise-exn:xmpp:stream
          'xml-not-well-formed "stream ended prematurely"))
        (else
         ;; TO DO: validate
         (read-dom/element doc in))))


;; Read the next tag from an input port, skipping any leading
;; whitespace.  Return the contents of the tag, i.e. without the < >.
(defmethod (read-tag (in <input-port>))
  (skip-whitespace in)
  (let ((char (read-char in)))
    (unless (equals? char #\<)
      (let ((avail (make-bytes (error-print-width))))
        (read-bytes-avail!* avail in)
        (raise-exn:xmpp:stream
         'xml-not-well-formed (echos "not a tag:" :s- char avail)))))
  (list->string
   (list-of char (char <- each-char in) until (equals? char #\>))))

(defmethod (start-tag? (tag <string>))
  (not (or (pi-tag? tag) (comment-tag? tag))))
(defmethod (pi-tag? (tag <string>))
  (string-prefix? "?" tag))
(defmethod (comment-tag? (tag <string>))
  (string-prefix? "!--" tag))

;; Read and discard whitespace characters from an input port.
;; Return the first non-whitespace character read.
(defmethod (skip-whitespace (in <input-port>))
  (loop-for (char <- (peek-char in) while (xml-whitespace-char? char))
    (read-char in)))

(defmethod (xml-whitespace-char? v)
  (memv v '(#\space #\tab #\newline #\return)))

;; Iterator constructor for reading characters from an input port.
(defmethod (each-char (in <input-port>))
  (list in identity (lambda (in) (eof-object? (peek-char in))) read-char))

