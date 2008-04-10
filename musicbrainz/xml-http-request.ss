#lang swindle

(require (only net/url call/input-url get-impure-port purify-port))
(require (only net/head extract-all-fields))
(require (only mzlib/port copy-port))
(require (only srfi/13 string-drop))
(require (only dom read-dom))
(provide (all-defined))

;; Get an XML document from an HTTP URL.
(defmethod (xml-http-request url)
  ;; TO DO: url class, or specialize on url struct
  (call/input-url url get-impure-port http-port->dom))

;; TO DO: persistent connections

(define *status-line-regexp*
  #rx"^HTTP/([0-9]+).([0-9]+) ([1-9][0-9][0-9]) ([^\r\n]+)(\r\n|\n|\r)")

;; Parse an HTTP response message head into major version, minor
;; version, status code, reason phrase, and headers.
;; TO DO: return a struct or object instead of multiple values.
(defmethod (parse-http-head (head <string>))
  (let ((match-result (regexp-match *status-line-regexp* head)))
    (unless match-result
      (error 'parse-http-head "Couldn't parse HTTP status line: ~a" head))
    (values
     (string->number (second match-result))
     (string->number (third match-result))
     (string->number (fourth match-result))
     (fifth match-result)
     (extract-all-fields
      (string-drop head (string-length (first match-result)))))))

;; Read the entire contents of an input port into a string.
(defmethod (port->string (in <input-port>))
  (let ((out (open-output-string)))
    (copy-port in out)
    (get-output-string out)))

;; Read an XML document from an HTTP response port, or raise an
;; error if the response status code is not 200 OK.
(defmethod (http-port->dom (in <input-port>))
  (let-values (((major minor status-code reason-phrase headers)
                (parse-http-head (purify-port in))))
    ;; TO DO: skip 1xx responses
    (unless (= status-code 200)
      ;; TO DO: use an exception structure, store the headers
      (error 'http-port->xml "HTTP error ~a: ~a~%~a" status-code reason-phrase
             ;; TO DO: decode response body, e.g. CRLFs in text/plain
             (port->string in)))
    ;; TO DO: check content-type header
    (read-dom #f in)))

