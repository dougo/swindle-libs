;;; JEP-0009: Jabber-RPC

#lang s-exp "swindle.ss"

(require "client.ss")
(require "dom.ss")
(require "iq.ss")
(require "jid.ss")
(require "stream.ss")
(require dom/dom)
(require (only net/base64 base64-decode))
(require (only srfi/19 date->string string->date))
(provide (all-defined))

(define *rpc-ns* "jabber:iq:rpc")
;; TO DO: identity = automation/rpc

;; Send a remote procedure call from client to target.
(defmethod (rpc (client <client>) (target <jid>) (method <symbol>) . params)
  (value (iq client 'set (make-rpc-request client method params) target)))


;; Override this to handle remote procedure calls to client from source.
;; TO DO: <rpc-handler> interface?
(defmethod (handle-rpc (client <client>) (source <jid>)
                       (method <symbol>) (params <list>))
  ;; TO DO: fault
  (void))


;; Serializing to RPC:

(defmethod (make-rpc-request (client <client>) (method <symbol>)
                             (params <list>))
  (xexpr->dom
   `(query
     ((xmlns ,*rpc-ns*))
     (methodCall
      (methodName ,(as <string> method))
      ,@(if (null? params)
            null
            `((params ,@(map rpc-param-xexpr params))))))
   (document (initial-stream client))))

(defmethod (make-rpc-response (client <client>) value)
  (xexpr->dom
   `(query
     ((xmlns ,*rpc-ns*))
     (methodResponse
      (params ,(rpc-param-xexpr value))))))

(defmethod (rpc-param-xexpr value)
  `(param ,(rpc-value-xexpr value)))

(defmethod (rpc-value-xexpr value)
  (cond ((symbol? value)
         (rpc-value-xexpr (symbol->string value)))
        ((vector? value)
         (rpc-value-xexpr (vector->list value)))
        (else
         `(value
           ,(cond ((int? value)
                   `(int ,(number->string value)))
                  ((string? value)
                   ;; TO DO: use Base64 if needed
                   `(string ,value))
                  ((number? value)
                   `(double ,(number->string value)))
                  ((boolean? value)
                   `(boolean ,(if value "1" "0")))
                  ((date? value)
                   `(dateTime.iso8601 ,(date->string value "~5")))
                  ((list? value)
                   `(array (data ,(map rpc-value-xexpr value))))
                  ((hash-table? value)
                   `(struct ,(hash-table-map value rpc-member-xexpr)))
                  (else
                   (error 'rpc "Unknown value type: ~v" value)))))))

(defmethod (rpc-member-xexpr (name <symbol>) value)
  `(member (name ,(as <string> name) ,(rpc-value-xexpr value))))

(defmethod (int? value)
  (and (integer? value) (exact? value) 
       (<= -2147483648 value 2147483647)))


;; Deserializing from RPC:

(defclass <rpc-element> (<jabber-element>))

(defsyntax defrpcelementclasses
  (syntax-rules ()
    ((_ (class-name tag-name) ...)
     (begin
       (defelementclass (class-name <rpc-element>) *rpc-ns* tag-name)
       ...))))

(defrpcelementclasses
  (<rpc-payload> "query")
  (<rpc-method-call> "methodCall")
  (<rpc-method-response> "methodResponse")
  (<rpc-params> "params")
  (<rpc-param> "param")
  (<rpc-value> "value")
  (<rpc-member> "member")
  (<rpc-data> "data"))

(defclass <rpc-value-data> (<rpc-element>))

(defsyntax defrpcvalueclasses
  (syntax-rules ()
    ((_ (class-name tag-name) ...)
     (begin
       (defelementclass (class-name <rpc-value-data>) *rpc-ns* tag-name)
       ...))))

(defrpcvalueclasses
  (<rpc-i4> "i4")
  (<rpc-int> "int")
  (<rpc-string> "string")
  (<rpc-double> "double")
  (<rpc-base64> "Base64")
  (<rpc-boolean> "boolean")
  (<rpc-date> "dateTime.iso8601")
  (<rpc-array> "array")
  (<rpc-struct> "struct"))

;; These methods generally assume that the RPC element is valid.  A
;; separate validation step might be better.

(defmethod (handle-iq-request (client <client>) (from <string>)
                              (payload <rpc-payload>))
  (let ((method-call (first-child-instance payload <rpc-method-call>)))
    ;; TO DO: handle exceptions, return faults.
    (make-rpc-response
     (handle-rpc client (as <jid> from)
                 (name method-call) (params method-call)))))

(defmethod (name (method-call <rpc-method-call>))
  (as <symbol> (text-field method-call 'methodName)))

(defmethod (params (method-call <rpc-method-call>))
  (let ((params (first-child-instance method-call <rpc-params>)))
    (if params
        (map value (child-instances method-call <rpc-param>))
        null)))

(defmethod (value (payload <rpc-payload>))
  (value (first-child-instance payload <rpc-method-response>)))

(defmethod (value (method-response <rpc-method-response>))
  (car (params method-response)))

(defmethod (params (method-response <rpc-method-response>))
  (let ((params (first-child-instance method-response <rpc-params>)))
    (map value (child-instances params <rpc-param>))))

(defmethod (value (param <rpc-param>))
  (value (first-child-instance param <rpc-value>)))

(defmethod (value (val <rpc-value>))
  (let ((data (first-child-instance val <rpc-value-data>)))
    (if data
        (value data)
        (text-content val))))

(defmethod (value (data <rpc-i4>))
  (string->number (text-content data)))
(defmethod (value (data <rpc-int>))
  (string->number (text-content data)))
(defmethod (value (data <rpc-string>))
  (text-content data))
(defmethod (value (data <rpc-double>))
  (string->number (text-content data)))
(defmethod (value (data <rpc-base64>))
  (base64-decode (text-content data)))
(defmethod (value (data <rpc-boolean>))
  (equals? "1" (text-content data)))
(defmethod (value (data <rpc-date>))
  (string->date (text-content data) "~Y~m~d~H~M~S"))

(defmethod (value (data <rpc-array>))
  (value (first-child-instance data <rpc-data>)))
(defmethod (value (data <rpc-data>))
  (map value (child-instances data <rpc-value>)))

(defmethod (value (data <rpc-struct>))
  (let ((ht (make-hash-table)))
    (for-each
     (lambda (member)
       (hash-table-put! ht (name member) (value member)))
     (child-instances data <rpc-member>))
    ht))
(defmethod (name (member <rpc-member>))
  (text-field member 'name))
(defmethod (value (member <rpc-member>))
  (value (first-child-instance member <rpc-value>)))


