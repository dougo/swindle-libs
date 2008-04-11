;; relying-party.ss -- OpenID 2.0 "Relying Party" functionality

;; Copyright 2008 Doug Orleans.  This program is distributed under the
;; terms of the GNU Affero General Public License.  See the file
;; COPYING for details.

#lang swindle

(provide (all-defined))

(require srfi/13)
(require net/url)
(require web-server/servlet)
(require (planet "htmlprag.ss" ("neil" "htmlprag.plt" 1)))

(require (only scheme for/list in-lines))
(require (only srfi/1 drop-right filter-map lset<=))
(require (only srfi/2 and-let*))
(require (only mzlib/struct copy-struct))
(require (only net/uri-codec alist->form-urlencoded))

(require "http.ss")
(require "html.ss")

(-defclass-auto-initargs- (:auto true))
(-defclass-autoaccessors-naming- :slot)

;;; The section headings below correspond to the OpenID Authentication 2.0
;;; specification: http://openid.net/specs/openid-authentication-2_0.html

;;; 3. Protocol Overview

(defmethod (authenticate (make-login-page <function>))
  ;; TO DO: optional arguments: non-interactive, use associations,
  ;; form customization, realm
  (let* (((values user-supplied-identifier http-request)
          ;; Step 1.
          (initiate-authentication make-login-page))
         (discovered-info
          ;; Step 2.
          (perform-discovery (normalize user-supplied-identifier)))
         ;; TO DO: Step 3. (optionally establish an association)
         (assertion
          ;; Step 4.
          (request-authentication (root-url http-request) discovered-info)))
    ;; (Steps 5 and 6 are performed by the OP.)
    (and (positive-assertion? assertion)
         ;; Step 7.
         (verify-assertion assertion))))

;;; 4. Data Formats

;;; 4.1 Protocol Messages

;; The parameters of a message are a list of (symbol . string)
;; key/value pairs.

(defclass <message> () parameters)

(defmethod (add-parameter (msg <message>) (key <symbol>) (value <string>))
  (make (class-of msg)
    :parameters (cons (cons key value)
                      (filter (lambda (pair) (not (eq? key (car pair))))
                              (parameters msg)))))

(add-ref-method <message>
  (lambda (msg key)
    (let ((pair (assq key (parameters msg))))
      (and pair (cdr pair)))))

;;; 4.1.1. Key-Value Form Encoding

(defmethod (read-key-value-form (in <input-port>))
  (make-message
   (for/list ((key-value (in-lines in 'linefeed))
              ;; There shouldn't be any blank lines, but LiveJournal
              ;; sometimes appends one.
              #:when (not (string-null? key-value)))
     (let ((colon (string-index key-value #\:)))
       (unless colon
         ;; TO DO: make exception structure
         (raise-user-error 'read-key-value-form
                           "Line has no colon: ~a" key-value))
       (cons (as <symbol> (string-take key-value colon))
             (string-drop key-value (+ colon 1)))))))

(defmethod (write-key-value-form (msg <message>)
                                 &opt (out (current-output-port)))
  (for-each (lambda (p) (fprintf out "~a:~a\n" (car p) (cdr p)))
            (parameters msg)))

;;; 4.1.2. HTTP Encoding

(defmethod (http-request->message request)
  (make-indirectly-received-message
   (filter-map
    (lambda (binding)
      (let ((key (as <string> (car binding))))
        (and (string-prefix? "openid." key)
             (cons (as <symbol> (string-drop key 7))
                   (cdr binding)))))
    (request-bindings request))
   (base-uri request)))

;; Return the absolute base URI of an HTTP request, including the authority.
(defmethod (base-uri http-request)
  (let ((relative-url (request-uri http-request)))
    (copy-struct
     url (root-url http-request)
     (url-path (url-path relative-url))
     (url-query (url-query relative-url)))))

;; Return a URL containing the scheme and authority of an HTTP request.
(defmethod (root-url http-request)
   ;; TO DO: what if https?
   (string->url (concat "http://" (request-authority http-request) "/")))

(defmethod (request-authority http-request)
   ;; TO DO: user
  (let ((host-ip (request-host-ip http-request))
        (host-port (request-host-port http-request)))
    (cond ((assq 'host (request-headers http-request)) => cdr)
          ;; TO DO: what if https?
          ((= host-port 80) host-ip)
          (else (format "~a:~a" host-ip host-port)))))

(defclass <indirectly-received-message> (<message>)
  request-url)

(defmethod (add-parameter (msg <indirectly-received-message>)
                          (key <symbol>) (value <string>))
  (let ((new-msg (call-next-method)))
    (set! (request-url new-msg) (request-url msg))
    new-msg))

(defmethod (->http-request-bindings (msg <message>))
  (map (lambda (binding)
         (cons (add 'openid. (car binding)) (cdr binding)))
       (parameters msg)))

(defmethod (->form-urlencoded (msg <message>))
  (alist->form-urlencoded (->http-request-bindings msg)))

(defmethod (make-message-url base-url (msg <message>))
  (copy-struct
   url base-url
   (url-query (append (->http-request-bindings msg)
                      (url-query base-url)))))

(defmethod (namespace (msg <message>))
  (ref msg 'ns))

;; message -> (or version false)
(defgeneric version) ;override scheme's version procedure
(defmethod (version (msg <message>))
  (let ((ns (namespace msg)))
    (findf (lambda (version) (member ns (namespaces version))) versions)))

(defclass <version> () namespaces)

(defmethod (namespace (version <version>))
  (car (namespaces version)))

(define versions null)

(define-syntax define-version
  (syntax-rules ()
    ((_ version namespace ...)
     (define version
       (let ((version (make-version (list namespace ...))))
         (set! versions (cons version versions))
         version)))))

(define-version version-2.0
  "http://specs.openid.net/auth/2.0")
(define-version version-1.1
  "http://openid.net/signon/1.1"
  "http://openid.net/signon/1.0"
  false)

(defmethod (mode (msg <message>))
  (ref msg 'mode))

;;; 4.2. Integer Representations

(defmethod (btwoc (n <integer>))
  (let loop ((n n) (bytes null))
    (if (and (zero? n) (not (null? bytes)) (< (car bytes) 128))
        (list->bytes bytes)
        (let-values (((q r) (quotient/remainder n 256)))
          (loop q (cons r bytes))))))

;;; 5. Communication Types

;;; 5.1. Direct Communication

(defmethod (communicate-directly (msg <message>) op-endpoint-url)
  ;; TO DO: handle errors, e.g. tcp-read
  (call/input-url/follow-redirects
   op-endpoint-url
   (lambda (url) (send-direct-request msg url))
   (lambda (url head in) (read-direct-response msg head in))))

;;; 5.1.1. Direct Request

(defmethod (send-direct-request (msg <message>) op-endpoint-url)
  (post-impure-port
   op-endpoint-url
   (string->bytes/latin-1 (->form-urlencoded msg))
   '("Content-Type: application/x-www-form-urlencoded")))

;;; 5.1.2. Direct Response

(defmethod (read-direct-response (msg <message>) (head <string>)
                                 (in <input-port>))
  (let ((status-code (status-code head)))
    (case status-code
      ((200) (read-key-value-form in))
      ;; TO DO: format error/contact/reference
      ((400) (raise (read-key-value-form in) #t))
      (else
       ;; TO DO: raise exception structure with head and body
       (error 'read-direct-response
              "Unexpected status code: ~a" status-code)))))

;;; 5.2. Indirect Communication

;; make-message should take a relative URL string to return to and
;; return a <message>, which is sent indirectly to the recipient URL.

(defmethod (communicate-indirectly (make-message <function>) recipient-url)
  ;; TO DO: let user choose between redirect and form redirect
  (check-indirect-response
   (http-request->message
    (send/suspend
     (lambda (k-url)
       (redirect (make-message k-url) recipient-url))))))

;;; 5.2.1. HTTP Redirect

(defmethod (redirect (msg <message>) recipient-url)
  (redirect-to (url->string (make-message-url recipient-url msg))))

;;; 5.2.2. HTTP Form Redirect

;; TO DO

;;; 5.2.3. Indirect Error Responses

(defmethod (check-indirect-response (msg <message>))
  (if (eq? 'error (mode msg))
      ;; TO DO: format error/contact/reference
      (raise msg #t)
      msg))

;;; 6. Generating Signatures

;; TO DO

;;; 7. Initiation and Discovery

;;; 7.1. Initiation

;; This presents a login page to the end user and returns the
;; user-supplied identifier, as well as the HTTP request that was
;; submitted.

(defmethod (initiate-authentication (make-login-page <function>))
  (let ((http-request
         (send/suspend
          (lambda (k-url)
            (make-login-page (make-end-user-form k-url))))))
    (values
     (extract-binding/single 'openid_identifier (request-bindings http-request))
     http-request)))

(defmethod (make-end-user-form (k-url <string>)
                               &opt (icon-url "/images/openid-icon-small.gif"))
  `(form ((action ,k-url) (method "post"))
         (input ((type "text") (name "openid_identifier")
                 (style ,(format user-input-style icon-url)))
         (input ((type "submit") (value "Login"))))))

(define user-input-style
  "background: #FFFFFF url(~s) no-repeat scroll 0pt 50%; padding-left: 18px;")

;;; 7.2. Normalization

;; This does not include step 4 (following redirects)-- that happens
;; in discovery if the identifier is a URL.

(defmethod (normalize (user-supplied-identifier <string>))
  ;; TO DO: strip off "xri://"
  ;; TO DO: look for XRI Global Context Symbol or "("
  ;; TO DO: handle errors
  (normalize-url
   (string->url
    (if (regexp-match #rx"^https?:" user-supplied-identifier)
        user-supplied-identifier
        (concat "http://" user-supplied-identifier)))))

(defmethod (normalize-url u)
  ;; TO DO: reject non-http[s] schemes?
  ;; TO DO: error if host is not FQDN
  (copy-struct
   url u
   (url-path-absolute? #t)
   (url-path
    (if (null? (url-path u))
        ;; Add trailing slash.
        (list (make-path/param "" null))
        (remove-dot-segments (url-path u))))
   ;; Remove fragment part.
   (url-fragment #f)))

;; RFC 3986 (URI) 5.2.4. Remove Dot Segments
;; (list path/param ...) -> (list path/param ...)
(defmethod (remove-dot-segments (path <list>))
  (let loop ((path path) (output-path null))
    (if (null? path)
        (reverse output-path)
        (let ((segment (car path)))
          (loop (cdr path)
                (case (path/param-path segment)
                  ((same) output-path)
                  ((up) (if (null? output-path) null (cdr output-path)))
                  (else (cons segment output-path))))))))

;;; 7.3 Discovery

(defmethod (perform-discovery normalized-identifier)
  ;; TO DO: XRI
  ;; TO DO: Yadis
  (call/input-url/follow-redirects
   normalized-identifier get-impure-port
   (lambda (claimed-identifier head pure-port)
     (if (= 2 (quotient (status-code head) 100))
         (perform-html-based-discovery claimed-identifier pure-port)
         ;; TO DO: make exception structure
         (error 'perform-discovery head)))))

;;; 7.3.1. Discovered Information

(defclass <discovered-information> ()
  op-endpoint-url
  protocol-version
  claimed-identifier
  op-local-identifier)

;;; 7.3.2. XRDS-Based Discovery

;; TO DO

;;; 7.3.3. HTML-Based Discovery

;; An openid2.provider link type takes precedence, even if it appears
;; later in the document head than openid.server.

(defmethod (perform-html-based-discovery claimed-identifier (in <input-port>))
  (let ((next (make-html-tokenizer in #t))
        (provider #f)
        (local_id #f)
        (server #f)
        (delegate #f))
    ;; TO DO: use parse-html/tokenizer, sxpath
    (let loop ()
      (let ((token (next)))
        (if (or (and provider local_id)
                (null? token)
                (and (start-tag-token? token) (eq? 'body (tag token))))
            (cond (provider
                   (make-discovered-information
                    provider signon-2.0 claimed-identifier local_id))
                  (server
                   (make-discovered-information
                    server signon-1.1 claimed-identifier delegate))
                  ;; TO DO: make exception structure
                  (else (raise-user-error
                         'perform-html-based-discovery
                         "No valid OP Endpoint URL found at: ~a"
                         (url->string claimed-identifier))))
            (begin
              (and-let* (((start-tag-token? token))
                         ((eq? 'link (tag token)))
                         (href (attribute token 'href))
                         (url (maybe-string->url href))
                         (rel (attribute token 'rel)))
                (for-each
                 (lambda (link-type)
                   (case (string->symbol link-type)
                     ((openid2.provider)
                      (set! provider url))
                     ((openid2.local_id)
                      (set! local_id url))
                     ((openid.server)
                      (set! server url))
                     ((openid.delegate)
                      (set! delegate url))))
                 (string-tokenize rel)))
              (loop)))))))

(define signon-2.0 (string->url "http://specs.openid.net/auth/2.0/signon"))
(define signon-1.1 (string->url "http://openid.net/signon/1.1"))

;; string -> (or url #f)
(defmethod (maybe-string->url (str <string>))
  (with-handlers ((exn:fail? not))
    (string->url str)))

;;; 8. Establishing Associations

;; TO DO

;;; 9. Requesting Authentication

(defmethod (request-authentication sender-url (info <discovered-information>))
  ;; TO DO: allow extension parameters
  (make-assertion
   (communicate-indirectly
    (lambda (k-url)
      (make-authentication-request
       (combine-url/relative sender-url k-url)
       ;; TO DO: only remove the last one or two path segments of return-url?
       sender-url info))
    (op-endpoint-url info))
   info))

;;; 9.1. Request Parameters

;; return-url must be absolute, including the host, and realm-url must
;; be a prefix with no fragment (possibly with a wild-card at the
;; beginning of the host).

(defmethod (make-authentication-request
            return-url realm-url (info <discovered-information>))
  (with-accessors info (claimed-identifier op-local-identifier)
    (make-message
     ;; TO DO: should we use the protocol-version as the namespace?
     `((ns . ,(namespace version-2.0))
       (mode . "checkid_setup")
       ;; TO DO: identifier_select
       (claimed_id . ,(url->string claimed-identifier))
       (identity . ,(url->string (or op-local-identifier
                                     claimed-identifier)))
       (return_to . ,(url->string return-url))
       (realm . ,(url->string realm-url))
       ;; 1.1 compatibility:
       (trust_root . ,(url->string realm-url))))))

;;; 9.2. Realms

;; TO DO

;;; 9.3. Immediate Requests

;; TO DO

;;; 10. Responding to Authentication Requests

(defgeneric message) ;override swindle's message procedure
(defclass <assertion> () message discovered-info)

(set! make-assertion (generic)) ;override automaker
(defmethod (make-assertion (msg <message>) (info <discovered-information>))
  (make (assertion-class msg) :message msg :discovered-info info))

(defmethod (assertion-class (msg <message>))
  (ref assertion-classes (mode msg)
       (lambda ()
         ;; TO DO: make exception structure
         (error 'assertion-class "Unknown mode: ~a" (mode msg)))))

(define assertion-classes (make-hash-table 'equal))

(define-syntax define-assertion-class
  (syntax-rules ()
    ((_ name mode (direct-super ...) slot ...)
     (begin
       (defclass name (direct-super ...) slot ...)
       (set! (ref assertion-classes mode) name)))))

(defmethod (parameters (assertion <assertion>))
  (parameters (message assertion)))

(add-ref-method <assertion>
  (lambda (assertion . indexes)
    (apply ref (message assertion) indexes)))

(defmethod (request-url (assertion <assertion>))
  (request-url (message assertion)))

(defmethod (request-url (message <message>)) #f)

;; TO DO: unsolicited positive assertions

;;; 10.1. Positive Assertions

(define-assertion-class <positive-assertion> "id_res" (<assertion>))

;;; 10.2. Negative Assertions

(defclass <negative-assertion> (<assertion>))

;;; 10.2.1. In Response to Immediate Requests

(define-assertion-class <temporary-negative-assertion> "setup_needed"
  (<negative-assertion>))

;;; 10.2.2. In Response to Non-Immediate Requests

(define-assertion-class <definitive-negative-assertion> "cancel"
  (<negative-assertion>))

;;; 11. Verifying Assertions

;; TO DO: should this raise exceptions to distinguish the failure cases?
(defmethod (verify-assertion (assertion <positive-assertion>))
  (and (verify-return-url assertion)
       (verify-discovered-information assertion)
       (check-nonce assertion)
       (verify-signature assertion)
       (make-authentication
        (claimed-identifier assertion)
        (extension-parameters assertion))))

(defclass <authentication> () claimed-identifier extension-parameters)

;;; 11.1. Verifying the Return URL

(defmethod (verify-return-url (assertion <positive-assertion>))
  (define (url-scheme-authority-and-path u)
    (url->string (copy-struct url u (url-query null) (url-fragment #f))))
  (let ((return-url (string->url (ref assertion 'return_to)))
        (assertion-url (request-url assertion)))
    (and (string=? (url-scheme-authority-and-path return-url)
                   (url-scheme-authority-and-path assertion-url))
         (lset<= equal? (url-query return-url) (url-query assertion-url)))))

;;; 11.2. Verifying Discovered Information

(defmethod (verify-discovered-information (assertion <positive-assertion>))
  (let ((claimed-identifier-in-assertion (ref assertion 'claimed_id))
        (info (discovered-info assertion)))
    (or (not claimed-identifier-in-assertion)
        (let ((claimed-identifier-in-assertion
               (copy-struct
                url (string->url claimed-identifier-in-assertion)
                (url-fragment #f))))
          ;; If the Claimed Identifier was not previously discovered,
          ;; perform discovery to make sure the OP is authorized to make
          ;; assertions about the Claimed Identifier.
          (unless (claimed-identifier info)
            (set! info (perform-discovery claimed-identifier-in-assertion))
            (set! (discovered-info assertion) info))
          ;; TO DO: XRDS discovery info must be in one <xrd:Service> element
          (and (url=? (claimed-identifier info)
                      claimed-identifier-in-assertion)
               (url=? (op-local-identifier info)
                      (string->url (ref assertion 'identity)))
               (url=? (op-endpoint-url info)
                      (string->url (ref assertion 'op_endpoint)))
               (url=? (protocol-version info)
                      (string->url (ref assertion 'ns))))))))

(defmethod (url=? url1 url2)
  (and (url? url1) (url? url2)
       (string=? (url->string url1) (url->string url2))))

;;; 11.3. Checking the Nonce

(defmethod (check-nonce (assertion <positive-assertion>))
  ;; TO DO
  #t)

;;; 11.4. Verifying Signatures

(defmethod (verify-signature (assertion <positive-assertion>))
  (verify-directly assertion))

;; 11.4.1. Verifying with an Association

;; TO DO

;; 11.4.2. Verifying Directly with the OpenID Provider

(defmethod (verify-directly (assertion <positive-assertion>))
  (let ((response
         (communicate-directly
          (add-parameter (message assertion) 'mode "check_authentication")
          (op-endpoint-url (discovered-info assertion)))))
    ;; TO DO: check invalidate_handle
    (equal? "true" (ref response 'is_valid))))

;;; 11.5. Identifying the end user

(defmethod (claimed-identifier (assertion <positive-assertion>))
  (cond ((ref assertion 'claimed_id) => string->url)
        (else (claimed-identifier (discovered-info assertion)))))

;;; 12. Extensions

(defmethod (extension-parameters (assertion <positive-assertion>))
  (filter (lambda (parm) (not (memq (car parm) disallowed-aliases)))
          (parameters (message assertion))))

(define disallowed-aliases
  '(assoc_handle
    assoc_type
    claimed_id
    contact
    delegate
    dh_consumer_public
    dh_gen
    dh_modulus
    error
    identity
    invalidate_handle
    mode
    ns
    op_endpoint
    openid
    realm
    reference
    response_nonce
    return_to
    server
    session_type
    sig
    signed
    trust_root))


;;; 13. Discovering OpenID Relying Parties

;; TO DO

