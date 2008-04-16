;; url.ss -- plug-in replacement for net/url with HTTPS support.

;; Copyright 2008 Doug Orleans.  This program is distributed under the
;; terms of the GNU Affero General Public License.  See the file
;; COPYING for details.

#lang scheme

(require scheme/unit)
(require net/url-sig)
(require net/url-unit)
(require net/tcp-sig)
(require net/tcp-unit)
(require net/ssl-tcp-unit)
(require (only-in net/url url-scheme))

(define-unit-binding ssl-tcp@
  (make-ssl-tcp@ #f #f #f #f #f #f #f)
  (import) (export tcp^))

(define-syntax define-wrapped-procs
  (syntax-rules ()
    ((_ sig unit (proc dom ...) ...)
     (begin
       (require (except-in net/url proc ...))
       (provide (all-from-out net/url))
       (define-signature sig (proc ...))
       (define-unit unit
         (import (tag tcp (prefix tcp: url^))
                 (tag ssl-tcp (prefix ssl-tcp: url^)))
         (export sig)
         (define/wrapped proc)
         ...)
       (provide/contract
        (proc ((url? dom ...) ((listof string?)) . ->* . input-port?))
        ...)))))

(define-wrapped-procs wrapped^ wrapped@
  (get-pure-port)
  (get-impure-port)
  (post-pure-port (or/c false/c bytes?))
  (post-impure-port bytes?)
  (head-pure-port)
  (head-impure-port)
  (delete-pure-port)
  (delete-impure-port)
  (put-pure-port (or/c false/c bytes?))
  (put-impure-port bytes?))

(define-compound-unit wrapped+urls@
  (import) (export WRAPPED)
  (link (((TCP : url^))
         (compound-unit/infer
          (import) (export url^)
          (link tcp@ url@)))
        (((SSL-TCP : url^))
         (compound-unit/infer
          (import) (export url^)
          (link ssl-tcp@ url@)))
        (((WRAPPED : wrapped^))
         wrapped@ (tag tcp TCP) (tag ssl-tcp SSL-TCP))))

(define-values/invoke-unit/infer wrapped+urls@)

(define-for-syntax (symbol-append . symbols)
  (string->symbol (apply string-append (map symbol->string symbols))))

(define-for-syntax (syntax-prefix sym stx)
  (datum->syntax stx (symbol-append sym (syntax-e stx))))

(define-syntax (define/wrapped stx)
  (syntax-case stx ()
    ((define/wrapped name)
     (with-syntax ((tcp:name (syntax-prefix 'tcp: #'name))
                   (ssl-tcp:name (syntax-prefix 'ssl-tcp: #'name)))
       (syntax
        (define (name url . args)
          (apply (if (ssl-tcp-scheme? url) ssl-tcp:name tcp:name)
                 url args)))))))

(define (ssl-tcp-scheme? url)
  (equal? "https" (url-scheme url)))
