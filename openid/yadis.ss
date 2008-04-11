;; yadis.ss -- Yadis resolution

;; Copyright 2008 Doug Orleans.  This program is distributed under the
;; terms of the GNU Affero General Public License.  See the file
;; COPYING for details.

#lang swindle

(provide (all-defined))

(require net/url)
(require (only xml read-xml))
(require (only net/head extract-field))
(require (planet "htmlprag.ss" ("neil" "htmlprag.plt" 1)))
(require "http.ss")
(require "html.ss")

;;; Yadis 1.0: http://yadis.org/wiki/Yadis_1.0_%28HTML%29

;; url -> (or document #f)
(defmethod (resolve-yadis-url yadis-url)
  (let (((values yadis-url yadis-resource-descriptor-url)
         (get-yadis-resource-descriptor-url yadis-url)))
    (if yadis-resource-descriptor-url
        (get-yadis-document yadis-resource-descriptor-url)
        (resolve-yadis-url-body yadis-url))))

;; url -> (or url #f)
(defmethod (get-yadis-resource-descriptor-url yadis-url)
  (call/input-url/follow-redirects
   yadis-url head-impure-port
   (lambda (url head in)
     (values url (cond ((extract-field "X-XRDS-Location" head)
                        => (lambda (loc) (combine-url/relative url loc)))
                       (else #f))))))

;; url -> (or document #f)
(defmethod (resolve-yadis-url-body yadis-url)
  (call/input-url/follow-redirects
   yadis-url get-impure-port
   (lambda (url head in)
     (if (string=? (extract-field "Content-Type" head) "application/xrds+xml")
         (read-yadis-document in)
         (cond ((read-yadis-resource-descriptor-url url in)
                => get-yadis-document)
               (else #f))))))

;; url input-port -> (or url #f)
(defmethod (read-yadis-resource-descriptor-url base-uri (in <input-port>))
  (let ((next (make-html-tokenizer in #t)))
    (let loop ()
      (let ((token (next)))
        (cond ((or (null? token)
                   (and (start-tag-token? token) (eq? 'body (tag token))))
               #f)
              ((and (start-tag-token? token) (eq? 'meta (tag token))
                    (equal? (attribute token 'http-equiv) "X-XRDS-Location"))
               (combine-url/relative base-uri (attribute token 'content)))
              (else
               (loop)))))))

;; url -> (or document #f)
(defmethod (get-yadis-document yadis-resource-descriptior-url)
  (call/input-url/follow-redirects
   yadis-resource-descriptior-url get-impure-port
   (lambda (url head in) (read-yadis-document in))))

;; input-port -> (or document #f)
(defmethod (read-yadis-document (in <input-port>))
  (read-xml in))
