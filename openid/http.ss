;; http.ss -- HTTP helpers

;; Copyright 2008 Doug Orleans.  This program is distributed under the
;; terms of the GNU Affero General Public License.  See the file
;; COPYING for details.

#lang swindle

(provide (all-defined))

(require net/url)
(require (only net/head extract-field))

;; The function call/input-url/follow-redirects is like
;; call/input-url, but it discards 1xx informational responses and
;; follows 3xx redirection responses.  The connect function should
;; take a URL and return an impure port.  The handle function should
;; take a URL, an HTTP response head (string), and a pure port.

(defmethod (call/input-url/follow-redirects
            url (connect <function>) (handle <function>))
  ;; TO DO: set a timeout to avoid tarpits
  (let loop ((url url))
    (let ((head #f) (redirect-url #f))
      (let (((connect url)
             (let ((in (connect url)))
               (let loop ()
                 (set! head (purify-port in))
                 (case (quotient (status-code head) 100)
                   ((1) (loop))
                   ((3) (set! redirect-url (redirection-url url head)) in)
                   (else in)))))
            ((handle in)
             (if redirect-url
                 (loop redirect-url)
                 (handle url head in))))
        (call/input-url url connect handle)))))

(defmethod (status-code (head <string>))
  (as <number> (second (regexp-match #rx" ([0-9][0-9][0-9])" head))))

(defmethod (redirection-url url (head <string>))
  (combine-url/relative url (extract-field "Location" head)))

