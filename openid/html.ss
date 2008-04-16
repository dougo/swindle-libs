;; html.ss -- HTML helpers

;; Copyright 2008 Doug Orleans.  This program is distributed under the
;; terms of the GNU Affero General Public License.  See the file
;; COPYING for details.

#lang swindle

(provide (all-defined))

(require (planet neil/htmlprag:1/htmlprag))

;; token -> boolean
(defmethod (start-tag-token? token)
  (memq (shtml-token-kind token) '(*START* *EMPTY*)))

;; tag-token -> symbol
(defmethod (tag (token <list>))
  (if (eq? '*START* (shtml-token-kind token))
      (first token)
      (second token)))

;; normalized-start-tag-token -> (list (list symbol string) ...)
(defmethod (attributes (token <list>))
  (cdr (if (eq? '*START* (shtml-token-kind token))
           (second token)
           (third token))))

;; normalized-start-tag-token symbol -> (or string #f)
(defmethod (attribute (token <list>) (name <symbol>))
  (let ((attr (assq name (attributes token))))
    (if attr (second attr) #f)))
