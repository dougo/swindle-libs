#lang swindle

(require (only "classes.ss" <server> base-url))
(require (only net/url combine-url/relative url url-query))
(require (only mzlib/struct copy-struct))
(provide (all-defined))

;; Append some query fields to a URL.
(defmethod (combine-url/query base-url (new-query <list>))
  (copy-struct url base-url (url-query
                             (append (url-query base-url) new-query))))

;; Construct a URL for fetching a resource by ID.  type is either
;; artist, release, or track.  inc is a list of symbols determining
;; what fields to include in the result.
(defmethod (resource-url (server <server>) (type <symbol>) (id <string>)
                         (inc <list>))
  (combine-url/query
   (combine-url/relative (base-url server) (format "~a/~a" type id))
   `((type . "xml") (inc . ,(echos . inc)))))

;; Construct a URL for searching for resources in a collection.
;; type is either artist, release, or track.  filters is an alist of
;; filter parameter names (symbols) and values (strings).
(defmethod (collection-url (server <server>) (type <symbol>)
                           (filters <list>))
  (combine-url/query
   (combine-url/relative (base-url server) (format "~a/" type))
   `((type . "xml") ,@filters)))
