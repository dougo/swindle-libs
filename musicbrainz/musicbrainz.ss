;; Interface to the MusicBrainz database via its XML Web service.
;; http://wiki.musicbrainz.org/XMLWebService

(module musicbrainz "swindle.ss"
  (require* "xml-http-request.ss")
  (require (only (lib "dom.ss" "dom") document-element dom->xexpr))
  (require (only (lib "url.ss" "net")
                 combine-url/relative string->url url url-query))
  (require (only (lib "struct.ss") copy-struct))
  (require (only (lib "pretty.ss") pretty-print))
  (provide (all-defined))

  (defclass <server> ()
    (base-url :initarg :url)
    :autoaccessors :slot)

  (define *test-server*
    (make <server> :url (string->url
                         "http://searchtest.musicbrainz.org/ws/1/")))

  ;; Append some query fields to a URL.
  (defmethod (combine-url/query base-url (new-query <list>))
    (copy-struct url base-url (url-query
                               (append (url-query base-url) new-query))))

  ;; Construct a URL for fetching a resource by ID.  type is either
  ;; artist, release, or track.  inc is a list of symbols determining
  ;; what fields to include in the result.
  (defmethod (resource-url (server <server>) (type <symbol>) (id <symbol>)
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

  ;; Search the releases collection given the artist name and release title.
  (defmethod (find-releases (server <server>) 
                            (artist <string>) (title <string>))
    ;; TO DO: convert XML document to list of releases.
    (xml-http-request
     (collection-url server 'release
                     `((artist . ,artist) (title . ,title)))))

  (defmethod (test &opt (title "Blur") (artist "Blur"))
    (pretty-print
     (dom->xexpr
      (document-element
       (find-releases *test-server* artist title)))))
)
