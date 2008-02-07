;; Interface to the MusicBrainz database via its XML Web service.
;; http://wiki.musicbrainz.org/XMLWebService

#lang swindle

(require (only "xml-http-request.ss" xml-http-request))
(require (only "url.ss" resource-url collection-url))
(require (only "parse.ss" parse-document))
(require (only net/url string->url))
(require "classes.ss")
(provide (all-from "classes.ss"))
(provide (all-defined))

(define *server*
  (make <server> :url (string->url "http://musicbrainz.org/ws/1/")))

(defmethod (mmd-request url)
  ;; TO DO: url class, or specialize on url struct
  (parse-document (xml-http-request url)))

(defmethod (get-resource (server <server>) (type <symbol>) (id <string>)
                         (inc <list>))
  (mmd-request (resource-url server type id inc)))

(defmethod (get-artist (server <server>) (id <string>) (inc <list>))
  (get-resource server 'artist id inc))
(defmethod (get-release (server <server>) (id <string>) (inc <list>))
  (get-resource server 'release id inc))


(defmethod (match-artists (server <server>) (name <string>))
  (mmd-request (collection-url server 'artist `((name . ,name)))))

;; Search the releases collection given the artist name and release title.
(defmethod (match-releases (server <server>) 
                           (artist <string>) (title <string>))
  (mmd-request (collection-url server 'release `((artist . ,artist)
                                                 (title . ,title)))))
