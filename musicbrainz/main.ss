;; Interface to the MusicBrainz database via its XML Web service.
;; http://wiki.musicbrainz.org/XMLWebService

#lang swindle

(require (only "xml-http-request.ss" xml-http-request))
(require (only "url.ss" resource-url collection-url))
(require (only "parse.ss" parse-document))
(require "classes.ss")
(provide (all-from "classes.ss"))
(provide (all-defined))

(defmethod (mmd-request url)
  ;; TO DO: url class, or specialize on url struct
  (parse-document (xml-http-request url)))

(defmethod (get-resource (type <symbol>) (id <string>) (inc <list>))
  (mmd-request (resource-url type id inc)))

;; Supported inc symbols: (none yet)
(defmethod (get-artist (id <string>) (inc <list>))
  (get-resource 'artist id inc))

;; Supported inc symbols: artist, counts
(defmethod (get-release (id <string>) (inc <list>))
  (get-resource 'release id inc))


(defmethod (match-artists (name <string>))
  (mmd-request (collection-url 'artist `((name . ,name)))))

;; Search the releases collection given the artist name and release title.
(defmethod (match-releases (artist <string>) (title <string>))
  (mmd-request (collection-url 'release `((artist . ,artist)
                                          (title . ,title)))))
