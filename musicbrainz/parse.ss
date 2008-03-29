;; Methods for parsing MMD (Musicbrainz XML Metadata Format) documents.

#lang swindle

(require (only "classes.ss" make-artist <release>
               <artist-list> <release-list> <track-list> <release-event>))
(require dom/dom)
(require (only dom/swindle find-if-iterator))
(require (only jabber/dom text-field))
(provide (all-defined))

;; TO DO: reuse the defelementclass abstraction from jabber/dom
;; TO DO: move text-field to dom/extra.ss (or something)
;; TO DO: check namespaces etc.  maybe validate against the MMD schema?

;; No error checking or anything, just quick and dirty for now.

(defmethod (parse-document (doc <document>))
  ;; Assumes there is no more than one child node that is an element.
  (let ((child (first-child-element (document-element doc))))
    (and child (parse-element child))))

(defmethod (first-child-element (node <node>))
  (find-if-iterator
   (lambda (child) (instance-of? child <element>))
   (each-child node)))

(defmethod (parse-element (elt <element>))
  (case (as <symbol> (local-name elt))
    ((artist) (parse-artist elt))
    ((release) (parse-release elt))
    ((artist-list release-list) (parse-resource-list elt))
    (else #f)))

(defmethod (parse-artist (elt <element>))
  (make-artist
   (attribute elt "id")
   (text-field elt 'name)
   (text-field elt 'sort-name)
   (text-field elt 'disambiguation)))

(defmethod (parse-release (elt <element>))
  (let ((artist (find-if-iterator
                 (lambda (child)
                   (and (instance-of? child <element>)
                        (equal? (local-name child) "artist")))
                 (each-child elt)))
        ;; TO DO: refactor each-child-element, find-child-element-by-name ?
        (tracks (find-if-iterator
                 (lambda (child)
                   (and (instance-of? child <element>)
                        (equal? (local-name child) "track-list")))
                 (each-child elt)))
        (release-events (find-if-iterator
                         (lambda (child)
                           (and (instance-of? child <element>)
                                (equal? (local-name child)
                                        "release-event-list")))
                         (each-child elt))))
    (make <release>
      :id (attribute elt "id")
      :artist (and artist (parse-artist artist))
      :title (text-field elt 'title)
      :tracks (and tracks (parse-resource-list tracks))
      :release-events (and release-events (parse-release-event-list
                                           release-events)))))
  
(defmethod (parse-resource-list (elt <element>))
  (make (case (as <symbol> (local-name elt))
          ((artist-list) <artist-list>)
          ((release-list) <release-list>)
          ((track-list) <track-list>))
    :count (as <number> (attribute elt "count"))
    :offset (as <number> (attribute elt "offset"))
    :resources
    (list-of (cons (as <number>
                       (attribute child "ext:score")) ;TO DO: use attribute-ns
                   (parse-element child))
      (child <- each-child elt)
      when (instance-of? child <element>))))

(defmethod (parse-release-event-list (elt <element>))
  ;; TO DO: do event lists have count/offset?
  (list-of (make <release-event>
             :date (attribute child "date")
             :country (attribute child "country"))
    (child <- each-child elt)
    when (and (instance-of? child <element>)
              (equal? (local-name child) "event"))))
