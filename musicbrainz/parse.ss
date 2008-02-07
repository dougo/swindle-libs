;; Methods for parsing MMD (Musicbrainz XML Metadata Format) documents.

#lang swindle

(require (only "classes.ss" <artist> <release> <artist-list> <release-list>))
(require dom/dom)
(require (only dom/swindle find-if-iterator))
(require (only jabber/dom text-field))
(provide (all-defined))

;; TO DO: reuse the defelementclass abstraction from jabber/dom
;; TO DO: move text-field to dom/extra.ss (or something)
;; TO DO: check namespaces etc.  maybe validate against the MMD schema?

;; No error checking or anything, just quick and diry for now.

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
  (make <artist>
    :id (attribute elt "id")
    :name (text-field elt 'name)
    :sortname (text-field elt 'sort-name)))

(defmethod (parse-release (elt <element>))
  (let ((artist (find-if-iterator
                 (lambda (child)
                   (and (instance-of? child <element>)
                        (equal? (local-name child) "artist")))
                 (each-child elt))))
    (make <release>
      :id (attribute elt "id")
      :artist (and artist (parse-artist artist))
      :title (text-field elt 'title))))

(defmethod (parse-resource-list (elt <element>))
  (make (case (as <symbol> (local-name elt))
          ((artist-list) <artist-list>)
          ((release-list) <release-list>))
    :count (as <number> (attribute elt "count"))
    :offset (as <number> (attribute elt "offset"))
    :resources
    (map (lambda (child)
           (cons (as <number>
                     (attribute child "ext:score")) ;TO DO: use attribute-ns
                 (parse-element child)))
         (list-of child
           (child <- each-child elt)
           when (instance-of? child <element>)))))
