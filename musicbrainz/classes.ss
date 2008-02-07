#lang swindle

(provide (all-defined))

(defclass <server> ()
  (base-url :initarg :url)
  :autoaccessors :slot)


;;; A resource is an object in the Musicbrainz database that can be
;;; accessed by a unique ID.

(defclass <resource> ()
  (id :initarg :id)
  ;; TO DO: relations?
  :autoaccessors :slot)

(defclass <artist> (<resource>)
  (name :initarg :name)
  (sortname :initarg :sortname)
  ;; TO DO: disambiguation, person/group, begin/end date, releases
  :autoaccessors :slot)

(defclass <release> (<resource>)
  (artist :initarg :artist)
  (title :initarg :title)
  ;; TO DO: tracks, type, status, language info, release events, discids
  :autoaccessors :slot)

;; TO DO: <track>, <label>

(defclass <resource-list> ()
  (count :initarg :count)
  (offset :initarg :offset)
  (resources :initarg :resources)       ;alist of (score . resource)
  :autoaccessors :slot)

(defclass <artist-list> (<resource-list>))
(defclass <release-list> (<resource-list>))
