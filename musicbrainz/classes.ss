#lang swindle

(provide (all-defined))

(-defclass-auto-initargs- (:auto #t))
(-defclass-autoaccessors-naming- :slot)

;;; A resource is an object in the Musicbrainz database that can be
;;; accessed by a unique ID.

(defclass <resource> () id)
;; TO DO: relations?

(defclass <artist> (<resource>) name sortname disambiguation)
;; TO DO: person/group, begin/end date, releases

(defclass <release> (<resource>) artist title tracks release-events)
;; TO DO: type, status, language info, discids

(defgeneric date)
(defclass <release-event> () date country)
;; TO DO: label?

;; TO DO: <track>, <label>

(defclass <resource-list> () count offset resources)
;; resources: either alist of (score . resource) or list of resource
;; TO DO: put score on <resource>

(defclass <artist-list> (<resource-list>))
(defclass <release-list> (<resource-list>))
(defclass <track-list> (<resource-list>))


(defmethod (type (resource <artist>)) "artist")
(defmethod (type (resource <release>)) "release")

(defmethod (url (resource <resource>))
  (format "http://musicbrainz.org/~a/~a.html" (type resource) (id resource)))

(defmethod (icon-url (resource <resource>) &opt big-icon?)
  (format "http://musicbrainz.org/images/entity/~a.gif" (type resource)))

(defmethod (icon-url (artist <artist>) &opt big-icon?)
  (if big-icon?
      "http://musicbrainz.org/images/aicon_lg.png"
      (call-next-method)))

(defmethod (link (resource <resource>) &opt big-icon?)
  `(a ((href ,(url resource)))
      (img ((src ,(icon-url resource big-icon?)) (alt ,(type resource))
            (style "vertical-align: bottom; border: 0px; margin-right: 2px;")))
      ,(xexpr resource)))

(defmethod (xexpr (artist <artist>))
  `(strong ,(name artist)))

(defmethod (xexpr (release <release>))
  `(cite ,(title release)))

(defmethod (xexprs (release <release>))
  (with-accessors release (artist tracks release-events)
  `(,(link artist) ", " ,(link release)
    ,@(if (or tracks release-events)
          `(" ("
            ,@(apply
               append
               (intersperse
                `("; ")
                (append
                 (if tracks (list (xexprs tracks)) null)
                 (if release-events (map xexprs release-events) null))))
            ")")
          null))))

(defmethod (xexprs (tracks <track-list>))
  `(,(as <string> (count tracks)) " track" ,(plural (count tracks))))

(defmethod (xexprs (release-event <release-event>))
  `(,(date release-event) " " ,(country release-event)))

(defmethod (plural (n = 1)) "")
(defmethod (plural (n <number>)) "s")

(defmethod (intersperse x (l <null>)) null)
(defmethod (intersperse x (l <pair>))
  (if (null? (cdr l))
      l
      (cons (car l) (cons x (intersperse x (cdr l))))))
