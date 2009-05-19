#lang swindle

(require "../swindle.ss")
(require "types.ss")
(require "interfaces.ss")
(require "bootstrap.ss")
(require (only "dom-implementation.ss" *the-dom-implementation*))
(require (only "collections.ss" <dom-implementation-list-impl>))

(defclass* <dom-implementation-source-impl> (<dom-implementation-source>))

(define* *the-dom-implementation-source*
  (make <dom-implementation-source-impl>))

(add-source! *the-dom-implementation-registry* *the-dom-implementation-source*)

(defmethod (dom-implementation (source <dom-implementation-source>)
			       (features <dom-string>))
  (let loop ((features (regexp-split " " features)))
    (cond ((null? features) *the-dom-implementation*)
	  ((string=? "" (car features)) (loop (cdr features)))
	  (else
	   (let ((feature (as <dom-string>
			      (regexp-replace "^\\+" (first features) "")))
		 (version (and (not (null? (cdr features)))
			       (regexp-match "^[0-9]" (second features))
			       (second features))))
	     (and (has-feature? *the-dom-implementation* feature version)
		  (loop (if version (cddr features) (cdr features)))))))))

(defmethod (dom-implementation-list (source <dom-implementation-source>)
				    (features <dom-string>))
  (let ((impl (dom-implementation source features)))
    (make <dom-implementation-list-impl>
      (if impl (vector impl) (vector)))))
