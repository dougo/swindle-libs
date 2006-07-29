(module character-data "../swindle.ss"
  (require "types.ss")
  (require "interfaces.ss")
  (require "exn.ss")
  (require (only "readonly.ss" check-readonly))
  (require (only "owned.ss" <owned>))
  (require (only "child.ss" <child>))
  (require (only (lib "13.ss" "srfi") string-replace substring/shared))

  ;; TO DO: make a <valued> mixin
  (defmethod (node-value (node <character-data>))
    (data node))
  (defmethod (set-node-value! (node <character-data>) (value <dom-string>))
    (set! (data node) value))

  (defbeforemethod (set-data! (node <character-data>) (data <dom-string>))
    (check-readonly node))

  (defmethod (len (cdata <character-data>))
    (string-length (data cdata)))

  (defmethod (append-data! (cdata <character-data>)
			   (arg <dom-string>))
    (replace-data! cdata (len cdata) 0 arg))

  (defmethod (delete-data! (cdata <character-data>)
			   (offset <exact-integer>)
			   (count <exact-integer>))
    (replace-data! cdata offset count ""))

  (defmethod (insert-data! (cdata <character-data>)
			   (offset <exact-integer>)
			   (arg <dom-string>))
    (replace-data! cdata offset 0 arg ))

  (defbeforemethod (replace-data! (cdata <character-data>)
				  (offset <exact-integer>)
				  (count <exact-integer>)
				  (arg <dom-string>))
    (check-range cdata offset count))
  (defmethod (replace-data! (cdata <character-data>)
			    (offset <exact-integer>)
			    (count <exact-integer>)
			    (arg <dom-string>))
    (let* ((s (data cdata))
	   (end (min (+ offset count) (len s))))
      (set! (data cdata)
	    (as <dom-string> (string-replace s arg offset end)))))

  (defbeforemethod (substring-data (cdata <character-data>)
				   (offset <exact-integer>)
				   (count <exact-integer>))
    (check-range cdata offset count))
  (defmethod (substring-data (cdata <character-data>)
			     (offset <exact-integer>)
			     (count <exact-integer>))
    (as <dom-string>
	(substring/shared (data cdata) offset (+ offset count))))

  (defmethod* (check-range (cdata <character-data>) (offset <exact-integer>)
			  &optional (count 0))
    (when (negative? offset)
      (raise-exn:dom *index-size-err*
	"the specified offset ~v is negative" offset))
    (when (> offset (len cdata))
      (raise-exn:dom *index-size-err*
	"the specified offset ~v is greater than the length of ~v"
	offset cdata))
    (when (negative? count)
      (raise-exn:dom *index-size-err*
	"the specified count ~v is negative" count)))

  (defclass* <character-data-impl> (<owned> <child> <character-data>)
    (data :initarg :data :initvalue "")
    :autoaccessors :slot)


  (defmethod (create-text-node (document <document>) (data <dom-string>))
    (make <text-impl> :document document :data data))

  (defmethod (node-name (node <text>)) "#text")
  (defmethod (node-type (node <text>)) *text-node*)

  (defmethod (clone-node (node <text>) deep?)
    (create-text-node (owner-document node) (data node)))

  (defmethod (split-text! (node <text>) (offset <exact-integer>))
    (let ((new-node (clone-node node #f)))
      (delete-data! node offset (- (len node) offset))
      (delete-data! new-node 0 offset)
      (when (parent-node node)
	(insert-before! (parent-node node) new-node (next-sibling node)))
      new-node))

  (defclass* <text-impl> (<character-data-impl> <text>))


  (defmethod (create-comment (document <document>) (data <dom-string>))
    (make <comment-impl> :document document :data data))

  (defmethod (node-name (node <comment>)) "#comment")
  (defmethod (node-type (node <comment>)) *comment-node*)

  (defmethod (clone-node (node <comment>) deep?)
    (create-comment (owner-document node) (data node)))

  (defclass* <comment-impl> (<character-data-impl> <comment>))

)
