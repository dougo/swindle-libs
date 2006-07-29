;;; Miscellaneous useful additions to Swindle.

(module extra (lib "swindle.ss" "swindle")
  (require (only (lib "pretty.ss") pretty-print))

  (defsubst* (defmethod* (name . formals) body ...)
    (begin
      (defgeneric* (name . formals))
      (defmethod (name . formals) body ...)))

  (define* (pps stx)
    (pretty-print (syntax-object->datum stx)))

  (add-as-method <boolean> not not)
  (add-len-method <hash-table> hash-table-count)

  (define* <iterator> <list>)

  (defmethod* (map-iterator (func <function>) (iterator <iterator>))
    (list-of (func elt) (elt <- identity iterator)))

  (defmethod* (for-each-iterator (func <function>) (iterator <iterator>))
    (loop-for (elt <- identity iterator) (func elt)))

  (defmethod* (find-if-iterator (pred <function>) (iterator <iterator>))
    (let/ec return
      (do-iterator (elt iterator) (when (pred elt) (return elt)))
      #f))

  (defmethod* (some-iterator (pred <function>) (iterator <iterator>))
    (let/ec return
      (do-iterator (elt iterator) (cond ((pred elt) => return)))
      #f))

  (defmethod* (every-iterator (pred <function>) (iterator <iterator>))
    (let/ec return
      (collect (last-answer #f answer)
	(elt <- identity iterator)
	(answer is (pred elt))
	(unless answer (return #f)))))

  (defsubst* (do-iterator (elt iterator) body ...)
    (for-each-iterator (lambda (elt) body ...) iterator))

  ;; In the following, "sequence" means any value that has a len
  ;; method and a single-integer-indexed ref method.

  (defmethod* (each-elt sequence)
    (list 0 1+
	  (lambda (i) (>= i (len sequence)))
	  (lambda (i) (ref sequence i))))

  (defmethod (each-elt (sequence <list>))
    (collect-iterator sequence))

  (defsyntax* (defseqmethod* stx)
    (define (concat-stx stx suffix)
      (datum->syntax-object stx (symbol-append (syntax-e stx) suffix) stx))
    (syntax-case stx ()
      ((_ (func (arg type) ...))
       (with-syntax ((func-sequence (concat-stx #'func '-sequence))
		     (func-iterator (concat-stx #'func '-iterator)))
	 #'(defmethod* (func-sequence (arg type) ... sequence)
	     (func-iterator arg ... (each-elt sequence)))))))

  (defseqmethod* (map (func <function>)))
  (defseqmethod* (for-each (proc <function>)))
  (defseqmethod* (find-if (pred <function>)))
  (defseqmethod* (some (pred <function>)))
  (defseqmethod* (every (pred <function>)))

  (defsubst* (do-sequence (elt sequence) body ...)
    (for-each-sequence (lambda (elt) body ...) sequence))


  (defmethod (as (class = <list>) sequence)
    (map-sequence identity sequence))

  (defmethod (as (class = <vector>) sequence)
    (let* ((len (len sequence))
	   (vec (make-vector len)))
      (loop-for (i <- 0 ..< len)
	(put! vec i (ref sequence i)))
      vec))

  (defmethod* (sequence-equals? seq1 seq2)
    (and (= (len seq1) (len seq2))
	 (let/ec return
	   (loop-for (elt1 <- each-elt seq1 and elt2 <- each-elt seq2)
	     (unless (equals? elt1 elt2) (return #f)))
	   #t)))
)
