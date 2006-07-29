(module interface (lib "swindle.ss" "swindle")
  (require "extra.ss")

  (defclass* <interface> (<class>))

  (defmethod (make (class <interface>) . initargs)
    (error 'make "~v can't be instantiated" class))

  (defmethod* (interface-of x) #f)

  ;; TO DO: constants?
  (defsubst* (definterface interface supers method ...)
    (begin
      (defclass interface supers :metaclass <interface>)
      (defmethod (interface-of (x interface)) interface)
      (definterfacemethods interface method ...)))

  (defsubst* (definterface* interface supers method ...)
    (begin
      (definterface interface supers method ...)
      (provide interface)
      (provide-interface-methods method ...)))

  (defsubst* (definterfacemethods interface (generic . formals) ...)
    (begin
      (definterfacemethod interface (generic . formals))
      ...))

  (defsubst* (definterfacemethods* interface method ...)
    (begin
      (definterfacemethods interface method ...)
      (provide-interface-methods method ...)))

  (defsubst* (provide-interface-methods (generic . formals) ...)
    (provide generic ...))

  (defsubst* (definterfacemethod interface (generic . formals))
    (begin
      (defgeneric (generic this . formals))
;      (defmethod (generic (this interface) . formals)
;	(error 'generic "interface method not implemented: ~v"
;	       '((this interface) . formals)))
;	)))
      ))

  (defsubst* (definterfacemethod* interface (generic . formals))
    (begin
      (definterfacemethod interface (generic . formals))
      (provide generic)))
)
