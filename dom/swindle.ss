#lang swindle

;; TO DO: allow other require-specs
(defsubst* (require* module-name ...)
  (begin (require module-name) ...
         (provide (all-from module-name)) ...))

(require* "extra.ss")
(require* "interface.ss")

