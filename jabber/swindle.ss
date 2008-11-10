#lang swindle

(require (planet swindle-libs/dom:1:0/swindle))
(provide (all-from (planet swindle-libs/dom:1:0/swindle)))
(provide (all-from-except swindle exit version))
(provide (rename version mz:version)
         (rename exit mz:exit))

