#lang swindle

(require dom/swindle)
(provide (all-from dom/swindle))
(provide (all-from-except swindle exit version))
(provide (rename version mz:version)
         (rename exit mz:exit))

