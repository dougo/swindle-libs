;; main.ss -- OpenID 2.0 main module

;; Copyright 2008 Doug Orleans.  This program is distributed under the
;; terms of the GNU Affero General Public License.  See the file
;; COPYING for details.

#lang swindle

(require "relying-party.ss")

(provide authenticate authentication? claimed-identifier extension-parameters)

