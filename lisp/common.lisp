;;; common.lisp --- Common stuff used in the Lisp integration test code.
;;
;; Copyright (C) 2011 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This Program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This Program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses>.

(load-system :sb-posix)
(load-system :com.dvlsoft.clon)
(load-system :cl-protobuf)
(load-system :usocket)
(load-system :cl-spread)
(load-system :cl-rsb)
(map nil #'unintern '(for finally collect else with in)) ;; iterate bug

(use-package :split-sequence)
(use-package :alexandria)
(use-package :bind)
(use-package :iter)
(use-package :com.dvlsoft.clon)

(let ((descriptor (pbf:load/text #P"../../data/Image.proto")))
  (pbb:emit descriptor :class)
  (pbb:emit descriptor :packed-size)
  (pbb:emit descriptor :serializer)
  (pbb:emit descriptor :deserializer))

(defvar *client/server-test-uri* "/rsbtest/clientserver")

(deftype cookie-type ()
  'non-negative-integer)

(defvar *cookie* nil
  "Contains the magic number we expect to receive from the
client. This is intended to protect against crosstalk between test
cases.")
