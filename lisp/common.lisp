;;; common.lisp --- Common stuff used in the Lisp integration test code.
;;
;; Copyright (C) 2011, 2012, 2013, 2014, 2015 Jan Moringen
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

(mapc #'load-system '(:lparallel :com.dvlsoft.clon
                      :cl-rsb
                      :rsb-converter-protocol-buffer
                      :rsb-transport-socket :rsb-transport-spread
                      :rsb-introspection
                      :rsb-clon))

(let ((descriptor (pbf:load/text #P"../../data/Image.proto")))
  (pbb:emit descriptor :class)
  (pbb:emit descriptor :packed-size)
  (pbb:emit descriptor :serializer)
  (pbb:emit descriptor :deserializer))


(setf *print-length* 1000
      *print-level*  30)

(cl:defpackage #:rsb.integration-test
  (:use
   #:cl
   #:alexandria
   #:split-sequence
   #:let-plus
   #:iter

   #:com.dvlsoft.clon))

(cl:in-package #:rsb.integration-test)

(defvar *client/server-test-uri* "/rsb-integration-test/request-reply")

(defvar *introspection-test-uri* "/rsb-integration-test/introspection")

(deftype cookie-type ()
  'non-negative-integer)

(defvar *cookie* nil
  "Contains the magic number we expect to receive from the
client. This is intended to protect against crosstalk between test
cases.")

(defun write-ready-file (tag)
  (write-string-into-file "" (format nil "test/lisp-~A-ready" tag)
                          :if-does-not-exist :create
                          :if-exists         :supersede))

;; Termination

;; TODO rename
(defvar *terminate* (list (bt:make-lock)
                          (bt:make-condition-variable)
                          nil))

(defun terminate-wait ()
  (bt:with-lock-held ((first *terminate*))
    (iter (until (third *terminate*))
          (bt:condition-wait (second *terminate*) (first *terminate*)))))

(defun terminate-notify ()
  (bt:with-lock-held ((first *terminate*))
    (setf (third *terminate*) t)
    (bt:condition-notify (second *terminate*))))
