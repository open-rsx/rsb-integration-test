;;; listener.lisp --- Listener part of the Lisp integration test code.
;;
;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
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

(cl:in-package :rsb.integration-test)

(defun listener-for-scope (sub-scope expected-size expected-causes nested-scope)
  (bt:make-thread
   (lambda ()
     (let* ((size-scope     (rsb:make-scope (format nil "/size-~D" expected-size)))
	    (expected-scope (rsb:merge-scopes nested-scope size-scope))
	    (listen-scope   (rsb:merge-scopes sub-scope size-scope))
	    (uri            (rsb:scope-string listen-scope)))
       (rsb:with-reader (reader uri)
	(iter (for i :from 0 :below 120)
	      (for event next (rsb:receive reader))
	      (assert (rsb:scope= (rsb:event-scope event) expected-scope))
	      (assert (= (length (rsb:event-data event)) expected-size))
	      (assert (set-equal (rsb:event-causes event) expected-causes
				 :test #'rsb:event-id=))))))))

(defun listener-main ()
  ;; Commandline option boilerplate.
  (setf rsb:*default-configuration* (rsb:options-from-default-sources))
  (make-synopsis
   :item (make-flag :long-name   "help"
		    :description "Display help text.")
   :item (rsb:make-options))
  (make-context)
  (when (getopt :long-name "help")
    (help)
    (return-from listener-main))

  (let* ((causes       (list (cons (uuid:make-null-uuid) 0)))
	 (nested-scope (rsb:make-scope "/sub_1/sub_2"))
	 (listeners    (map-product
			(rcurry #'listener-for-scope causes nested-scope)
			(rsb:super-scopes nested-scope :include-self? t)
			'(4 256 400000))))
    (sleep 1)
    (write-ready-file "listener")
    (map 'nil #'bt:join-thread listeners))

  (format t "[Lisp   Listener] Done~%"))
