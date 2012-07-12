;;; informer.lisp --- Informer part of the Lisp integration test code.
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

(cl:in-package :rsb.integration-test)

(defun informer-main ()
  ;; Commandline option boilerplate.
  (setf rsb:*default-configuration* (rsb:options-from-default-sources))
  (make-synopsis
   :item (make-flag    :long-name   "help"
		       :description "Display help text.")
   :item (make-lispobj :long-name   "listener-pid"
		       :typespec    'positive-integer
		       :description "PID of listener process for inclusion in event meta-data.")
   :item (rsb:make-options))
  (make-context)
  (when (getopt :long-name "help")
    (help)
    (return-from informer-main))

  (let ((listener-pid (getopt :long-name "listener-pid"))
	(start        (local-time:now))
	(causes       (list (cons (uuid:make-null-uuid) 0))))
    (unless listener-pid
      (help)
      (exit 1))

    (iter (for size in '(4 256 400000))
	  (let* ((scope (format nil "/size~D/sub1/sub2" size))
		 (data  (make-string size :initial-element #\c)))
	    (format t "[Lisp   Informer] ~@<Processing scope ~A~@:>~%" scope)
	    (rsb:with-informer (informer scope t)
	      (iter (for i :from 0 :below 120)
		    (let ((event (rsb:make-event
				  scope data
				  :informer-lang "Lisp"
				  :index         (format nil "~D" (+ listener-pid i))
				  :causes        causes)))
		      (setf (rsb:timestamp event :informer-start) start)
		      (rsb:send informer event))))))))
