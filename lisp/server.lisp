;;; server.lisp --- Server part of the Lisp integration test code.
;;
;; Copyright (C) 2011, 2012 Jan Moringen
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

(defun server-main ()
  ;; Commandline option boilerplate.
  (setf rsb:*default-configuration* (rsb:options-from-default-sources))
  (make-synopsis
   :item (make-flag    :long-name     "help"
		       :description   "Display help text.")
   :item (make-lispobj :long-name     "cookie"
		       :typespec      'cookie-type
		       :default-value 0
		       :description   "A cookie for verification in \"ping\" method call.")
   :item (rsb:make-options))
  (make-context)
  (when (getopt :long-name "help")
    (help)
    (return-from server-main))
  (setf *cookie* (getopt :long-name "cookie"))

  (format t "[Lisp   Server] Providing service on ~A~%"
	  *client/server-test-uri*)

  (rsb.patterns:with-local-server (server *client/server-test-uri*)
    (rsb.patterns:with-methods (server)
	(("ping" (request cookie-type)
	   (format t "[Lisp   Server] \"ping\" method called with request ~A~%"
		   request)
	   (unless (= request *cookie*)
	     (format *error-output* "Received cookie value ~D not equal to expected value ~D"
		     request *cookie*)
	     (sb-ext:quit :unix-status 1))
	   "pong")

	 ("echo" (request string)
	   (format t "[Lisp   Server] \"echo\" method called~%")
	   request)

	 ("addone" (request non-negative-integer)
	   (when (zerop request)
	     (format t "[Lisp   Server] \"addone\" method called (for 0)~%"))
	   (1+ request))

	 ("error" (request string)
	   (declare (ignore request))
	   (format t "[Lisp   Server] \"error\" method called~%")
	   (error "intentional error"))

	 ("putimage" (request running.example:image)
	   (declare (ignore request))
	   (format t "[Lisp   Server] \"putimage\" method called~%")
	   (values))

	 ("terminate" ()
	   (format t "[Lisp   Server] \"terminate\" method called~%")
	   (terminate-notify)
	   (values)))

      ;; Wait for "terminate" call.
      (terminate-wait)))

  (format t "[Lisp   Server] Done~%"))
