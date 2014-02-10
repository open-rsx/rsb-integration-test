;;; client.lisp --- Client part of the Lisp integration test code.
;;
;; Copyright (C) 2011, 2012, 2014 Jan Moringen
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

(defun client-main ()
  ;; Commandline option boilerplate.
  (setf rsb:*default-configuration* (rsb:options-from-default-sources))
  (make-synopsis
   :item (make-flag    :long-name     "help"
		       :description   "Display help text.")
   :item (make-lispobj :long-name     "cookie"
		       :typespec      'cookie-type
		       :default-value 0
		       :description   "A cookie that is verified by the server in the \"ping\" method call.")
   :item (rsb:make-options))
  (make-context)
  (when (getopt :long-name "help")
    (help)
    (return-from client-main))
  (setf *cookie* (getopt :long-name "cookie"))

  (format t "[Lisp   Client] Communicating with remote server at ~A~%"
	  *client/server-test-uri*)

  (rsb.patterns.request-reply:with-remote-server (server *client/server-test-uri*)
    ;; Test ping method.
    (format t "[Lisp   Client] Calling \"ping\" method with request ~A~%"
	    *cookie*)
    (assert (string= (rsb.patterns.request-reply:call server "ping" *cookie*) "pong"))

    ;; Test echo method.
    (format t "[Lisp   Client] Calling \"echo\" method~%")
    (assert (string= (rsb.patterns.request-reply:call server "echo" "hello from Lisp")
		     "hello from Lisp"))

    ;; Test calling addone method synchronously and asynchronously.
    (let ((add-one (rsb.patterns.request-reply:server-method server "addone")))
      (format t "[Lisp   Client] Calling \"addone\" method (100 times, synchronous)~%")
      (assert (equalp (map 'list add-one (iota 100))
		      (iota 100 :start 1)))

      (format t "[Lisp   Client] Calling \"addone\" method (100 times, asynchronous)~%")
      (assert (equalp (map 'list #'rsb.patterns.request-reply:future-result
			   (map 'list (rcurry add-one :block? nil)
				(iota 100)))
		      (iota 100 :start 1))))

    ;; Test protocol buffer payload
    (format t "[Lisp   Client] Calling \"putimage\" method~%")
    (rsb.patterns.request-reply:call
     server "putimage"
     (make-instance 'running.example:image
                    :width  1024
                    :height 1024
                    :depths (make-array 3
                                        :element-type     '(unsigned-byte 32)
                                        :initial-contents '(8 8 8))
                    :data   (make-array (* 3 1024 1024)
                                        :element-type    '(unsigned-byte 8)
                                        :initial-element 3)))

    ;; Test error-producing method.
    (format t "[Lisp   Client] Calling \"error\" method~%")
    (let ((signaled? t))
      (ignore-errors
	(rsb.patterns.request-reply:call server "error" "does not matter")
	(setf signaled? nil))
      (unless signaled?
	(error "~@<Method did not signal.~@:>")))

    ;; Ask server to terminate.
    (format t "[Lisp   Client] Calling \"terminate\" method~%")
    (rsb.patterns.request-reply:call server "terminate" rsb.converter:+no-value+))

  (format t "[Lisp   Client] Done~%"))
