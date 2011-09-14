(load-system :sb-posix)
(load-system :com.dvlsoft.clon)
(load-system :cl-protobuf)
(load-system :cl-spread)
(load-system :cl-rsb)
(map nil #'unintern '(for finally collect else with in)) ;; iterate bug

(use-package :alexandria)
(use-package :iter)
(use-package :com.dvlsoft.clon)

(defvar *client/server-test-uri* "spread:/rsbtest/clientserver")

(deftype cookie-type ()
  'non-negative-integer)

(defvar *cookie* nil
  "Contains the magic number we expect to receive from the
client. This is intended to protect against crosstalk between test
cases.")

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

(defun main ()
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
    (return-from main))
  (setf *cookie* (getopt :long-name "cookie"))

  (format t "[Lisp   Server] Providing service on ~A~%"
	  *client/server-test-uri*)

  (rsb.patterns:with-local-server (server *client/server-test-uri*)
    (rsb.patterns:with-methods (server)
	(("ping" (request cookie-type)
	   (format t "[Lisp   Server] \"ping\" method called with request ~A~%"
		   request)
	   (assert (= request *cookie*))
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

	 ("terminate" (request string)
	   (declare (ignore request))
	   (format t "[Lisp   Server] \"terminate\" method called~%")
	   (terminate-notify)
	   "")) ;; ensure valid reply

      ;; Wait for "terminate" call.
      (terminate-wait)))

  (format t "[Lisp   Server] Done!~%"))

(dump "server" main)
