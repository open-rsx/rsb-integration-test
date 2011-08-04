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
  (setf rsb:*default-configuration* (rsb:options-from-default-sources))
  (make-synopsis
   :item (make-flag    :long-name   "help"
		       :description "Display help text.")
   :item (rsb:make-options))
  (make-context)
  (when (getopt :long-name "help")
    (help)
    (return-from main))

  (format t "[Lisp   Server] Providing service on ~A~%"
	  *client/server-test-uri*)

  (rsb.patterns:with-local-server (server *client/server-test-uri*)
    (rsb.patterns:with-methods (server)
	(("echo" (request string)
	   (format t "[Lisp   Server] \"echo\" method called~%")
	   request)
	 ("error" (request string)
	   (declare (ignore request))
	   (format t "[Lisp   Server] \"error\" method called~%")
	   (error "intentional error"))
	 ("terminate" (request string)
	   (declare (ignore request))
	   (format t "[Lisp   Server] \"terminate\" method called~%")
	   (terminate-notify)))
      (terminate-wait)))

  (format t "[Lisp   Server] done!~%"))

(dump "server" main)
