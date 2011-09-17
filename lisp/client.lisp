(defun main ()
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
    (return-from main))
  (setf *cookie* (getopt :long-name "cookie"))

  (format t "[Lisp   Client] Communicating with remote server at ~A~%"
	  *client/server-test-uri*)

  (rsb.patterns:with-remote-server (server *client/server-test-uri*)
    ;; Test ping method.
    (format t "[Lisp   Client] Calling \"ping\" method with request ~A~%"
	    *cookie*)
    (assert (string= (rsb.patterns:call server "ping" *cookie*) "pong"))

    ;; Test echo method.
    (format t "[Lisp   Client] Calling \"echo\" method~%")
    (assert (string= (rsb.patterns:call server "echo" "hello from Lisp")
		     "hello from Lisp"))

    ;; Test calling addone method synchronously and asynchronously.
    (let ((add-one (rsb.patterns:server-method server "addone")))
      (format t "[Lisp   Client] Calling \"addone\" method (100 times, synchronous)~%")
      (assert (equalp (map 'list add-one (iota 100))
		      (iota 100 :start 1)))

      (format t "[Lisp   Client] Calling \"addone\" method (100 times, asynchronous)~%")
      (assert (equalp (map 'list #'rsb.patterns:future-result
			   (map 'list (rcurry add-one :block? nil)
				(iota 100)))
		      (iota 100 :start 1))))

    ;; Test error-producing method.
    (format t "[Lisp   Client] Calling \"error\" method~%")
    (let ((signaled? t))
      (ignore-errors
	(rsb.patterns:call server "error" "does not matter")
	(setf signaled? nil))
      (unless signaled?
	(error "~@<Method did not signal.~@:>")))

    ;; Ask server to terminate.
    (format t "[Lisp   Client] Calling \"terminate\" method~%")
    (rsb.patterns:call server "terminate" "does not matter"))

  (format t "[Lisp   Client] Done!~%"))

(dump "client" main)
