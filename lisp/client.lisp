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

(defun main ()
  (make-synopsis
   :item (make-flag    :long-name   "help"
		       :description "Display help text.")
   :item (rsb:make-options))
  (make-context)
  (when (getopt :long-name "help")
    (help)
    (return-from main))

  (format t "[Lisp   Client] Communicating with remote server at ~A~%"
	  *client/server-test-uri*)

  #+later
  (rsb.patterns:with-remote-server (server *client/server-test-uri*)
    ;; Test echo method.
    (assert (string= (rsb.patterns:call server "echo" "bla") "bla"))

    ;; Test error-producing method.
    (let (signaled?)
      (ignore-errors
	(rsb.patterns:call server "error" "does not matter")
	(setf signaled? t))
      (unless signaled?
	(error "~@<Method did not signal.~@:>")))

    ;; Ask server to terminate.
    (rsb.patterns:call server "terminate"))

  (format t "[Lisp   Client] Done!~%"))

(dump "client" main)
