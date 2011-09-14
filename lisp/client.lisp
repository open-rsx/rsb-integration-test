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
    ;; Test echo method.
    (format t "[Lisp   Client] calling \"echo\" method~%")
    (assert (string= (rsb.patterns:call server "echo" "bla") "bla"))

    ;; Test error-producing method.
    (format t "[Lisp   Client] calling \"error\" method~%")
    (let ((signaled? t))
      (ignore-errors
	(rsb.patterns:call server "error" "does not matter")
	(setf signaled? nil))
      (unless signaled?
	(error "~@<Method did not signal.~@:>")))

    ;; Ask server to terminate.
    (format t "[Lisp   Client] calling \"terminate\" method~%")
    (rsb.patterns:call server "terminate" "does not matter"))

  (format t "[Lisp   Client] Done!~%"))

(dump "client" main)
