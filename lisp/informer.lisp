
(%load-silently :sb-posix)
(%load-silently :com.dvlsoft.clon)
(%load-silently :cl-protobuf)
(%load-silently :cl-spread)
(%load-silently :cl-rsb)
(map nil #'unintern '(for finally collect else with in)) ;; iterate bug

(use-package :alexandria)
(use-package :iter)
(use-package :com.dvlsoft.clon)

(defun main ()
  (setf rsb:*default-configuration*
	(cons '((:transport :spread :converter)
		. (:fundamental-ascii-string
		   :fundamental-utf-8-string
		   :fundamental-bytes))
	 (rsb:options-from-default-sources)))
  (make-synopsis
   :item (make-flag    :long-name   "help"
		       :description "Display help text.")
   :item (make-lispobj :long-name   "listener-pid"
		       :typespec    'positive-integer
		       :description "PID of listener process for inclusion in event meta-data.")
   :item (rsb::make-options))
  (make-context)
  (when (getopt :long-name "help")
    (help)
    (return-from main))

  (let ((listener-pid (getopt :long-name "listener-pid"))
	(start        (local-time:now)))
    (unless listener-pid
      (help)
      (exit 1))

    (iter (for size in '(4 256 400000))
	  (let ((scope (format nil "/size~D/sub1/sub2" size))
		(uri   (format nil "spread:/size~D/sub1/sub2" size))
		(data  (make-string size :initial-element #\c)))
	    (format t "[Lisp   Informer] ~@<Processing scope ~A~@:>~%" scope)
	    (rsb:with-informer (informer uri 'string)
	      (iter (for i :from 0 :below 120)
		    (let ((event (rsb:make-event/typed
				  scope data 'string
				  :informer-lang "Lisp"
				  :index         (format nil "~D" (+ listener-pid i)))))
		      (setf (rsb:timestamp event :informer-start) start)
		      (rsb:send informer event))))))))

(com.dvlsoft.clon:dump "informer" main)
