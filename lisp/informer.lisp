
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
  (setf rsb:*default-configuration* (rsb:options-from-default-sources))
  (make-synopsis
   :item (make-flag :long-name   "help"
		    :description "Display help text.")
   :item (rsb::make-options))
  (make-context)
  (when (getopt :long-name "help")
    (help)
    (return-from main))

  (let ((start (local-time:now)))
    (iter (for size in '(4 256 400000))
	  (let ((scope (format nil "spread:/size~D/sub1/sub2" size))
		(data  (make-string size :initial-element #\c)))
	    (format t "[Lisp   Informer] ~@<Processing scope ~A~@:>~%" scope)
	    (rsb:with-informer (informer scope 'string)
	      (iter (for i :from 0 :below 120)
		    (let ((event (rsb:make-event/typed
				  "/" data 'string
				  :informer-lang "Lisp"
				  :informer-pid  (format nil "~D" (+ (sb-posix:getpid) i)))))
		      (setf (rsb:timestamp event :process-start) start)
		      (rsb:send informer event))))))))

(com.dvlsoft.clon:dump "informer" main)
