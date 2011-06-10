(%load-silently :bordeaux-threads)
(%load-silently :com.dvlsoft.clon)
(%load-silently :cl-protobuf)
(%load-silently :cl-spread)
(%load-silently :cl-rsb)
(map nil #'unintern '(for finally collect else with in)) ;; iterate bug

(use-package :alexandria)
(use-package :iter)
(use-package :com.dvlsoft.clon)

(defun listener-for-scope (size scope)
  (bt:make-thread
   (lambda ()
     (rsb:with-reader (reader (format nil "spread:/size~D~A"
					  size (rsb:scope-string scope)))
       (iter (for i :from 0 :below 120)
	     (for event next (rsb:receive reader))
	     (assert (= (length (rsb:event-data event)) size))
	     (when (zerop (mod i 30))
	       (format t "[Lisp   Listener] ~@<Scope ~A: ~_received event ~D/~D: ~_~S~@:>~%"
		       (rsb:participant-scope reader) i 120 event)))))))

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

  (let ((listeners (map-product
		  #'listener-for-scope
		  '(4 256 400000)
		  (rsb:super-scopes (rsb:make-scope "/sub1/sub2")
				    :include-self? t))))
  (sleep 1)
  (with-open-file (stream "test/lisp-listener-ready"
			  :if-does-not-exist :create)
    (declare (ignorable stream)))
  (map 'nil #'bt:join-thread listeners))

  (format t "[Lisp   Listener] done!~%"))

(com.dvlsoft.clon:dump "listener" main)
