(load "lisp/sbclrc")
(%load-silently :bordeaux-threads)
(%load-silently :cl-spread)
(%load-silently :cl-rsb)

(use-package :alexandria)
(use-package :iter)

(defun listener-for-scope (size scope)
  (bt:make-thread
   (lambda ()
     (rsb:with-receiver (receiver (format nil "spread:/size~D~A"
					  size (rsb:scope-string scope)))
       (iter (for i :from 0 :below 120)
	     (for event next (rsb:receive receiver))
	     (assert (= (length (rsb:event-data event)) size))
	     (when (zerop (mod i 30))
	       (format t "[Lisp   Listener] ~@<Scope ~A: ~_received event ~D/~D: ~_~S~@:>~%"
		       scope i 120 event)))))))

(let ((listeners (map-product
		  #'listener-for-scope
		  '(4 256 400000)
		  (rsb:super-scopes (rsb:make-scope "/sub1/sub2")
				    :include-self? t))))
  (map 'nil #'bt:join-thread listeners))
