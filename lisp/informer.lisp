(load "lisp/sbclrc")
(%load-silently :bordeaux-threads)
(%load-silently :cl-protobuf)
(%load-silently :cl-spread)
(%load-silently :cl-rsb)

(use-package :alexandria)
(use-package :iter)

(iter (for size in '(4 256 400000))
      (let ((scope (format nil "spread:/size~D/sub1/sub2" size))
	    (data  (make-string size :initial-element #\c)))
	(format t "[Lisp   Informer] ~@<Processing scope ~A~@:>~%" scope)
	(rsb:with-informer (informer scope 'string)
	  (iter (for i :from 0 :below 120)
		(rsb::send informer data)))))
