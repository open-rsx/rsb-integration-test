(load-system :com.dvlsoft.clon)
(load-system :cl-rsb)
(map nil #'unintern '(for finally collect else with in)) ;; iterate bug

(use-package :split-sequence)
(use-package :alexandria)
(use-package :bind)
(use-package :iter)
(use-package :com.dvlsoft.clon)

(defun main ()
  ;; Commandline option boilerplate.
  (setf rsb:*default-configuration* (rsb:options-from-default-sources))
  (make-synopsis
   :item (make-flag    :long-name   "help"
		       :description "Display help text.")
   :item (rsb:make-options))
  (make-context)
  (when (getopt :long-name "help")
    (help)
    (return-from main))

  ;; Check event id generation test cases.
  (iter (for line in-file "data/event-id-cases.txt" :using #'read-line)
	(bind (((origin sequence-number expected)
		(split-sequence #\Space line))
	       (origin          (uuid:make-uuid-from-string origin))
	       (sequence-number (parse-integer sequence-number :radix 16))
	       (expected        (uuid:make-uuid-from-string expected))
	       (event           (make-instance 'rsb:event
					       :scope           "/"
					       :origin          origin
					       :sequence-number sequence-number)))
	  (format t "~A ~8,'0X ~A [~A]~%"
		  origin sequence-number (rsb:event-id event) expected)
	  (assert (rsb::uuid= (rsb:event-id event) expected)))))

(dump "event_id" main)
