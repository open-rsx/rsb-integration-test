(%load-silently :com.dvlsoft.clon)
(%load-silently :cl-protobuf)
(%load-silently :cl-spread)
(%load-silently :cl-rsb)
(map nil #'unintern '(for finally collect else with in)) ;; iterate bug

(use-package :alexandria)
(use-package :iter)
(use-package :com.dvlsoft.clon)

(defvar *interesting-options*
  '((:qualityofservice :reliability)
    (:qualityofservice :ordering)

    (:errorhandling :onhandlererror)

    (:transport :inprocess :enabled)

    (:transport :spread :host)
    (:transport :spread :port)
    (:transport :spread :enabled)

    (:transport :spread :converter :lisp :string)))

(defun main ()
  (setf rsb:*default-configuration* (rsb:options-from-default-sources))
  (make-synopsis
   :postfix "CONFIGFILE OUTPUTFILE"
   :item    (make-flag :long-name   "help"
		       :description "Display help text.")
   :item    (rsb::make-options))
  (make-context)
  (when (getopt :long-name "help")
    (help)
    (return-from main))
  (unless (length= 2 (remainder))
    (help)
    (exit 1))

  (with-input-from-file (stream (nth 0 (remainder)))
    (setf rsb:*default-configuration* (rsb:options-from-stream stream)))

  (with-output-to-file (stream (nth 1 (remainder))
			       :if-exists :supersede)
    (iter (for key   in   *interesting-options*)
	  (for value next (rsb:option-value key))
	  (format stream "~{~(~A~)~^.~}: ~A~%" key value))))

(com.dvlsoft.clon:dump "config" main)
