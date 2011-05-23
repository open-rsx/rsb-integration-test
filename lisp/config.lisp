(load "lisp/sbclrc")
(%load-silently :bordeaux-threads)
(%load-silently :cl-protobuf)
(%load-silently :cl-spread)
(%load-silently :cl-rsb)

(use-package :alexandria)
(use-package :iter)

(defvar *interesting-options*
  '((:qualityofservice :reliability)
    (:qualityofservice :ordering)

    (:errorhandling :onhandlererror)

    (:transport :inprocess :enabled)

    (:transport :spread :host)
    (:transport :spread :port)
    (:transport :spread :enabled)

    (:transport :spread :converter :lisp :string)))

(with-input-from-file (stream (nth 1 *posix-argv*))
  (setf rsb:*default-configuration* (rsb:options-from-stream stream)))

(with-output-to-file (stream (nth 2 *posix-argv*)
			     :if-exists :supersede)
  (iter (for key   in   *interesting-options*)
	(for value next (rsb:option-value key))
	(format stream "~{~(~A~)~^.~}: ~A~%" key value)))
