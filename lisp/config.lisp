;;; config.lisp --- Config part of the Lisp integration test code.
;;
;; Copyright (C) 2011, 2012, 2014 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This Program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This Program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses>.

(cl:in-package :rsb.integration-test)

(defvar *interesting-options*
  '((:qualityofservice :reliability)
    (:qualityofservice :ordering)

    (:errorhandling :onhandlererror)

    (:transport :inprocess :enabled)

    (:transport :spread :host)
    (:transport :spread :port)
    (:transport :spread :enabled)

    (:transport :spread :converter :lisp :utf-8-string)))

(defun config-main ()
  ;; Commandline option boilerplate.
  (setf rsb:*configuration*
        (append '(((:transport :spread :enabled) . "1"))
                (rsb:options-from-default-sources)))
  (make-synopsis
   :postfix "CONFIGFILE OUTPUTFILE"
   :item    (make-flag :long-name   "help"
                       :description "Display help text.")
   :item    (rsb:make-options))
  (make-context)
  (when (getopt :long-name "help")
    (help)
    (return-from config-main))
  (unless (length= 2 (remainder))
    (help)
    (sb-ext:exit :code 1))

  (with-input-from-file (stream (nth 0 (remainder)))
    (setf rsb:*configuration* (rsb:options-from-stream stream)))

  (with-output-to-file (stream (nth 1 (remainder))
                               :if-exists :supersede)
    (iter (for key   in   *interesting-options*)
          (for value next (rsb:option-value key))
          (format stream "~{~(~A~)~^.~}: ~A~%" key value))))
