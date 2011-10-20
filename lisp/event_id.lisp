;;; event_id.lisp --- Event id part of the Lisp integration test code.
;;
;; Copyright (C) 2011 Jan Moringen
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
	  (assert (uuid:uuid= (rsb:event-id event) expected)))))

(dump "event_id" main)
