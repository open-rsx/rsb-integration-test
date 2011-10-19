;;; listener.lisp --- Listener part of the Lisp integration test code.
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

(defun listener-for-scope (size scope)
  (bt:make-thread
   (lambda ()
     (rsb:with-reader (reader (format nil "spread:/size~D~A"
				      size (rsb:scope-string scope)))
       (iter (for i :from 0 :below 120)
	     (for event next (rsb:receive reader))
	     (assert (= (length (rsb:event-data event)) size)))))))

(defun main ()
  ;; Commandline option boilerplate.
  (setf rsb:*default-configuration* (rsb:options-from-default-sources))
  (make-synopsis
   :item (make-flag :long-name   "help"
		    :description "Display help text.")
   :item (rsb:make-options))
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

(dump "listener" main)
