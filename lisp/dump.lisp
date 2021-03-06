;;; dump.lisp --- Dump a single binary for all tests.
;;
;; Copyright (C) 2011, 2012, 2013, 2014, 2015 Jan Moringen
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

(cl:in-package #:rsb.integration-test)

(defvar *filename->entry-point*
  '(("event_id"             . event-id-main)
    ("config"               . config-main)
    ("listener"             . listener-main)
    ("informer"             . informer-main)
    ("client"               . client-main)
    ("server"               . server-main)
    ("local-introspection"  . local-introspection-main)
    ("remote-introspection" . remote-introspection-main)))

(defun main ()
  "Entry point function of the test program."
  (log4cl:remove-all-appenders log4cl:*root-logger*)
  (log:config :debu9
              :pattern "%&%<%I%;<;;>;-5p [%D{%H:%M:%S}]%:; [;;];t %g{}{}{:downcase}%:; ;F (%C{}{ }{:downcase})%2.2N - %:_%m%>%n"
              :stream  *error-output*)
  (make-synopsis)
  (let* ((progname (pathname-name
                    (pathname (first (uiop:raw-command-line-arguments)))))
         (entry    (cdr (assoc progname *filename->entry-point*
                               :test #'string=))))
    (if entry
        (funcall entry)
        (format *error-output* "~@<Invoke as ~{~A~^ or ~}.~@:_~@:_This ~
                                is usually done by creating symbolic ~
                                links~@:_~@:_~:*~{~2T~A ->
                                test~@:_~}~@:>~%"
                (mapcar #'car *filename->entry-point*)))))

(setf uiop:*image-entry-point* 'main)
(uiop:dump-image "test" :executable t)
