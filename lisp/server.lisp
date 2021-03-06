;;; server.lisp --- Server part of the Lisp integration test code.
;;
;; Copyright (C) 2011-2016 Jan Moringen
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

(defun server-main ()
  ;; Commandline option boilerplate.
  (setf rsb:*configuration* (rsb:options-from-default-sources))
  (make-synopsis
   :item (make-flag    :long-name     "help"
                       :description   "Display help text.")
   :item (make-lispobj :long-name     "cookie"
                       :typespec      'cookie-type
                       :default-value 0
                       :description   "A cookie for verification in \"ping\" method call.")
   :item (rsb:make-options))
  (make-context)
  (when (getopt :long-name "help")
    (help)
    (return-from server-main))
  (setf *cookie* (getopt :long-name "cookie"))

  (format t "[Lisp   Server] Providing service on ~A~%"
          *client/server-test-uri*)

  (rsb:with-participant (server :local-server *client/server-test-uri*)
    (rsb.patterns.request-reply:with-methods (server)
        (("ping" (request cookie-type)
           (format t "[Lisp   Server] \"ping\" method called with request ~A~%"
                   request)
           (unless (= request *cookie*)
             (format *error-output* "Received cookie value ~D not equal to expected value ~D"
                     request *cookie*)
             (sb-ext:exit :code 1))
           "pong")

         ("echoBoolean" (request boolean)
           (format t "[Lisp   Server] \"echoBoolean\" method called ~
                      with argument ~S~%"
                   request)
           request)

         #+no ("echoInt32" (request (signed-byte 32 ))
           (format t "[Lisp   Server] \"echoInt32\" method called ~
                      with argument ~S~%"
                   request)
           request)

         ("echoInt64" (request (signed-byte 64))
           (format t "[Lisp   Server] \"echoInt64\" method called ~
                      with argument ~S~%"
                   request)
           request)

         #+no ("echoFloat" (request single-float)
           (format t "[Lisp   Server] \"echoFloat\" method called ~
                      with argument ~S~%"
                   request)
           request)

         ("echoDouble" (request double-float)
           (format t "[Lisp   Server] \"echoDouble\" method called ~
                      with argument ~S~%"
                   request)
           request)

         ("echoString" (request string)
           (format t "[Lisp   Server] \"echoString\" method called ~
                      with argument ~S~%"
                   request)
           request)

         ("echoScope" (request rsb:scope)
           (format t "[Lisp   Server] \"echoScope\" method called ~
                      with argument ~S~%"
                   request)
           request)

         ("addone" (request non-negative-integer)
           (when (zerop request)
             (format t "[Lisp   Server] \"addone\" method called (for 0)~%"))
           (1+ request))

         ("error" (request string)
           (declare (ignore request))
           (format t "[Lisp   Server] \"error\" method called~%")
           (error "intentional error"))

         ("putimage" (request running.example:image)
           (declare (ignore request))
           (format t "[Lisp   Server] \"putimage\" method called~%")
           (values))

         ("terminate" ()
           (format t "[Lisp   Server] \"terminate\" method called~%")
           (terminate-notify)
           (values)))

      ;; Wait for "terminate" call.
      (write-ready-file "server")
      (terminate-wait)))

  (format t "[Lisp   Server] Done~%"))
