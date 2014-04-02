;;; local-introspection.lisp --- Local introspection test code for Lisp.
;;
;; Copyright (C) 2014 Jan Moringen
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

(defun local-introspection-main ()
  ;; Commandline option boilerplate.
  ;;
  ;; The value of the cookie is not used. The purpose of the
  ;; commandline option is for the remote introspection to have
  ;; commandline arguments to verify.
  (setf rsb:*configuration* (rsb:options-from-default-sources))
  (make-synopsis
   :item (make-flag    :long-name     "help"
                       :description   "Display help text.")
   :item (make-lispobj :long-name     "cookie"
                       :typespec      'cookie-type
                       :default-value 0
                       :description   "A cookie for verifying introspection of commandline arguments.")
   :item (rsb:make-options))
  (make-context)
  (when (getopt :long-name "help")
    (help)
    (return-from local-introspection-main))

  (format t "[Lisp   Local  Introspection] Creating participants on ~A~%"
          *introspection-test-uri*)

  (let ((step (lparallel:promise)))
    ;; This remote-server is for synchronization and coordination only
    ;; and thus is not made visible to the remote introspection.
    (rsb.patterns.request-reply:with-remote-server (remote-server *introspection-test-uri*
                                                                  :introspection? nil)
      (rsb.patterns.request-reply:with-local-server (local-server *introspection-test-uri*)
        (rsb.patterns.request-reply:with-methods (local-server)
            (("local-step" ()
               (format t "[Lisp   Local  Introspection] \"local-step\" method called~%")
               (lparallel:fulfill step t)
               (values)))

          ;; Tell remote-introspection process that we are ready by
          ;; calling the "remote-start" method.
          (let ((pid (rsb.introspection::current-process-id)))
            (format t "[Lisp   Local  Introspection] Calling ~
                       \"remote-start\" method with pid ~D~%"
                    pid)
            (rsb.patterns.request-reply:call remote-server "remote-start" pid))

          ;; Wait for "local-step" call with the participant
          ;; configuration unchanged. After receiving the call,
          ;; destroy the local-server.
          (lparallel:force step)

          ;; FIXME Workaround to not deactivate server while method
          ;; call is in progress.
          (sleep 1)))

      ;; Destroying the local-server lets the remote-introspection
      ;; receive a Bye event.

      ;; The "remote-step" call blocks until the remote-introspection
      ;; process is done.
      (format t "[Lisp   Local  Introspection] Calling \"remote-step\" method~%")
      (rsb.patterns.request-reply:call remote-server "remote-step"
                                       rsb.converter:+no-value+)))
  (format t "[Lisp   Local  Introspection] Done~%"))
