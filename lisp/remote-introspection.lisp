;;; remote-introspection.lisp --- Remote introspection test code for Lisp.
;;
;; Copyright (C) 2014, 2015 Jan Moringen
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

;;; Expectation checking and reporting
;;;
;;; The generic function `expect-objects' tries naively to match and
;;; align actual and expected objects in order to produce precise
;;; error reports. Since the approach is naive, it can easily produce
;;; suboptimal alignments and confuse missing/unexpected vs.
;;; mismatched objects as a result.
;;;
;;; All unmatched expectations are reported by signaling
;;; `unmatched-expectation-error' conditions which form a path to the
;;; "root cause mismatch" via `more-conditions:cause'.

(define-condition unmatched-expectation-error (simple-error
                                               more-conditions:chainable-condition)
  ((entry       :initarg  :entry
                :reader   unmatched-expectation-error-entry
                :documentation
                "The entry that did not match the expectation.")
   (expectation :initarg  :expectation
                :reader   unmatched-expectation-error-expectation
                :documentation
                "The expectation that the entry did not match."))
  (:default-initargs
   :entry       (more-conditions:missing-required-initarg
                 'unmatched-expectation-error :entry)
   :expectation (more-conditions:missing-required-initarg
                 'unmatched-expectation-error :expectation))
  (:report
   (lambda (condition stream)
     (format stream "~@<~?~/more-conditions:maybe-print-cause/~@:>"
             (simple-condition-format-control   condition)
             (simple-condition-format-arguments condition)
             condition)))
  (:documentation
   "This error is signaled when an introspected actual system state
    does not match the expected system state."))

(defgeneric mismatch-depth (condition)
  (:method ((condition more-conditions:chainable-condition))
    (if-let ((cause (more-conditions:cause condition)))
      (1+ (mismatch-depth cause))
      (call-next-method)))
  (:method ((condition t))
    0))

(defgeneric expect-objects (entry expectation))

(flet ((call-with-mismatch-error-signaling
           (kind entry expectation thunk)
         (handler-case
             (funcall thunk)
           (error (condition)
             (error 'unmatched-expectation-error
                    :entry            entry
                    :expectation      expectation
                    :format-control   "~@<~A~
                                         ~@:_~@:_~
                                         ~2@T~A~
                                         ~@:_~@:_~
                                         does not match expectation~
                                         ~@:_~@:_~
                                         ~2@T~A~
                                         ~@:_~@:_~
                                       .~@:>"
                    :format-arguments (list kind entry expectation)
                    :cause            condition)))))

  (defmethod expect-objects :around ((entry       t)
                                     (expectation cons))
    (if (typep expectation '(cons (eql &optional)))
        (call-next-method entry (second expectation))
        (call-next-method)))

  (defmethod expect-objects ((entry       rsb.introspection:host-entry)
                             (expectation cons))
    (let+ (((&accessors-r/o
             ((&accessors-r/o
               (id               rsb.introspection:host-info-id)
               (hostname         rsb.introspection:host-info-hostname)
               (machine-type     rsb.introspection:host-info-machine-type)
               (machine-version  rsb.introspection:host-info-machine-version)
               (software-type    rsb.introspection:host-info-software-type)
               (software-version rsb.introspection:host-info-software-version))
              rsb.introspection:entry-info)
             (children rsb.introspection:entry-children))
            entry)
           ((expected-kind expected-id expected-hostname
             &optional
             expected-machine-type  expected-machine-version
             expected-software-type expected-software-version
             &rest expected-children)
            expectation))
      (call-with-mismatch-error-signaling
       "Host" entry expectation
       (lambda ()
         (assert (eq      expected-kind     :host))
         (assert (string= expected-id       id))
         (assert (string= expected-hostname hostname))
         (when (and expected-machine-type machine-type)
           (assert (string= expected-machine-type machine-type)))
         (when (and expected-machine-version machine-version)
           (assert (string= expected-machine-version machine-version)))
         (when (and expected-software-type software-type)
           (assert (string= expected-software-type software-type)))
         (when (and expected-software-version software-version)
           (assert (string= expected-software-version software-version)))
         (expect-objects children expected-children)))))

  (defmethod expect-objects ((entry       rsb.introspection:process-entry)
                             (expectation cons))
    (let+ (((&accessors-r/o
             ((&accessors-r/o
               (process-id            rsb.introspection:process-info-process-id)
               (program-name          rsb.introspection:process-info-program-name)
               (commandline-arguments rsb.introspection:process-info-commandline-arguments))
              rsb.introspection:entry-info)
             (children rsb.introspection:entry-children))
            entry)
           ((expected-kind expected-process-id
             expected-program-name expected-commandline-arguments
             &rest expected-children)
            expectation))
      (call-with-mismatch-error-signaling
       "Process" entry expectation
       (lambda ()
         (assert (eq expected-kind       :process))
         (assert (=  expected-process-id process-id))
         ;; FIXME can we do better for Java?
         (unless (search "java" program-name :test #'string-equal)
           (assert (search expected-program-name program-name))
           (assert (ends-with-subseq expected-commandline-arguments
                                     commandline-arguments
                                     :test #'string=)))
         (expect-objects children expected-children)))))

  (defmethod expect-objects ((entry       rsb.introspection:participant-entry)
                             (expectation cons))
    (let+ (((&accessors-r/o
             ((&accessors-r/o
               (kind  rsb.introspection:participant-info-kind)
               (scope rsb.introspection:participant-info-scope))
              rsb.introspection:entry-info)
             (children rsb.introspection:entry-children))
            entry)
           ((expected-kind expected-scope &rest expected-children)
            expectation))
      (call-with-mismatch-error-signaling
       "Participant" entry expectation
       (lambda ()
         (assert (eq         expected-kind  kind))
         (assert (rsb:scope= expected-scope scope))
         (expect-objects children expected-children))))))

(defmethod expect-objects ((entry sequence) (expectation sequence))
  ;; This method tries (naively) to align the sequences ENTRY and
  ;; EXPECTATION such that the difference corresponding elements of
  ;; both sequences are as similar a possible. When ENTRY and
  ;; EXPECTATION have different numbers of elements, an attempt is
  ;; made to identify the unexpected/missing elements.
  (let+ ((best-depth)
         (best-condition)
         (best-matched)
         (best-aligned)
         ((&flet align-permutation (entries expectations)
            (mapc #'expect-objects entries expectations)
            t))
         ((&flet align (entries expectations)
            (map-permutations
             (lambda (permutation)
               (handler-case
                   (when (align-permutation permutation expectations)
                     (setf best-depth     t
                           best-condition nil
                           best-matched   entries
                           best-aligned   expectations)
                     (return-from align))
                 (unmatched-expectation-error (condition)
                   (when (or (not best-depth)
                             (and (not (eq best-depth t))
                                  (> (mismatch-depth condition) best-depth)))
                     (setf best-depth     (mismatch-depth condition)
                           best-condition condition
                           best-matched   entries
                           best-aligned   expectations)))))
             entries)))
         ((&labels resize (entries expectations)
            (let ((entries-length      (length entries))
                  (expectations-length (length expectations)))
              (cond
                ((> entries-length expectations-length)
                 (dolist (entry entries)
                   (resize (remove entry entries) expectations)))
                ((< entries-length expectations-length)
                 (dolist (expectation expectations)
                   (resize entries (remove expectation expectations))))
                (t
                 (align entries expectations)))))))
    (resize entry expectation)
    (let ((unexpected (set-difference entry       best-matched))
          (missing    (set-difference expectation best-aligned)))
      (setf missing (remove-if (of-type '(cons (eql &optional))) missing))
      (cond
        ((and (not unexpected) (not missing) best-condition)
         (error best-condition))
        ((or unexpected missing best-condition)
         (error 'unmatched-expectation-error
                :entry            entry
                :expectation      expectation
                :format-control   "~@<~
                                     ~@[Unexpected entries~
                                       ~@:_~@:_~
                                       ~2@T~{~A~^, ~}~
                                       ~@:_~@:_~
                                     ~]~
                                     ~@[Missing entries~
                                       ~@:_~@:_~
                                       ~2@T~{~A~^, ~}~
                                       ~@:_~@:_~
                                     ~]~
                                     ~@[Mismatched entries~
                                       ~@:_~@:_~
                                       ~2@T~{~<~{~A~@:_↕~@:_~A~}~:>~^, ~}~
                                       ~@:_~@:_~
                                     ~]~
                                   .~@:>"
                :format-arguments (list unexpected missing
                                        (when best-condition
                                          (let+ (((&structure-r/o
                                                   unmatched-expectation-error- entry expectation)
                                                  best-condition))
                                            (list (list (list entry expectation))))))
                :cause            best-condition))))))

(defun check-expectation (introspection expectation)
  (rsb.introspection:with-database-lock (introspection)
    (expect-objects
     (rsb.introspection:introspection-hosts
      (rsb.introspection::introspection-database introspection))
     expectation)))

(defun expected-objects-1 (pid cookie)
  "Return a description of the object tree expected at the first
   \"checkpoint\"."
  (let ((host-info    (rsb.introspection:current-host-info))
        (method-scope (rsb:merge-scopes
                       '("local-step") *introspection-test-uri*)))
    `((:host ,(rsb.introspection:host-info-id               host-info)
             ,(rsb.introspection:host-info-hostname         host-info)
             ,(rsb.introspection:host-info-machine-type     host-info)
             ,(rsb.introspection:host-info-machine-version  host-info)
             ,(rsb.introspection:host-info-software-type    host-info)
             ,(rsb.introspection:host-info-software-version host-info)
       (:process ,pid "local-introspection" ("--cookie" ,(write-to-string cookie))
        (:local-server ,*introspection-test-uri*
         (:local-method ,method-scope
          (&optional (:listener ,method-scope))
          (&optional (:informer ,method-scope)))))))))

(defun expected-objects-2 ()
  "Return a description of the object tree expected at the second
   \"checkpoint\"."
  (let ((host-info (rsb.introspection:current-host-info)))
    `((:host ,(rsb.introspection:host-info-id       host-info)
             ,(rsb.introspection:host-info-hostname host-info)))))

;;; Entry point
;;;
;;; Creates participants, organizes execution of the test and call
;;; verification machinery.

(defun remote-introspection-main ()
  ;; Globally disable publishing of introspection events for this
  ;; process (to simplify things and reduce the potential for
  ;; confusion).
  (setf rsb:*configuration*
        (acons '(:introspection :enabled) nil
               (remove '(:introspection :enabled)
                       (rsb:options-from-default-sources)
                       :key #'car :test #'equal)))
  ;; Commandline option boilerplate.
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
    (return-from remote-introspection-main))

  (format t "[Lisp   Remote Introspection] Creating participants on ~A~%"
          *introspection-test-uri*)

  (let+ ((cookie                  (getopt :long-name "cookie"))
         ;; Synchronization
         (local-introspection-pid (lparallel:promise))
         (step/start              (lparallel:promise))
         (step/end                (lparallel:promise))
         ;; Introspection events
         (events                  (lparallel.queue:make-queue))
         ((&flet ignorable-participant-event? (object subject event)
            (declare (ignore object))
            "Ignore listener and informer participants which implement
             method the \"local-step\" local-method."
            (when (eq event :participant-added)
              (let ((info (rsb.introspection:entry-info subject)))
                (and (member (rsb.introspection:participant-info-kind info)
                             '(:listener :informer))
                     (rsb:scope=
                      (rsb.introspection:participant-info-scope info)
                      (rsb:merge-scopes
                       '("local-step") *introspection-test-uri*)))))))
         ((&flet on-database-change (object subject event)
            (let+ (((&labels log-event (kind object event subject)
                      (lparallel.queue:push-queue
                       (list kind object event subject) events)
                      (format t "[Lisp   Remote Introspection] ~A~%~
                                                               ~29@T ~A~%~
                                                               ~29@T   ~A~%"
                              event subject object)))
                   ((&labels on-process-change (object subject event)
                      (unless (ignorable-participant-event? object subject event)
                        (log-event :process object event subject))))
                   ((&labels on-host-change (object subject event)
                      (log-event :host object event subject)
                      (flet ((hook () (rsb.introspection:database-change-hook subject)))
                        (case event
                          (:process-added
                           (hooks:add-to-hook
                            (hook) (alexandria:curry #'on-process-change subject)))
                          (:process-removed
                           (hooks:clear-hook (hook))))))))
              (log-event :database object event subject)
              (flet ((hook () (rsb.introspection:database-change-hook subject)))
                (case event
                  (:host-added
                   (hooks:add-to-hook
                    (hook) (alexandria:curry #'on-host-change subject)))
                  (:host-removed
                   (hooks:clear-hook (hook))))))))
         ((&flet drain (&optional (amount 1))
            (loop :repeat amount :do (lparallel.queue:pop-queue events)))))
    ;; This local-server is strictly used for synchronization (and
    ;; publishing of introspection events is disabled for this process
    ;; anyway).
    (rsb:with-participant (server :local-server *introspection-test-uri*)
      (rsb.patterns.request-reply:with-methods (server)
          (("remote-start" (pid non-negative-integer)
             "Called by the local-introspection when it is ready to
              start."
             (format t "[Lisp   Remote Introspection] \"remote-start\" ~
                        method called with pid ~D~%"
                     pid)
             (lparallel:fulfill local-introspection-pid pid)
             (values))
           ("remote-step" ()
             "Called by the local-introspection when it has reached a
              state in which the \"second snapshot\" can be taken."
             (format t "[Lisp   Remote Introspection] \"remote-step\" ~
                        method called~%")
             (lparallel:fulfill step/start)
             (lparallel:force step/end)
             (values)))

        ;; This remote-introspection participant performs its survey
        ;; immediately but cannot receive any replies (since the
        ;; local-introspection process is not running at that time)
        ;; and thus tests populating the database based on spontaneous
        ;; Hello and Bye events (which are published when the
        ;; local-introspection is started).
        (rsb:with-participant
            (introspection :remote-introspection rsb.introspection:+introspection-scope+
                           :update-interval nil
                           :change-handler  (curry #'on-database-change :introspection))
          ;; Tell the test runner that we are ready, then wait for the
          ;; local-introspection program to call our "remote-start"
          ;; method, thereby telling us its PID.
          (write-ready-file "remote-introspection")
          (lparallel:force local-introspection-pid)

          ;; Take and verify the "first snapshot". At this point, the
          ;; local-introspection process is expected to have one
          ;; local-server with one method. See `expected-objects-1'.
          (let ((expected-1 (expected-objects-1
                             (lparallel:force local-introspection-pid)
                             cookie)))

            ;; Wait for expected introspection events to arrive then
            ;; take and verify the "first snapshot" as described in
            ;; the previous comment.
            (drain 4)
            (check-expectation introspection expected-1)

            ;; Test introspection survey (as opposed to receiving
            ;; spontaneous introspection events) by creating a new
            ;; remote-introspection participant after the participants
            ;; in the local-introspection process have already been
            ;; created. Apply the same "wait for, take and verify
            ;; first snapshot" logic as above.
            ;;
            ;; Obviously, this remote-introspection should "see" the
            ;; same snapshot despite gathering the information
            ;; differently.
            (rsb:with-participant
                (introspection :remote-introspection rsb.introspection:+introspection-scope+
                               :update-interval nil
                               :change-handler  (curry #'on-database-change :introspection))
              (drain 4)
              (check-expectation introspection expected-1)))

          ;; Tell the local-introspection process to continue. The
          ;; process will deactivate its local-server and call our
          ;; "remote-step" method when it is ready.
          (format t "[Lisp   Remote Introspection] Calling \"local-step\" method~%")
          (rsb:with-participant (server :remote-server *introspection-test-uri*)
            (rsb.patterns.request-reply:call
             server "local-step" rsb.converter:+no-value+))
          (lparallel:force step/start)

          ;; Drain expected participant and process removal events.
          (drain 6)

          ;; Take and verify the "second snapshot". At this point, the
          ;; local-introspection process is expected to not have any
          ;; participants since it deactivated its local-server
          ;; participant.
          (check-expectation
           introspection (expected-objects-2)))

        ;; Test is done, make the "remote-step" call return. This will
        ;; tell the local-introspection process to terminate.
        (format t "[Lisp   Remote Introspection] Returning from \"remote-step\" method~%")
        (lparallel:fulfill step/end t)

        ;; FIXME Give the local-introspection process time to tear
        ;; down its network connection and terminate.
        (sleep 1))))

  (format t "[Lisp   Remote Introspection] Done~%"))
