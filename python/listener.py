#!/usr/bin/env python
# ============================================================
#
# Copyright (C) 2011 by Johannes Wienke <jwienke at techfak dot uni-bielefeld dot de>
#
# This program is free software; you can redistribute it
# and/or modify it under the terms of the GNU General
# Public License as published by the Free Software Foundation;
# either version 2, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# ============================================================

import rsb
import logging
from threading import Condition

if __name__ == '__main__':

#    logging.basicConfig()
#    logging.getLogger().setLevel(logging.DEBUG)

    class Receiver(object):

        def __init__(self, scope, size, expected):
            self.scope = scope
            self.size = size
            self.expected = expected
            self.counter = 0
            self.condition = Condition()

        def __call__(self, event):
            assert(len(event.getData()) == self.size)

            with self.condition:
                self.counter += 1
                if (self.counter % 30 == 0):
                    print("[Python Listener] Scope %s: Received event %d/%d: %s"
                          % (self.scope, self.counter, self.expected, event))
                if self.isDone():
                    self.condition.notifyAll()

        def isDone(self):
            return self.counter == self.expected

    listeners = []
    receivers = []
    for size in [ 4, 256, 400000 ]:
        scope = rsb.Scope("/size%d/sub1/sub2" % size)
        scopes = scope.superScopes(True)
        for scope in scopes[1:]:
            listener = rsb.Subscriber(scope)
            listeners.append(listener)
            subscription = rsb.Subscription()
            scopeFilter = rsb.filter.ScopeFilter(scope)
            subscription.appendFilter(scopeFilter)

            receiver = Receiver(scope, size, 120)
            receivers.append(receiver)
            subscription.appendAction(receiver)

            listener.addSubscription(subscription)

    open('test/python-listener-ready', 'w').close()

    for receiver in receivers:
        with receiver.condition:
            while not receiver.isDone():
                print("[Python Listener] waiting for receiver %s" % receiver.scope)
                receiver.condition.wait(60)
                assert(receiver.isDone())

    print("[Python Listener] done!")
