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
import uuid
from threading import Condition

if __name__ == '__main__':

#    logging.basicConfig()
#    logging.getLogger().setLevel(logging.DEBUG)

    class Receiver(object):

        def __init__(self, expectedScope, expectedSize, expectedCause, expectedCount):
            self.expectedScope = expectedScope
            self.expectedSize  = expectedSize
            self.expectedCause = expectedCause

            self.expectedCount = expectedCount
            self.counter       = 0

            self.condition     = Condition()

        def __call__(self, event):
            assert(event.scope == self.expectedScope)
            assert(len(event.data) == self.expectedSize)
            assert(len(event.causes) == 1)
            assert(event.causes[0] == self.expectedCause)

            with self.condition:
                self.counter += 1
                if self.isDone():
                    self.condition.notifyAll()

        def isDone(self):
            return self.counter == self.expectedCount

    listeners = []
    receivers = []
    for size in [ 4, 256, 400000 ]:
        scope = rsb.Scope("/size%d/sub1/sub2" % size)
        scopes = scope.superScopes(True)
        for superscope in scopes[1:]:
            listener = rsb.Listener(superscope)
            listeners.append(listener)

            receiver = Receiver(scope,
                                size,
                                rsb.EventId(uuid.UUID('00000000-0000-0000-0000-000000000000'), 0),
                                120)
            receivers.append(receiver)
            listener.addHandler(receiver)

    open('test/python-listener-ready', 'w').close()

    for receiver in receivers:
        with receiver.condition:
            while not receiver.isDone():
                print("[Python Listener] Waiting for receiver %s" % receiver.expectedScope)
                receiver.condition.wait(60)
                assert(receiver.isDone())

    print("[Python Listener] done!")
