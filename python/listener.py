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
from rsb import transport
from rsb.rsbspread import SpreadPort
import logging
from threading import Condition

if __name__ == '__main__':

    #logging.basicConfig()
    #logging.getLogger().setLevel(logging.DEBUG)

    class Receiver(object):

        def __init__(self):
            self.counter = 0
            self.condition = Condition()

        def __call__(self, event):
            with self.condition:
                self.counter += 1
                self.condition.notifyAll()
            print("Received: %s, counter is now %s" % (event, self.counter))

    testScope = '/example/informer' # TODO scope object
    scopes = [ '/', '/example', '/example/informer' ] # TODO superScopes
    listeners = []
    receivers = []
    for scope in scopes:
        router = transport.Router(inPort=SpreadPort())
        listener = rsb.Subscriber(testScope, router)
        listeners.append(listener)
        subscription = rsb.Subscription()
        scopeFilter = rsb.filter.ScopeFilter(testScope)
        subscription.appendFilter(scopeFilter)

        receiver = Receiver()
        receivers.append(receiver)
        subscription.appendAction(receiver)

        subscriber.addSubscription(subscription)

    for receiver in receivers:
        with receiver.condition:
            while receiver.counter < 1200:
                receiver.condition.wait()

    print("done!")

    subscriber.deactivate()
