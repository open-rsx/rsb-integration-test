#!/usr/bin/env python
# ============================================================
#
# Copyright (C) 2011 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
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

import time
import threading

import rsb
import rsb.patterns

terminate = False
lock      = threading.Lock()
condition = threading.Condition(lock = lock)

def terminateWait():
    global terminate
    with lock:
        while not terminate:
            condition.wait()

def terminateNotify():
    global terminate
    with lock:
        terminate = True
        condition.notify()

if __name__ == '__main__':

    scope = rsb.Scope('/rsbtest/clientserver')
    print '[Python Server] Providing service at scope %s' % scope

    localServer = rsb.patterns.LocalServer(scope)

    def echo(x):
        print '[Python Server] "echo" method called'
        return x
    localServer.addMethod('echo', echo, str, str)

    def error(x):
        print '[Python Server] "error" method called'
        raise RuntimeError, "intentional error"
    localServer.addMethod('error', error, str, str)

    def _terminate(x):
        print '[Python Server] "terminate" method called'
        terminateNotify()
        return ''
    localServer.addMethod('terminate', _terminate, str, str)

    terminateWait()
    time.sleep(1) # give the terminate call time to complete

    print '[Python Server] done!'

    localServer.deactivate()
