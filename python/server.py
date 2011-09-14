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
import optparse

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
    parser = optparse.OptionParser()
    parser.add_option('--cookie',
                      dest    = 'cookie',
                      type    = int,
                      default = 0,
                      help    = 'A cookie for verification in \"ping\" method call.')
    options, args = parser.parse_args()
    cookie = options.cookie

    scope = rsb.Scope('/rsbtest/clientserver')
    print '[Python Server] Providing service at scope %s' % scope

    localServer = rsb.patterns.LocalServer(scope)

    def ping(request):
        print '[Python Server] "ping" method called with request %s' % request
        assert(request == cookie)
        return 'pong'
    localServer.addMethod('ping', ping, int, str)

    def echo(x):
        print '[Python Server] "echo" method called'
        return x
    localServer.addMethod('echo', echo, str, str)

    def addOne(x):
        if x == 0:
            print '[Python Server] "addone" method called (for 0)'
        return x + 1
    localServer.addMethod('addone', addOne, int, int)

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
