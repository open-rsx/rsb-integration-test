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

import sys
import time
import threading
import optparse

import rsb
import rsb.converter
import rsb.patterns

sys.path.append('build/python')
from Image_pb2 import Image

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
    converter = rsb.converter.ProtocolBufferConverter(messageClass = Image)
    rsb.converter.registerGlobalConverter(converter)
    rsb.__defaultParticipantConfig = rsb.ParticipantConfig.fromDefaultSources()

    parser = optparse.OptionParser()
    parser.add_option('--cookie',
                      dest    = 'cookie',
                      type    = long,
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
    localServer.addMethod('ping', ping, long, str)

    def echo(x):
        print '[Python Server] "echo" method called'
        return x
    localServer.addMethod('echo', echo, str, str)

    def addOne(x):
        if x == 0:
            print '[Python Server] "addone" method called (for 0)'
        return x + 1
    localServer.addMethod('addone', addOne, long, long)

    def putImage(x):
        print '[Python Server] "putImage" method called'
    localServer.addMethod('putimage', putImage, Image, type(None))

    def error(x):
        print '[Python Server] "error" method called'
        raise RuntimeError, "intentional error"
    localServer.addMethod('error', error, str, str)

    def _terminate(x):
        print '[Python Server] "terminate" method called'
        terminateNotify()
    localServer.addMethod('terminate', _terminate, str, type(None))

    terminateWait()
    time.sleep(1) # give the terminate call time to complete

    print '[Python Server] Done!'

    localServer.deactivate()
