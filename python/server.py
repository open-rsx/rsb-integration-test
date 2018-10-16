#!/usr/bin/env python
# ============================================================
#
# Copyright (C) 2011-2016 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
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

from numbers import Integral
import logging
import sys
import time
import threading
import optparse

import rsb
import rsb.converter

sys.path.append('build/python')
from Image_pb2 import Image

terminate = False
lock = threading.Lock()
condition = threading.Condition(lock=lock)


def terminate_wait():
    global terminate
    with lock:
        while not terminate:
            condition.wait()


def terminate_notify():
    global terminate
    with lock:
        terminate = True
        condition.notify()


if __name__ == '__main__':
    logging.basicConfig(level=logging.DEBUG,
                        format='%(asctime)s %(name)-12s %(levelname)-8s\n%(message)s',
                        stream=sys.stderr)

    converter = rsb.converter.ProtocolBufferConverter(message_class=Image)
    rsb.converter.register_global_converter(converter)
    rsb.__default_participant_config = \
        rsb.ParticipantConfig.from_default_sources()

    parser = optparse.OptionParser()
    parser.add_option('--cookie',
                      dest='cookie',
                      type=int,
                      default=0,
                      help='A cookie for verification in \"ping\" method call.')
    options, args = parser.parse_args()
    cookie = options.cookie

    scope = rsb.Scope('/rsb-integration-test/request-reply')
    print('[Python Server] Providing service at scope %s' % scope)

    with rsb.create_local_server(scope) as local_server:

        def ping(request):
            print('[Python Server] "ping" method called '
                  'with request {}'.format(request))
            assert(request == cookie)
            return 'pong'
        local_server.add_method('ping', ping, int, str)

        def echo(x):
            print('[Python Server] "echo*" method called '
                  'with argument {}'.format(x))
            return x
        for (method, typ) in [('echoBoolean', bool),
                              #('echoInt32',   int),
                              ('echoInt64',   Integral),
                              #('echoFloat',   float),
                              ('echoDouble',  float),
                              ('echoString',  str),
                              ('echoScope',   rsb.Scope)]:
            local_server.add_method(method, echo, typ, typ)

        def addOne(x):
            if x == 0:
                print('[Python Server] "addone" method called (for 0)')
            return int(x + 1)
        local_server.add_method('addone', addOne, int, int)

        def putImage(x):
            print('[Python Server] "putImage" method called')
        local_server.add_method('putimage', putImage, Image, type(None))

        def error(x):
            print('[Python Server] "error" method called')
            raise RuntimeError("intentional error")
        local_server.add_method('error', error, str, str)

        def _terminate():
            print('[Python Server] "terminate" method called')
            terminate_notify()
        local_server.add_method('terminate', _terminate, type(None), type(None))

        open('test/python-server-ready', 'w').close()
        print("[Python Server] Ready")
        terminate_wait()
        time.sleep(1)  # give the terminate call time to complete

    print('[Python Server] Done')
