#!/usr/bin/env python
# ============================================================
#
# Copyright (C) 2014 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
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

import logging
import sys
import os
import time
import threading
import optparse

import rsb
import rsb.introspection


class Step(object):
    def __init__(self):
        self.__step = False
        self.__lock = threading.Lock()
        self.__condition = threading.Condition(lock=self.__lock)

    def wait(self):
        with self.__lock:
            while not self.__step:
                self.__condition.wait()

    def notify(self):
        with self.__lock:
            self.__step = True
            self.__condition.notify()


if __name__ == '__main__':
    logging.basicConfig(level=logging.DEBUG,
                        format='%(asctime)s %(name)-12s %(levelname)-8s\n%(message)s',
                        stream=sys.stderr)

    # The value of the cookie is not used. The purpose of the
    # commandline option is for the remote introspection to have
    # commandline arguments to verify.
    parser = optparse.OptionParser()
    parser.add_option('--cookie',
                      dest='cookie',
                      type=int,
                      default=0,
                      help='A cookie for verification in \"ping\" method call.')
    options, args = parser.parse_args()

    configNoIntrospection = rsb.ParticipantConfig.from_default_sources()
    configNoIntrospection.introspection = False
    scope = rsb.Scope('/rsb-integration-test/introspection')
    print('[Python Local  Introspection] Creating participants on %s' % scope)

    # This remote-server is for synchronization and coordination only
    # and thus is not made visible to the remote introspection.
    with rsb.create_remote_server(scope, config=configNoIntrospection) as remoteServer:
        with rsb.create_local_server(scope) as localServer:
            localStep = Step()

            def _localStep():
                print('[Python Local  Introspection] "local-step" method called')
                localStep.notify()
            localServer.add_method('local-step', _localStep,
                                   type(None), type(None))

            # Tell remote-introspection process that we are ready by
            # calling the "remote-start" method.
            pid = os.getpid()
            print(
                '[Python Local  Introspection] Calling "remote-start" method with pid %d' % pid)
            remoteServer.get_method('remote-start')(pid)

            # Wait for "local-step" call with the participant
            # configuration unchanged. After receiving the call,
            # destroy the local-server.
            localStep.wait()

            # FIXME Workaround to not deactivate server while method
            # call is in progress.
            time.sleep(1)

        # Destroying the local-server lets the remote-introspection
        # receive a Bye event.

        # The "remote-step" call blocks until the remote-introspection
        # process is done.
        print('[Python Local  Introspection] Calling "remote-step" method')
        remoteServer.get_method('remote-step')()

    print('[Python Local  Introspection] Done')
