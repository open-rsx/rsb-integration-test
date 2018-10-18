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

import logging
import sys
import optparse

import rsb
import rsb.converter

sys.path.append('build/python')
from Image_pb2 import Image


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
                      help='A cookie that is verified by the server in the \"ping\" method call.')
    options, args = parser.parse_args()
    cookie = options.cookie

    scope = rsb.Scope('/rsb-integration-test/request-reply')
    print('[Python Client] Communicating with server at scope %s' % scope)

    with rsb.create_remote_server(scope) as remote_server:

        print('[Python Client] Calling "ping" method with request %s' % cookie)
        assert(remote_server.ping(int(cookie)) == 'pong')

        for (method, value) in [('echoBoolean', True),
                                #('echoInt32',   -1),
                                ('echoInt64',   1099511627776),
                                #('echoFloat',   1.2345),
                                ('echoDouble',  1e300),
                                ('echoString',  'hello from Python'),
                                ('echoScope',   rsb.Scope('/scope'))]:
            print('[Python Client] Calling "' + method
                  + '" method with argument ' + str(value))
            assert(remote_server.get_method(method)(value) == value)

        print('[Python Client] Calling "addone" method (100 times, synchronous)')
        assert(list(map(remote_server.addone, list(range(100))))
               == list(range(1, 101)))

        print('[Python Client] Calling "addone" method (100 times, asynchronous)')
        assert([x.get()
                for x in list(map(remote_server.addone.asynchronous,
                                  list(range(100))))]
               == list(range(1, 101)))

        print('[Python Client] Calling "putimage" method')
        image = Image()
        image.width = 100
        image.height = 100
        image.data = b'1'*(3 * 1024 * 1024)
        remote_server.putimage(image)

        print('[Python Client] Calling "error" method')
        try:
            remote_server.error('does not matter')
            sys.exit(1)
        except:
            pass

        print('[Python Client] Calling "terminate" method')
        remote_server.terminate()

    print('[Python Client] Done')
