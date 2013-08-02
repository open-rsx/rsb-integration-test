#!/usr/bin/env python
# ============================================================
#
# Copyright (C) 2011, 2012 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
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
import optparse

import rsb
import rsb.converter
import rsb.patterns

sys.path.append('build/python')
from Image_pb2 import Image

if __name__ == '__main__':

    converter = rsb.converter.ProtocolBufferConverter(messageClass = Image)
    rsb.converter.registerGlobalConverter(converter)
    rsb.__defaultParticipantConfig = rsb.ParticipantConfig.fromDefaultSources()

    parser = optparse.OptionParser()
    parser.add_option('--cookie',
                      dest    = 'cookie',
                      type    = long,
                      default = 0,
                      help    = 'A cookie that is verified by the server in the \"ping\" method call.')
    options, args = parser.parse_args()
    cookie = options.cookie

    scope = rsb.Scope('/rsbtest/clientserver')
    print '[Python Client] Communicating with server at scope %s' % scope

    remoteServer = rsb.patterns.RemoteServer(scope)

    print '[Python Client] Calling "ping" method with request %s' % cookie
    remoteServer.ping(cookie)

    print '[Python Client] Calling "echo" method'
    assert(remoteServer.echo('hello from Python') == 'hello from Python')

    print '[Python Client] Calling "addone" method (100 times, synchronous)'
    assert(map(remoteServer.addone, range(100)) == range(1, 101))

    print '[Python Client] Calling "addone" method (100 times, asynchronous)'
    assert(map(lambda x: x.get(),
               map(remoteServer.addone.async, range(100)))
           == range(1, 101))

    print '[Python Client] Calling "putimage" method'
    image = Image()
    image.width  = 100
    image.height = 100
    image.data   = '1'*(3 * 1024 * 1024)
    remoteServer.putimage(image)

    print '[Python Client] Calling "error" method'
    try:
        remoteServer.error('does not matter')
        sys.exit(1)
    except:
        pass

    print '[Python Client] Calling "terminate" method'
    remoteServer.terminate('')

    print '[Python Client] Done'

    remoteServer.deactivate()
