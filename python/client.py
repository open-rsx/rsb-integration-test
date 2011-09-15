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
import optparse

import rsb
import rsb.patterns

if __name__ == '__main__':
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

    print '[Python Client] calling method "ping" with request %s' % cookie
    remoteServer.ping(cookie)

    print '[Python Client] calling method "echo"'
    assert(remoteServer.echo('hello from Python') == 'hello from Python')

    print '[Python Client] calling "addone" method (100 times, synchronous)'
    assert(map(remoteServer.addone, range(100)) == range(1, 101))

    print '[Python Client] calling "addone" method (100 times, asynchronous)'
    assert(map(lambda x: x.get(),
               map(remoteServer.addone.async, range(100)))
           == range(1, 101))

    print '[Python Client] calling method "error"'
    try:
        remoteServer.error('does not matter')
        sys.exit(1)
    except:
        pass

    print '[Python Client] calling method "terminate"'
    remoteServer.terminate('')

    print '[Python Client] Done!'

    remoteServer.deactivate()
