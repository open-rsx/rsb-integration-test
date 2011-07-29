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

import rsb
import rsb.patterns
import logging

if __name__ == '__main__':

    scope = rsb.Scope('/rsbtest/clientserver')
    print('[Python Server] Providing service at scope %s' % scope)

    localServer = rsb.patterns.LocalServer(scope)

    print('[Python Server] done!')

    localServer.deactivate()
