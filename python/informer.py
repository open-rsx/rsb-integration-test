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
import time

if __name__ == '__main__':

    #logging.basicConfig()
    #logging.getLogger().setLevel(logging.DEBUG)

    for size in [ 4, 256, 400000 ]:
        scope = "/size%d/sub1/sub2" % size
        print("[Python Informer] Processing scope %s" % scope)
        router = transport.Router(outPort=SpreadPort())
        publisher = rsb.Publisher(rsb.Scope(scope), router, "string")

        for i in range(120):
            publisher.publishData('c' * size)

    print("[Python Informer] done!")

    publisher.deactivate()
