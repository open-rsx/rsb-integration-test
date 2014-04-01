#!/usr/bin/env python
# ============================================================
#
# Copyright (C) 2011 by Johannes Wienke <jwienke at techfak dot uni-bielefeld dot de>
# Copyright (C) 2011, 2012, 2014 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
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
import rsb
import uuid

if __name__ == '__main__':

    listenerPid = int(sys.argv[2])

    for size in [ 4, 256, 400000 ]:
        scope = "/size-%d/sub_1/sub_2" % size
        print("[Python Informer] Processing scope %s" % scope)
        informer = rsb.createInformer(scope, dataType = str)

        for i in range(120):
            event = rsb.Event(scope = scope,
                              data = 'c' * size,
                              type = str,
                              userInfos = {
                    "informer-lang": "Python",
                    "index":         str(listenerPid + i)
                    },
                              userTimes = {
                    "informer-start": time.time()
                    },
                              causes = set([
                        rsb.EventId(uuid.UUID('00000000-0000-0000-0000-000000000000'), 0)
                        ]))
            informer.publishEvent(event)

        informer.deactivate()

    print("[Python Informer] Done")
