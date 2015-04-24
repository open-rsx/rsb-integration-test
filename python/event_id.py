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

import logging
import uuid

import rsb

if __name__ == "__main__":
    logging.basicConfig(level=logging.DEBUG,
                        format='%(asctime)s %(name)-12s %(levelname)-8s\n%(message)s')
    with open('data/event-id-cases.txt') as input:
        for line in input.readlines():
            origin, seqnum, expected = map(str.strip, line.split(' '))
            originId, seqnum, expectedId = \
                (uuid.UUID(hex = origin), int(seqnum, 16), uuid.UUID(hex = expected))
            event = rsb.Event(id = rsb.EventId(participantId = originId, sequenceNumber = seqnum))
            assert event.id.getAsUUID() == expectedId
