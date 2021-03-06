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
import sys
import rsb

if __name__ == "__main__":
    if len(sys.argv) != 3:
        sys.exit(1)

    logging.basicConfig(level=logging.DEBUG,
                        format='%(asctime)s %(name)-12s %(levelname)-8s\n%(message)s',
                        stream=sys.stderr)

    config = rsb.ParticipantConfig.from_file(sys.argv[1])
    with open(sys.argv[2], 'w') as out:
        out.write('qualityofservice.reliability: %s\n' %
                  config.quality_of_service_spec.reliability.name)
        out.write('qualityofservice.ordering: %s\n' %
                  config.quality_of_service_spec.ordering.name)
        out.write('errorhandling.onhandlererror: %s\n' % 'EXIT')  # TODO fake

        out.write('transport.inprocess.enabled: %s\n' %
                  config.get_transport('inprocess').enabled)

        spread = config.get_transport('spread')
        out.write('transport.spread.host: %s\n' %
                  spread.options.get('host'))
        out.write('transport.spread.port: %s\n' %
                  spread.options.get('port'))
        out.write('transport.spread.enabled: %s\n' % spread.enabled)
        out.write('transport.spread.converter.python.utf-8-string: %s\n' %
                  spread.converter_rules['utf-8-string'])
