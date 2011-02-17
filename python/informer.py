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

if __name__ == '__main__':
    
    logging.basicConfig()
    logging.getLogger().setLevel(logging.DEBUG)
    
    router = transport.Router(outPort=SpreadPort())
    publisher = rsb.Publisher("rsb://example/informer", router, "string") 
    
    for i in range(1200):
        publisher.publishData("blub");
        
    print("done!")
    
    publisher.deactivate()
    