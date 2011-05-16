/* ============================================================
 *
 * This file is a part of the RSB project
 *
 * Copyright (C) 2010 by Sebastian Wrede <swrede at techfak dot uni-bielefeld dot de>
 *
 * This program is free software; you can redistribute it
 * and/or modify it under the terms of the GNU General
 * Public License as published by the Free Software Foundation;
 * either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * ============================================================ */

#include <vector>
#include <iostream>

#include <stdlib.h>
#include <math.h>

#include <boost/format.hpp>
#include <boost/timer.hpp>

#include <rsc/logging/Logger.h>

#include <rsb/Informer.h>
#include <rsb/Factory.h>

using namespace std;
using namespace boost;
using namespace rsc::logging;
using namespace rsc::misc;
using namespace rsb;

int main(void) {

    LoggerPtr l = Logger::getLogger("informer");

    Factory &factory = Factory::getInstance();

    boost::timer t;

    vector<int> sizes;
    sizes.push_back(4);
    sizes.push_back(256);
    sizes.push_back(400000);
    for (vector<int>::const_iterator it = sizes.begin(); it != sizes.end(); ++it) {
	Scope scope(str(format("/size%1%/sub1/sub2") % *it));
	cout << "[C++    Informer] processing scope " << scope << endl;
	Informer<string>::Ptr informer = factory.createInformer<string> (scope);
	Informer<string>::DataPtr s(new string(*it, 'c'));
	for (int j = 0; j < 1200; j++) {
	    informer->publish(s);
	}
    }
    cout << "[C++    Informer] Elapsed time sending messages: " << t.elapsed()
	 << " s" << endl;

    return EXIT_SUCCESS;

}
