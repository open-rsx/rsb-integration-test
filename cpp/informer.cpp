/* ============================================================
 *
 * This file is a part of the RSB project
 *
 * Copyright (C) 2010 by Sebastian Wrede <swrede at techfak dot uni-bielefeld dot de>
 *               2011 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
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
#include <boost/lexical_cast.hpp>

#include <rsc/misc/langutils.h>

#include <rsb/Informer.h>
#include <rsb/Factory.h>
#include <rsb/Scope.h>
#include <rsb/EventId.h>
#include <rsb/MetaData.h>

using namespace std;
using namespace boost;
using namespace rsc::logging;
using namespace rsc::misc;
using namespace rsb;

int main(int argc, char *argv[]) {
    if (argc != 3) {
        cerr << "usage: informer --listener-pid LISTENER-PID " << endl;
        return EXIT_FAILURE;
    }
    int listenerPid = lexical_cast<int>(argv[2]);

    Factory &factory = getFactory();

    uint64_t start = currentTimeMicros();

    vector<int> sizes;
    sizes.push_back(4);
    sizes.push_back(256);
    sizes.push_back(400000);
    vector< Informer<string>::Ptr > informers;
    for (vector<int>::const_iterator it = sizes.begin(); it != sizes.end();
         ++it) {
        Scope scope(str(format("/size%1%/sub1/sub2") % *it));
        cout << "[C++    Informer] Processing scope " << scope << endl;
        Informer<string>::Ptr informer = factory.createInformer<string>(scope);
        informers.push_back(informer);

        Informer<string>::DataPtr s(new string(*it, 'c'));
        EventPtr event = informer->createEvent();
        event->setData(s);
        event->mutableMetaData().setUserInfo("informer-lang", "cpp");
        event->mutableMetaData().setUserTime("informer-start", start);
        event->addCause(EventId(rsc::misc::UUID("00000000-0000-0000-0000-000000000000"), 0));
        for (int j = 0; j < 120; j++) {
            event->mutableMetaData().setUserInfo("index",
                                                 lexical_cast<string>(listenerPid + j));
            informer->publish(event);
        }
    }

    return EXIT_SUCCESS;
}
