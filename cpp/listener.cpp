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

#include <iostream>

#include <stdlib.h>
#include <math.h>

#include <fstream>

#include <boost/format.hpp>
#include <boost/thread/recursive_mutex.hpp>
#include <boost/thread/condition.hpp>

#include <rsb/EventId.h>
#include <rsb/Listener.h>
#include <rsb/Handler.h>
#include <rsb/Factory.h>

using namespace std;
using namespace boost;
using namespace rsc::logging;
using namespace rsc::misc;
using namespace rsb;
using namespace rsb::filter;

class MyEventHandler: public Handler {
public:
    MyEventHandler(const Scope  &expectedScope,
                   unsigned int  expectedSize,
                   EventId       expectedCause,
                   long          expectedCount) :
        expectedScope(expectedScope),
        expectedSize(expectedSize), expectedCause(expectedCause),
        count(0), expectedCount(expectedCount) {
    }

    void handle(EventPtr event) {
        boost::shared_ptr<string> datum
            = static_pointer_cast<string>(event->getData());

        assert(event->getScope() == this->expectedScope);
        assert(datum->size() == this->expectedSize);
        assert(event->getCauses().size() == 1);
        assert(*event->getCauses().begin() == this->expectedCause);

        ++this->count;
        if (isDone()) {
            boost::recursive_mutex::scoped_lock lock(m);
            cond.notify_all();
        }
    }

    bool isDone() {
        return this->count == this->expectedCount;
    }

    Scope                  expectedScope;
    unsigned int           expectedSize;
    EventId                expectedCause;

    long                   count;
    long                   expectedCount;

    boost::recursive_mutex m;
    boost::condition       cond;
};

typedef boost::shared_ptr<MyEventHandler> MyEventHandlerPtr;

int main(void) {
    Factory &factory = Factory::getInstance();

    vector<ListenerPtr> listeners;
    vector<MyEventHandlerPtr> handlers;

    vector<int> sizes;
    sizes.push_back(4);
    sizes.push_back(256);
    sizes.push_back(400000);
    for (vector<int>::const_iterator it = sizes.begin(); it != sizes.end();
         ++it) {
        Scope scope(str(format("/size%1%/sub1/sub2") % *it));
        vector<Scope> scopes = scope.superScopes(true);
        for (vector<Scope>::const_iterator it_ = scopes.begin() + 1;
             it_ != scopes.end(); ++it_) {
            listeners.push_back(factory.createListener(*it_));
            handlers.push_back(MyEventHandlerPtr(new MyEventHandler(scope,
                                                                    *it,
                                                                    EventId(rsc::misc::UUID("00000000-0000-0000-0000-000000000000"),
                                                                            0),
                                                                    120)));
            listeners.back()->addHandler(handlers.back());
        }
    }

    cout << "[C++    Listener] Listener setup finished. Waiting for messages..."
         << endl;
    {
        ofstream stream("test/cpp-listener-ready");
    }

    // wait *here* for shutdown as this is not known to the Subscriber
    for (vector<MyEventHandlerPtr>::const_iterator it = handlers.begin();
         it != handlers.end(); ++it) {
        MyEventHandlerPtr handler = *it;
        boost::recursive_mutex::scoped_lock lock(handler->m);
        while (!handler->isDone()) {
            handler->cond.wait(lock);
        }
    }

    return EXIT_SUCCESS;
}
