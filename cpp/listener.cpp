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

#include <iostream>

#include <stdlib.h>
#include <math.h>

#include <boost/format.hpp>
#include <boost/bind.hpp>
#include <boost/thread/recursive_mutex.hpp>
#include <boost/thread/condition.hpp>
#include <boost/timer.hpp>

#include <rsc/logging/Logger.h>

#include <rsb/Listener.h>
#include <rsb/Handler.h>
#include <rsb/filter/ScopeFilter.h>
#include <rsb/Factory.h>

using namespace std;
using namespace boost;
using namespace rsc::logging;
using namespace rsc::misc;
using namespace rsb;
using namespace rsb::filter;

class MyDataHandler: public DataFunctionHandler<string> {
public:
    MyDataHandler(const Scope &scope, unsigned int size, long expected) :
    DataFunctionHandler<string> (
	    boost::bind(&MyDataHandler::handle, this, _1)),
    scope(scope), size(size), count(0), expected(expected) {
    }

    void handle(boost::shared_ptr<string> e) {
	assert(e->size() == this->size);

	if ((count++ % 300) == 0)
	    cout << (format("[C++    Listener] %1%: Event %2%/%3% received: %4%")
		     % this->scope % this->count % this->expected % e) << endl;

	if (isDone()) {
	    boost::recursive_mutex::scoped_lock lock(m);
	    cond.notify_all();
	}
    }

    bool isDone() {
	return this->count == this->expected;
    }

    Scope scope;
    unsigned int size;
    long count;
    long expected;
    boost::recursive_mutex m;
    boost::condition cond;
};

typedef boost::shared_ptr<MyDataHandler> MyDataHandlerPtr;

int main(void) {

    LoggerPtr l = Logger::getLogger("receiver");

    Factory &factory = Factory::getInstance();

    boost::timer t;

    vector<ListenerPtr> listeners;
    vector<MyDataHandlerPtr> handlers;

    vector<int> sizes;
    sizes.push_back(4);
    sizes.push_back(256);
    sizes.push_back(400000);
    for (vector<int>::const_iterator it = sizes.begin(); it != sizes.end(); ++it) {
	Scope scope(str(format("/size%1%/sub1/sub2") % *it));
	vector<Scope> scopes = scope.superScopes(true);
	for (vector<Scope>::const_iterator it_ = scopes.begin() + 1; it_ != scopes.end(); ++it_) {
	    listeners.push_back(factory.createListener(*it_));
	    handlers.push_back(MyDataHandlerPtr(new MyDataHandler(*it_, *it, 1200)));
	    listeners.back()->addHandler(handlers.back());
	}
    }

    cout << "[C++    Listener] Listener setup finished. Waiting for messages..." << endl;

    // wait *here* for shutdown as this is not known to the Subscriber
    for (vector<MyDataHandlerPtr>::const_iterator it = handlers.begin(); it != handlers.end(); ++it) {
	MyDataHandlerPtr handler = *it;
	boost::recursive_mutex::scoped_lock lock(handler->m);
	while (!handler->isDone()) {
	    handler->cond.wait(lock);
	}
    }

    cout << "[C++    Listener] Elapsed time " << t.elapsed() << " s" << endl;

    return EXIT_SUCCESS;
}
