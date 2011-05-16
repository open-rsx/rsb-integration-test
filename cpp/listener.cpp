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
    MyDataHandler() :
	DataFunctionHandler<string> (
	    boost::bind(&MyDataHandler::handle, this, _1)),
	count(0) {
    }

    void handle(boost::shared_ptr<string> e) {
	if ((count++ % 300) == 0)
	    cout << (format("[C++    Listener] Data %1%/%2% received: %3%")
		     % count % 1200 % e) << endl;

	if (count == 1200) {
	    boost::recursive_mutex::scoped_lock lock(m);
	    cond.notify_all();
	}
    }

    long count;
    boost::recursive_mutex m;
    boost::condition cond;
};

typedef boost::shared_ptr<MyDataHandler> MyDataHandlerPtr;

int main(void) {

    LoggerPtr l = Logger::getLogger("receiver");

    Factory &factory = Factory::getInstance();

    boost::timer t;

    Scope testScope("/example/informer");
    vector<Scope> scopes = testScope.superScopes();
    vector<ListenerPtr> listeners;
    vector<MyDataHandlerPtr> handlers;
    for (vector<Scope>::const_iterator it = scopes.begin(); it != scopes.end(); ++it) {
	listeners.push_back(factory.createListener(*it));
	handlers.push_back(MyDataHandlerPtr(new MyDataHandler()));
	listeners.back()->addHandler(handlers.back());
    }

    cout << "[C++    Listener] Listener setup finished. Waiting for messages..." << endl;

    // wait *here* for shutdown as this is not known to the Subscriber
    for (vector<MyDataHandlerPtr>::const_iterator it = handlers.begin(); it != handlers.end(); ++it) {
	MyDataHandlerPtr handler = *it;
	boost::recursive_mutex::scoped_lock lock(handler->m);
	while (handler->count != 1200) {
	    handler->cond.wait(lock);
	}

	cout << "[C++    Listener] Last message was sent to the following groups: " << endl;
	cout << "[C++    Listener] Elapsed time per message (" << handler->count
	     << " messages received): " << t.elapsed() / handler->count << " s"
	     << endl;
    }

    return EXIT_SUCCESS;
}
