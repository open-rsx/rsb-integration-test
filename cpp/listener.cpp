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

#include <boost/thread/recursive_mutex.hpp>
#include <boost/thread/condition.hpp>
#include <boost/timer.hpp>

#include <rsc/logging/Logger.h>

#include <rsb/Listener.h>
#include <rsb/Subscription.h>
#include <rsb/Handler.h>
#include <rsb/filter/ScopeFilter.h>
#include <rsb/Factory.h>

using namespace std;
using namespace rsc::logging;
using namespace rsc::misc;
using namespace rsb;
using namespace rsb::filter;

class MyDataHandler: public DataFunctionHandler<string> {
public:
	MyDataHandler() :
                DataFunctionHandler<string>(boost::bind(&MyDataHanlder::handle)),
                count(0) {
	}

	void handle(boost::shared_ptr<string> e) {
		cout << "Data received: " << *e << endl;
		count++;
		if (count == 1200) {
			boost::recursive_mutex::scoped_lock lock(m);
			cond.notify_all();
		}
	}

	long count;
	boost::recursive_mutex m;
	boost::condition cond;
};

int main(void) {

	LoggerPtr l = Logger::getLogger("receiver");

	Factory &factory = Factory::getInstance();

	boost::timer t;

	ListenerPtr s = factory.createListener("rsb://example/informer");
	boost::shared_ptr<MyDataHandler> dh(new MyDataHandler());
        s->appendHandler(dh);

	cout << "Subscriber setup finished. Waiting for messages..." << endl;

	// wait *here* for shutdown as this is not known to the Subscriber
	{
		boost::recursive_mutex::scoped_lock lock(dh->m);
		while (dh->count != 1200) {
			dh->cond.wait(lock);
		}
	}

	cout << "Last message was sent to the following groups: " << endl;

	cout << "Elapsed time per message (" << dh->count
			<< " messages received): " << t.elapsed() / dh->count << " s"
			<< endl;

	return EXIT_SUCCESS;
}
