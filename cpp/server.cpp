/* ============================================================
 *
 * This file is part of the RSB project
 *
 * Copyright (C) 2011 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
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

#include <stdexcept>

#include <boost/thread.hpp>

#include <rsb/Factory.h>

using namespace std;

using namespace boost;

using namespace rsb;
using namespace rsb::patterns;

class EchoCallback: public Server::Callback<string, string> {
public:
    shared_ptr<string> call(const string &/*methodName*/,
                            shared_ptr<string> request) {
        cout << "[C++    Server] echo method called" << endl;
        return request;
    }
};

class ErrorCallback: public Server::Callback<string, string> {
public:
    shared_ptr<string> call(const string &/*methodName*/,
                            shared_ptr<string> /*request*/) {
        cout << "[C++    Server] error method called" << endl;
        throw runtime_error("intentional exception");
    }
};

class TerminateCallback: public Server::Callback<string, string> {
public:
    TerminateCallback():
        done(false) {
    }

    shared_ptr<string> call(const string &/*methodName*/,
                            shared_ptr<string> /*request*/) {
        cout << "[C++    Server] terminate method called" << endl;
        mutex::scoped_lock lock(this->mutex_);
        this->done = true;
        this->condition.notify_all();
        return shared_ptr<string>(new string(""));
    }

    void wait() {
        mutex::scoped_lock lock(this->mutex_);
        while (!this->done) {
            this->condition.wait(lock);
        }
    }
private:
    volatile bool      done;
    mutex              mutex_;
    condition_variable condition;
};

int main(int /*argc*/, char */*argv*/[]) {

    Scope scope("/rsbtest/clientserver");
    cout << "[C++    Server] Providing service on " << scope << endl;

    Factory &factory = Factory::getInstance();
    ServerPtr server = factory.createServer(scope);

    shared_ptr<TerminateCallback> terminate(new TerminateCallback());

    server->registerMethod("echo",      Server::CallbackPtr(new EchoCallback()));
    server->registerMethod("error",     Server::CallbackPtr(new ErrorCallback()));
    server->registerMethod("terminate", terminate);

    terminate->wait();
    cout << "[C++    Server] done!" << endl;

    return EXIT_SUCCESS;
}
