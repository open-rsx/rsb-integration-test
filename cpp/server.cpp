/* ============================================================
 *
 * This file is part of the RSB project
 *
 * Copyright (C) 2011, 2012, 2013 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
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

#include <boost/cstdint.hpp>

#include <boost/thread.hpp>

#include <boost/program_options.hpp>

#include <rsb/Factory.h>
#include <rsb/converter/Repository.h>
#include <rsb/converter/ProtocolBufferConverter.h>

#include <Image.pb.h>

using namespace std;

using namespace boost;
using namespace boost::program_options;

using namespace rsb;
using namespace rsb::converter;
using namespace rsb::patterns;

typedef boost::int64_t IntegerType;

class PingCallback: public Server::Callback<IntegerType, string> {
public:
    PingCallback(IntegerType expected) :
        expected(expected) {
    }

    boost::shared_ptr<string> call(const string &/*methodName*/,
                                   boost::shared_ptr<IntegerType> request) {
        cout << "[C++    Server] \"ping\" method called with request "
             << *request << endl;
        if (*request != this->expected) {
            cerr << "Received cookie " << *request
                 << " when expecting " << this->expected << endl;
            exit(1);
        }
        return boost::shared_ptr<string>(new string("pong"));
    }
private:
    IntegerType expected;
};

class EchoCallback: public Server::Callback<string, string> {
public:
    boost::shared_ptr<string> call(const string &/*methodName*/,
                                   boost::shared_ptr<string> request) {
        cout << "[C++    Server] \"echo\" method called" << endl;
        return request;
    }
};

class AddOneCallback: public Server::Callback<IntegerType, IntegerType> {
public:
    boost::shared_ptr<IntegerType> call(const string &/*methodName*/,
                                        boost::shared_ptr<IntegerType> request) {
        if (*request == 0) {
            cout << "[C++    Server] \"addone\" method called (for 0)" << endl;
        }
        return boost::shared_ptr<IntegerType>(new IntegerType(*request + 1));
    }
};

class PutimageCallback: public Server::Callback<running::example::Image, void> {
public:
    void call(const string &/*methodName*/,
              boost::shared_ptr<running::example::Image> /*request*/) {
        cout << "[C++    Server] \"putimage\" method called" << endl;
    }
};

class ErrorCallback: public Server::Callback<string, string> {
public:
    boost::shared_ptr<string> call(const string &/*methodName*/,
                                   boost::shared_ptr<string> /*request*/) {
        cout << "[C++    Server] \"error\" method called" << endl;
        throw runtime_error("intentional exception");
    }
};

class TerminateCallback: public Server::Callback<void, void> {
public:
    TerminateCallback():
        done(false) {
    }

    void call(const string &/*methodName*/) {
        cout << "[C++    Server] \"terminate\" method called" << endl;
        mutex::scoped_lock lock(this->mutex_);
        this->done = true;
        this->condition.notify_all();
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

int main(int argc, char *argv[]) {
    converterRepository<string>()->registerConverter(Converter<string>::Ptr(new ProtocolBufferConverter<running::example::Image>()));

    IntegerType cookie;

    options_description options("Allowed options");
    options.add_options()
        ("help",
         "Display a help message.")
        ("cookie",
         value<IntegerType>(&cookie),
         "A cookie for verification in \"ping\" method call.");
    variables_map map;
    store(command_line_parser(argc, argv)
          .options(options)
          .run(), map);
    notify(map);
    if (map.count("help")) {
        cout << "usage: server [OPTIONS]" << endl;
        cout << options << endl;
        exit(0);
    }

    Scope scope("/rsbtest/clientserver");
    cout << "[C++    Server] Providing service on " << scope << endl;

    ServerPtr server = getFactory().createServer(scope);

    shared_ptr<TerminateCallback> terminate(new TerminateCallback());

    server->registerMethod("ping",      Server::CallbackPtr(new PingCallback(cookie)));
    server->registerMethod("echo",      Server::CallbackPtr(new EchoCallback()));
    server->registerMethod("addone",    Server::CallbackPtr(new AddOneCallback()));
    server->registerMethod("putimage",  Server::CallbackPtr(new PutimageCallback()));
    server->registerMethod("error",     Server::CallbackPtr(new ErrorCallback()));
    server->registerMethod("terminate", terminate);

    terminate->wait();
    sleep(1);
    cout << "[C++    Server] Done" << endl;

    return EXIT_SUCCESS;
}
