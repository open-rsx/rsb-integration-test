/* ============================================================
 *
 * This file is part of the RSB project
 *
 * Copyright (C) 2011-2016 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
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
#include <fstream>

#include <boost/cstdint.hpp>

#include <boost/thread.hpp>

#include <boost/program_options.hpp>

#include <rsb/Factory.h>
#include <rsb/converter/Repository.h>
#include <rsb/converter/ProtocolBufferConverter.h>

#include <Image.pb.h>

using namespace std;

using namespace boost::program_options;

using namespace rsb;
using namespace rsb::converter;
using namespace rsb::patterns;

typedef boost::int64_t IntegerType;

class PingCallback: public LocalServer::Callback<IntegerType, string> {
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

template <typename T>
class EchoCallback: public LocalServer::Callback<T, T> {
public:
    boost::shared_ptr<T> call(const string &methodName,
                              boost::shared_ptr<T> request) {
        cout << "[C++    Server] \"" << methodName
             << "\" method called with argument " << *request << endl;
        return request;
    }
};

class AddOneCallback: public LocalServer::Callback<IntegerType, IntegerType> {
public:
    boost::shared_ptr<IntegerType> call(const string &/*methodName*/,
                                        boost::shared_ptr<IntegerType> request) {
        if (*request == 0) {
            cout << "[C++    Server] \"addone\" method called (for 0)" << endl;
        }
        return boost::shared_ptr<IntegerType>(new IntegerType(*request + 1));
    }
};

class PutimageCallback: public LocalServer::Callback<running::example::Image, void> {
public:
    void call(const string &/*methodName*/,
              boost::shared_ptr<running::example::Image> /*request*/) {
        cout << "[C++    Server] \"putimage\" method called" << endl;
    }
};

class ErrorCallback: public LocalServer::Callback<string, string> {
public:
    boost::shared_ptr<string> call(const string &/*methodName*/,
                                   boost::shared_ptr<string> /*request*/) {
        cout << "[C++    Server] \"error\" method called" << endl;
        throw runtime_error("intentional exception");
    }
};

class TerminateCallback: public LocalServer::Callback<void, void> {
public:
    TerminateCallback():
        done(false) {
    }

    void call(const string &/*methodName*/) {
        cout << "[C++    Server] \"terminate\" method called" << endl;
        boost::mutex::scoped_lock lock(this->mutex_);
        this->done = true;
        this->condition.notify_all();
    }

    void wait() {
        boost::mutex::scoped_lock lock(this->mutex_);
        while (!this->done) {
            this->condition.wait(lock);
        }
    }
private:
    volatile bool             done;
    boost::mutex              mutex_;
    boost::condition_variable condition;
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

    Scope scope("/rsb-integration-test/request-reply");
    cout << "[C++    Server] Providing service on " << scope << endl;

    LocalServerPtr server = getFactory().createLocalServer(scope);

    boost::shared_ptr<TerminateCallback> terminate(new TerminateCallback());

    server->registerMethod("ping",        LocalServer::CallbackPtr(new PingCallback(cookie)));

    server->registerMethod("echoBoolean", LocalServer::CallbackPtr(new EchoCallback<bool>()));
    //server->registerMethod("echoInt32",   LocalServer::CallbackPtr(new EchoCallback<boost::int32_t>()));
    server->registerMethod("echoInt64",   LocalServer::CallbackPtr(new EchoCallback<boost::int64_t>()));
    //server->registerMethod("echoFloat",   LocalServer::CallbackPtr(new EchoCallback<float>()));
    server->registerMethod("echoDouble",  LocalServer::CallbackPtr(new EchoCallback<double>()));
    server->registerMethod("echoString",  LocalServer::CallbackPtr(new EchoCallback<string>()));
    server->registerMethod("echoScope",   LocalServer::CallbackPtr(new EchoCallback<Scope>()));

    server->registerMethod("addone",      LocalServer::CallbackPtr(new AddOneCallback()));
    server->registerMethod("putimage",    LocalServer::CallbackPtr(new PutimageCallback()));
    server->registerMethod("error",       LocalServer::CallbackPtr(new ErrorCallback()));
    server->registerMethod("terminate",   terminate);

    {
        ofstream stream("test/cpp-server-ready");
    }

    terminate->wait();
    sleep(1);
    cout << "[C++    Server] Done" << endl;

    return EXIT_SUCCESS;
}
