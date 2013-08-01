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

int main(int argc, char *argv[]) {
    converterRepository<string>()->registerConverter(Converter<string>::Ptr(new ProtocolBufferConverter<running::example::Image>()));

    IntegerType cookie;

    options_description options("Allowed options");
    options.add_options()
        ("help",
         "Display a help message.")
        ("cookie",
         value<IntegerType>(&cookie),
         "A cookie that is verified by the server in the \"ping\" method call.");
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
    cout << "[C++    Client] Communicating with remote server at " << scope << endl;

    RemoteServerPtr remoteServer = getFactory().createRemoteServer(scope);

    // Call "ping" method with cookie value.
    {
        cout << "[C++    Client] Calling \"ping\" method" << endl;
        shared_ptr<IntegerType> request(new IntegerType(cookie));
        if (*remoteServer->call<string>("ping", request) != "pong") {
            cerr << "Call to \"ping\" method did produce expected result" << endl;
            return EXIT_FAILURE;
        }
    }

    // Call echo method.
    {
        cout << "[C++    Client] Calling \"echo\" method" << endl;
        shared_ptr<string> request(new string("hello from C++"));
        if (*remoteServer->call<string>("echo", request)
            != "hello from C++") {
            cerr << "Call to \"echo\" method did not produce expected result." << endl;
            return EXIT_FAILURE;
        }
    }

    // Call a method multiple times with and without overlapping calls.
    cout << "[C++    Client] Calling \"addone\" method (100 times, synchronous)" << endl;
    {
        for (IntegerType i = 0; i < 100; ++i) {
            shared_ptr<IntegerType> request(new IntegerType(i));
            IntegerType result
                = *remoteServer->call<IntegerType>("addone", request);
            if (result != i + 1) {
                cerr << "Synchronous call to \"addone\" method returned " << result
                     << " instead of " << i + 1 << endl;
                return EXIT_FAILURE;
            }
        }
    }

    cout << "[C++    Client] Calling \"addone\" method (100 times, asynchronous)" << endl;
    {
        vector< RemoteServer::DataFuture<IntegerType> > futures;
        for (IntegerType i = 0; i < 100; ++i) {
            shared_ptr<IntegerType> request(new IntegerType(i));
            futures.push_back(remoteServer->callAsync<IntegerType>("addone", request));
        }
        for (IntegerType i = 0; i < 100; ++i) {
            IntegerType result = *futures[i].get(10);
            if (result != i + 1) {
                cerr << "Asynchronous call to \"addone\" method returned " << result
                     << " instead of " << i + 1 << endl;
                return EXIT_FAILURE;
            }
        }
    }

    cout << "[C++    Client] Calling \"putimage\" method" << endl;
    boost::shared_ptr<running::example::Image>
        image(new running::example::Image());
    image->set_width(100);
    image->set_height(100);
    image->set_data(string(3 * 1024 * 1024, 'a'));
    remoteServer->call<void>("putimage", image);

    // Exercise exception mechanism.
    cout << "[C++    Client] Calling \"error\" method" << endl;
    try {
        shared_ptr<string> request(new string(""));
        remoteServer->call<string>("error", request);
        cout << "[C++    Client] Call to error method did not produce an exception" << endl;
        return EXIT_FAILURE;
    } catch (const std::exception &) {
    }

    // Ask the remote server to terminate
    {
        cout << "[C++    Client] Calling \"terminate\" method" << endl;
        shared_ptr<string> request(new string(""));
        remoteServer->call<string>("terminate", request);
    }

    cout << "[C++    Client] Done " << endl;

    return EXIT_SUCCESS;
}
