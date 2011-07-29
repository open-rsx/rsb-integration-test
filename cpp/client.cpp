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

#include <rsb/Factory.h>

using namespace std;

using namespace boost;

using namespace rsc::logging;

using namespace rsb;
using namespace rsb::patterns;

int main(int /*argc*/, char */*argv*/[]) {
    Scope scope("/rsbtest/clientserver");
    cout << "[C++    Client] Communicating with remote server at " << scope << endl;

    Factory &factory = Factory::getInstance();
    RemoteServerPtr remoteServer = factory.createRemoteServer(scope);

    // Call a regular method.
    assert(*remoteServer->call<string>("echo",
                                      shared_ptr<string>(new string("ping")))
           == "ping");

    // Exercise exception mechanism.
    try {
        remoteServer->call<string>("error",
                                   shared_ptr<string>(new string("")));
        cout << "[C++    Client] call to error method did not produce an exception" << endl;
        return -1;
    } catch (const std::exception &) {
    }

    // Ask the remote server to terminate
    remoteServer->call<string>("terminate",
                               shared_ptr<string>(new string("")));

    cout << "[C++    Client] done! " << endl;

    return EXIT_SUCCESS;
}
