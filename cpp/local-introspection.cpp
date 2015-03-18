/* ============================================================
 *
 * This file is part of the RSB project
 *
 * Copyright (C) 2014 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
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

#include <boost/cstdint.hpp>

#include <boost/thread.hpp>

#include <boost/program_options.hpp>

#include <rsc/os/ProcessInfo.h>

#include <rsb/Factory.h>

using namespace std;

namespace po = boost::program_options;

using namespace rsb;
using namespace rsb::patterns;

typedef boost::int64_t IntegerType;

class LocalStepCallback: public LocalServer::Callback<void, void> {
public:
    LocalStepCallback():
        done(false) {
    }

    void call(const string &/*methodName*/) {
        cout << "[C++    Local  Introspection] \"local-step\" method called" << endl;
        boost::mutex::scoped_lock lock(this->mutex);
        this->done = true;
        this->condition.notify_all();
    }

    void wait() {
        boost::mutex::scoped_lock lock(this->mutex);
        while (!this->done) {
            this->condition.wait(lock);
        }
    }
private:
    volatile bool             done;
    boost::mutex              mutex;
    boost::condition_variable condition;
};

int main(int argc, char *argv[]) {
    // The value is not used. The purpose of the commandline option is
    // for the remote introspection to have commandline arguments to
    // verify.
    IntegerType cookie;

    po::options_description options("Allowed options");
    options.add_options()
        ("help",
         "Display a help message.")
        ("cookie",
         po::value<IntegerType>(&cookie),
         "A cookie for verification introspection of commandline arguments.");
    po::variables_map map;
    po::store(po::command_line_parser(argc, argv).options(options).run(), map);
    po::notify(map);
    if (map.count("help")) {
        cout << "usage: server [OPTIONS]" << endl;
        cout << options << endl;
        return EXIT_SUCCESS;
    }

    ParticipantConfig configNoIntrospection
        = getFactory().getDefaultParticipantConfig();
    configNoIntrospection.setIsIntrospectionEnabled(false);
    Scope scope("/rsb-integration-test/introspection");
    cout << "[C++    Local  Introspection] Creating participants on "
         << scope << endl;

    {
        // This remote-server is for synchronization and coordination
        // only and thus is not made visible to the remote
        // introspection.
        RemoteServerPtr remoteServer
            = getFactory().createRemoteServer(scope,
                                              configNoIntrospection,
                                              configNoIntrospection);

        {
            LocalServerPtr localServer = getFactory().createLocalServer(scope);
            boost::shared_ptr<LocalStepCallback> localStep(new LocalStepCallback());
            localServer->registerMethod("local-step", localStep);

            // Tell remote-introspection process that we are ready by
            // calling the "remote-start" method.
            IntegerType pid = rsc::os::currentProcessId();
            cout << "[C++    Local  Introspection] Calling \"remote-start\""
                    " method with pid "
                 << pid << endl;
            remoteServer->call<void, IntegerType>("remote-start",
                                                  boost::shared_ptr<IntegerType>
                                                  (new IntegerType(pid)));

            // Wait for "local-step" call with the participant
            // configuration unchanged. After receiving the call,
            // destroy the local-server.
            localStep->wait();

            // FIXME Workaround to not deactivate server while method
            // call is in progress.
            sleep(1);
        }

        // Destroying the local-server lets the remote-introspection
        // receive a Bye event.

        // The "remote-step" call blocks until the
        // remote-introspection process is done.
        cout << "[C++    Local  Introspection] Calling \"remote-step\" method"
             << endl;
        remoteServer->call<void>("remote-step");
    }

    cout << "[C++    Local  Introspection] Done" << endl;

    return EXIT_SUCCESS;
}
