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

package rsb.integration.test;

import java.lang.Thread;

import rsb.Scope;
import rsb.Factory;

import rsb.patterns.DataCallback;
import rsb.patterns.LocalServer;
import rsb.patterns.RemoteServer;

import rsb.util.os.ProcessInfoSelector;

public class LocalIntrospectionTest {

    private static class LocalStepCallback extends DataCallback<Void, Void> {

        private boolean done = false;

        public Void invoke(Void request) {
            System.out.println("[Java   Local  Introspection] \"local-step\" method called");
            synchronized (this) {
                this.done = true;
                this.notify();
            }
            return null;
        }

        public void waitForCall() throws InterruptedException {
            synchronized (this) {
                while (!this.done) {
                    this.wait();
                }
            }
        }

    }

    public static void main(String[] args) throws Throwable {

        rsb.config.ParticipantConfig configNoIntrospection
            = Factory.getInstance().getDefaultParticipantConfig().copy();
        configNoIntrospection.setIntrospectionEnabled(false);
        Scope scope = new Scope("/rsb-integration-test/introspection");
        System.out.println("[Java   Local  Introspection] Creating participants on "
                           + scope);

        RemoteServer remoteServer = null;
        try {
            remoteServer
                = Factory.getInstance().createRemoteServer(scope,
                                                           configNoIntrospection);
            remoteServer.activate();

            LocalServer localServer = null;
            try {
                localServer = Factory.getInstance().createLocalServer(scope);
                localServer.activate();
                LocalStepCallback localStep = new LocalStepCallback();
                localServer.addMethod("local-step", localStep);

                // Tell remote-introspection process that we are ready by
                // calling the "remote-start" method.
                Long pid = new Long(ProcessInfoSelector.getProcessInfo().getPid());
                System.out.println("[Java   Local  Introspection] Calling \"remote-start\""
                                   + " method with pid " + pid);
                remoteServer.call("remote-start", pid);

                // Wait for "local-step" call with the participant
                // configuration unchanged. After receiving the call,
                // destroy the local-server.
                localStep.waitForCall();

                // FIXME Workaround to not deactivate server while
                // method call is in progress.
                Thread.sleep(1000);
            } finally {
                if (localServer != null) {
                    localServer.deactivate();
                }
            }

            // Destroying the local-server lets the remote-introspection
            // receive a Bye event.

            // The "remote-step" call blocks until the
            // remote-introspection process is done.
            System.out.println("[Java   Local  Introspection] Calling \"remote-step\" method");
            remoteServer.call("remote-step");
        } finally {
            if (remoteServer != null) {
                remoteServer.deactivate();
            }
        }

        System.out.println("[Java   Local  Introspection] Done");

    }
}
