/* ============================================================
 *
 * This file is part of the RSB project.
 *
 * Copyright (C) 2011, 2012, 2013 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2, or (at
 * your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * ============================================================ */

import java.lang.InterruptedException;
import java.lang.Throwable;
import java.lang.Thread;

import rsb.InitializeException;
import rsb.Scope;
import rsb.Factory;

import rsb.converter.ProtocolBufferConverter;
import rsb.converter.DefaultConverterRepository;

import rsb.patterns.LocalServer;
import rsb.patterns.DataCallback;

import running.example.RunningExample.Image;

public class server {

    private static class Terminate extends DataCallback<Void, Void> {

        private boolean terminate;

        public Void invoke(Void request) {
            System.out.println("[Java   Server] \"terminate\" method called");
            synchronized (this) {
                this.terminate = true;
                this.notify();
            }
            return null;
        }

        public void waitForCall() throws InterruptedException {
            synchronized (this) {
                while (!this.terminate) {
                    this.wait();
                }
                Thread.currentThread().sleep(2000);
            }
        }
    }

    static boolean failed = false;

    public static void main(String[] args) throws Throwable {
        ProtocolBufferConverter<Image> converter = new ProtocolBufferConverter<Image>(Image.getDefaultInstance());
        DefaultConverterRepository.getDefaultConverterRepository().addConverter(converter);

        if (args.length < 2 || !args[0].equals("--cookie")) {
            System.err.println("Missing --cookie option");
            System.exit(1);
        }
        final Long cookie = Long.parseLong(args[1]);

        Scope scope = new Scope("/rsbtest/clientserver");

        System.out.println("[Java   Server] Providing service on " + scope);

        LocalServer server = null;
        try {
            server = Factory.getInstance().createLocalServer(scope);
            server.activate();

            // Implement and register "ping" method.
            DataCallback<String, Long> ping = new DataCallback<String, Long>() {
                public String invoke(Long request) throws Throwable {
                    System.out.println("[Java   Server] \"ping\" method called with request " + request);
                    if (!(request.equals(cookie))) {
                        System.err.println("Received cookie value " + request + " not equal to expected value " + cookie);
                        failed = true;
                    }
                    return "pong";
                }
            };
            server.addMethod("ping", ping);

            // Implement and register "echo" method.
            DataCallback<String, String> echo = new DataCallback<String, String>() {
                public String invoke(String request) {
                    System.out.println("[Java   Server] \"echo\" method called");
                    return request;
                }
            };
            server.addMethod("echo", echo);

            // Implement and register "addone" method.
            DataCallback<Long, Long> addOne
                = new DataCallback<Long, Long>() {
                public Long invoke(Long request) throws Throwable {
                    if (request == 0) {
                        System.out.println("[Java   Server] \"addone\" method called (for 0)");
                    }
                    return request + 1;
                }
            };
            server.addMethod("addone", addOne);

            // Implement and register "putimage" method.
            DataCallback<Object, Image> putImage = new DataCallback<Object, Image>() {
                public Object invoke(Image request) throws Throwable {
                    System.out.println("[Java   Server] \"putimage\" method called");
                    return null;
                }
            };
            server.addMethod("putimage", putImage);

            // Implement and register "error" method.
            DataCallback<String, String> error = new DataCallback<String, String>() {
                public String invoke(String request) throws Throwable {
                    System.out.println("[Java   Server] \"error\" method called");
                    throw new Throwable("intentional error");
                }
            };
            server.addMethod("error", error);

            // Register "terminate" method.
            Terminate terminate = new Terminate();
            server.addMethod("terminate", terminate);

            // Block until "terminate" method has been called.
            terminate.waitForCall();
        } catch (Throwable e) {
            e.printStackTrace();
            failed = true;
        } finally {
            if (server != null) {
                server.deactivate();
            }
        }

        if (failed) {
            System.exit(1);
        }

        System.out.println("[Java   Server] Done!");
    }

}
