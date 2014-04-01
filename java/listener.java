/* ============================================================
 *
 * This file is part of the RSB project.
 *
 * Copyright (C) 2011, 2012, 2014 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
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

import java.lang.Integer;
import java.lang.Runnable;
import java.lang.Throwable;
import java.lang.Thread;
import java.lang.InterruptedException;

import java.io.File;
import java.io.IOException;

import java.util.ArrayList;

import rsb.ParticipantId;
import rsb.EventId;
import rsb.Event;
import rsb.Scope;
import rsb.Factory;
import rsb.Listener;
import rsb.AbstractEventHandler;
import rsb.transport.TransportFactory;
import rsb.RSBException;

public class listener {

    private static class EventHandler extends AbstractEventHandler
                                      implements Runnable {

        public EventHandler(Scope listenScope,
                            Scope expectedScope,
                            int expectedSize,
                            int expectedCount)
            throws Throwable {
            this.expectedScope = expectedScope;
            this.expectedSize = expectedSize;

            this.expectedCount = expectedCount;

            this.listener = Factory.getInstance().createListener(listenScope);
            this.listener.activate();
            listener.addHandler(this, true);
        }

        @Override
        public void handleEvent(Event event) {
            String data = (String) event.getData();

            this.count++;
        }

        public boolean isDone() {
            return this.count == this.expectedCount;
        }

        public void run() {
            while (!isDone()) {
                try {
                    Thread.sleep(1);
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
            }
            try {
                this.listener.deactivate();
            } catch (RSBException e) {
                throw new RuntimeException(e);
            } catch (InterruptedException e) {
                throw new RuntimeException(e);
            }
        }

        private final Scope expectedScope;
        private final int expectedSize;
        private final EventId expectedCause = new EventId(new ParticipantId("00000000-0000-0000-0000-000000000000"), 0);

        private int count;
        private final int expectedCount;

        private Listener listener;
    }

    public static void main(String[] args) {

        ArrayList<Integer> sizes = new ArrayList<Integer>();
        sizes.add(4);
        sizes.add(256);
        sizes.add(400000);
        ArrayList<String> components = new ArrayList<String>();
        components.add("/");
        components.add("sub_1");
        components.add("/sub_2");
        ArrayList<Thread> listeners = new ArrayList<Thread>();
        for (int size : sizes) {
            String scopeString = "/size-" + size;
            for (String component : components) {
                scopeString += component;
                Scope scope = new Scope(scopeString);
                try {
                    Thread thread = new Thread(new EventHandler(scope,
                                                                new Scope("/size-" + size),
                                                                size,
                                                                120));
                    thread.start();
                    listeners.add(thread);
                    Thread.sleep(1);
                } catch (java.lang.Throwable e) {
                    System.err.println("[Java   Listener] Failure for size "
                                       + size + ": " + e);
                    System.exit(1);
                }
            }
        }

        try {
            File file = new File("test/java-listener-ready");
            file.createNewFile();
            System.err.println("[Java   Listener] Ready");
        } catch (IOException e) {
            System.err.println("[Java   Listener] Could not create marker file.");
            System.exit(1);
        }

        for (Thread listener : listeners) {
            try {
                listener.join();
            } catch (InterruptedException e) {
                System.err
                    .println("[Java   Listener] Interrupted while waiting for thread.");
                System.exit(1);
            }
        }
        System.err.println("[Java   Listener] Done");
    }

}
