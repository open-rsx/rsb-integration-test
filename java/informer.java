/* ============================================================
 *
 * This file is part of the RSB project.
 *
 * Copyright (C) 2011, 2012 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
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

import java.util.ArrayList;

import rsb.Factory;
import rsb.Informer;
import rsb.InitializeException;
import rsb.Scope;
import rsb.MetaData;
import rsb.Event;
import rsb.EventId;
import rsb.ParticipantId;

public class informer {

    public static void main(String[] args) throws Throwable {

        long startTime = System.currentTimeMillis() * 1000;
        int listenerPid = Integer.parseInt(args[1]);

        ArrayList<Integer> sizes = new ArrayList<Integer>();
        sizes.add(4);
        sizes.add(256);
        sizes.add(400000);
        for (int size : sizes) {
            Scope scope = new Scope("/size" + size + "/sub1/sub2");

            System.out.println("[Java   Informer] Processing scope " + scope);
            try {
                Informer<String> p = Factory.getInstance().createInformer(scope);
                p.activate();

                StringBuilder builder = new StringBuilder();
                for (int i = 0; i < size; ++i) {
                    builder.append('c');
                }
                String data = builder.toString();

                Event event = new Event(scope, String.class, data);
                event.addCause(new EventId(new ParticipantId("00000000-0000-0000-0000-000000000000"), 0));
                MetaData metaData = event.getMetaData();
                metaData.setUserInfo("informer-lang", "Java");
                metaData.setUserTime("informer-start", startTime);

                for (int j = 1; j <= 120; j++) {
                    metaData.setUserInfo("index", "" + (listenerPid + j));
                    p.send(event);
                }

                p.deactivate();

            } catch (InitializeException e) {
                e.printStackTrace();
                System.exit(1);
            }
        }
    }

}
