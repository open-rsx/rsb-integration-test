/* ============================================================
 *
 * This file is part of the RSB project.
 *
 * Copyright (C) 2011-2016 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
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

import java.util.List;
import java.util.ArrayList;

import java.util.concurrent.Future;

import com.google.protobuf.ByteString;

import rsb.Factory;
import rsb.InitializeException;
import rsb.Scope;

import rsb.converter.ProtocolBufferConverter;
import rsb.converter.DefaultConverterRepository;

import rsb.patterns.RemoteServer;

import running.example.RunningExample.Image;

public class ClientTest {

    public static void main(String[] args) throws Throwable {
        ProtocolBufferConverter<Image> converter = new ProtocolBufferConverter<Image>(Image.getDefaultInstance());
        DefaultConverterRepository.getDefaultConverterRepository().addConverter(converter);

        Long cookie = 0L;
        if (args[0].equals("--cookie")) {
            cookie = Long.parseLong(args[1]);
        }

        Scope scope = new Scope("/rsb-integration-test/request-reply");

        System.out.println("[Java   Client] Communicating with remote server at " + scope);

        boolean failed = false;
        RemoteServer server = null;
        try {
            server = Factory.getInstance().createRemoteServer(scope);
            server.activate();

            // Call "ping" method.
            System.out.println("[Java   Client] Calling \"ping\" method");
            if (!server.call("ping", cookie).equals("pong")) {
                throw new Throwable("Incorrect reply from \"ping\" method");
            }

            // Call "echoBoolean" method.
            {
                Boolean value = true;
                System.out.println("[Java   Client] Calling \"echoBoolean\" method with argument "
                                   + value);
                if (!server.call("echoBoolean", value).equals(value)) {
                    throw new Throwable("Incorrect reply from \"echoBoolean\" method");
                }
            }

            // Call "echoInt32" method.
            // {
            //     Integer value = new Integer(-1);
            //     System.out.println("[Java   Client] Calling \"echoInt32\" method with argument "
            //                        + value);
            //     if (!server.call("echoInt32", value).equals(value)) {
            //         throw new Throwable("Incorrect reply from \"echoInt32\" method");
            //     }
            // }

            // Call "echoInt64" method.
            {
                Long value = new Long(1099511627776L);
                System.out.println("[Java   Client] Calling \"echoInt64\" method with argument "
                                   + value);
                if (!server.call("echoInt64", value).equals(value)) {
                    throw new Throwable("Incorrect reply from \"echoInt64\" method");
                }
            }

            // Call "echoFloat" method.
            // {
            //     Float value = new Float(1.2345f);
            //     System.out.println("[Java   Client] Calling \"echoFloat\" method with argument "
            //                        + value);
            //     if (!server.call("echoFloat", value).equals(value)) {
            //         throw new Throwable("Incorrect reply from \"echoFloat\" method");
            //     }
            // }

            // Call "echoDouble" method.
            {
                Double value = new Double(1e300);
                System.out.println("[Java   Client] Calling \"echDoubleo\" method with argument "
                                   + value);
                if (!server.call("echoDouble", value).equals(value)) {
                    throw new Throwable("Incorrect reply from \"echoDouble\" method");
                }
            }

            // Call "echoString" method.
            {
                String value = "hello from Java";
                System.out.println("[Java   Client] Calling \"echStringo\" method with argument "
                                   + value);
                if (!server.call("echoString", value).equals(value)) {
                    throw new Throwable("Incorrect reply from \"echoString\" method");
                }
            }

            // Call "echoScope" method.
            {
                Scope value = new Scope("/scope");
                System.out.println("[Java   Client] Calling \"echoScope\" method with argument "
                                   + value);
                if (!server.call("echoScope", value).equals(value)) {
                    throw new Throwable("Incorrect reply from \"echoScope\" method");
                }
            }

            // Test calling "addone" method synchronously and asynchronously.
            System.out.println("[Java   Client] Calling \"addone\" method (100 times, synchronous)");
            for (long i = 0; i < 100; ++i) {
                Long result = server.<Long, Long>call("addone", new Long(i));
                if (result != i + 1) {
                    throw new Throwable("Incorrect result for " + i + "-th call: " + result);
                }
            }

            System.out.println("[Java   Client] Calling \"addone\" method (100 times, asynchronous)");
            List< Future<Long> > futures = new ArrayList< Future<Long> >();
            for (long i = 0; i < 100; ++i) {
                Future<Long> future = server.callAsync("addone", i);
                futures.add(future);
            }
            int i = 0;
            for (Future<Long> future : futures) {
                Long result = future.get();
                if (result != i + 1) {
                    throw new Throwable("Incorrect result for " + i + "-th call: " + result);
                }
                i += 1;
            }

            // Call "putimage" method
            System.out.println("[Java   Client] Calling \"putimage\" method");
            byte[] temp = new byte[3 * 1024 * 1024];
            Image image = Image.newBuilder()
                .setWidth(100)
                .setHeight(100)
                .setData(com.google.protobuf.ByteString.copyFrom(temp))
                .build();
            server.call("putimage", image);

            // Call "error" method.
            {
                System.out.println("[Java   Client] Calling \"error\" method");
                String result = null;
                boolean error = false;
                try {
                    result = server.call("error", "no sense");
                    error = false;
                } catch (Throwable t) {
                    error = true;
                }
                if (!error) {
                    throw new Throwable("Expected exception has not been thrown");
                }
            }

            // Call "terminate" method.
            {
                System.out.println("[Java   Client] Calling \"terminate\" method");
                server.callAsync("terminate");
            }
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

        System.out.println("[Java   Client] Done");
    }

}
