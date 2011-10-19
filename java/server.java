/* ============================================================
 *
 * This file is part of the RSB project.
 *
 * Copyright (C) 2011 Jan Moringen jmoringe@techfak.uni-bielefeld.de
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

    private static class Terminate implements DataCallback<String, String> {

	private boolean terminate;

	public String invoke(String request) {
	    System.out.println("[Java   Server] \"terminate\" method called");
	    synchronized (this) {
		this.terminate = true;
		this.notify();
	    }
	    return "";
	}

	public void waitForCall() throws InterruptedException {
	    synchronized (this) {
		while (!this.terminate) {
		    this.wait();
		}
	    }
	}
    }

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

	try {
	    LocalServer s = Factory.getInstance().createLocalServer(scope);
	    s.activate();

	    // Implement and register "ping" method.
	    DataCallback<String, Long> ping = new DataCallback<String, Long>() {
		public String invoke(Long request) throws Throwable {
		    System.out.println("[Java   Server] \"ping\" method called with request " + request);
		    if (!(request.equals(cookie))) {
			System.err.println("Received cookie value " + request + " not equal to expected value " + cookie);
			System.exit(1);
		    }
		    return "pong";
		}
	    };
	    s.addMethod("ping", ping);

	    // Implement and register "echo" method.
	    DataCallback<String, String> echo = new DataCallback<String, String>() {
		public String invoke(String request) {
		    System.out.println("[Java   Server] \"echo\" method called");
		    return request;
		}
	    };
	    s.addMethod("echo", echo);

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
	    s.addMethod("addone", addOne);

	    // Implement and register "putimage" method.
	    DataCallback<Object, Image> putImage = new DataCallback<Object, Image>() {
		public Object invoke(Image request) throws Throwable {
		    System.out.println("[Java   Server] \"putimage\" method called");
		    return null;
		}
	    };
	    s.addMethod("putimage", putImage);

	    // Implement and register "error" method.
	    DataCallback<String, String> error = new DataCallback<String, String>() {
		public String invoke(String request) throws Throwable {
		    System.out.println("[Java   Server] \"error\" method called");
		    throw new Throwable("intentional error");
		}
	    };
	    s.addMethod("error", error);

	    // Register "terminate" method.
	    Terminate terminate = new Terminate();
	    s.addMethod("terminate", terminate);

	    // Block until "terminate" method has been called.
	    terminate.waitForCall();

	    s.deactivate();
	} catch (InitializeException e) {
	    e.printStackTrace();
	    System.exit(1);
	}

	System.out.println("[Java   Server] Done!");
    }

}
