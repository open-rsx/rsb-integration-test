/* ============================================================
 *
 * This file is part of the RSB project.
 *
 * Copyright (C) 2011, 2014 Jan Moringen jmoringe@techfak.uni-bielefeld.de
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

import java.io.FileWriter;
import java.io.PrintWriter;
import java.io.IOException;
import java.io.File;

import rsb.util.Properties;
import rsb.util.ConfigLoader;

public class ConfigTest {

    public static void main(String[] args) {
	if (args.length != 2) {
	    System.exit(1);
	}

	try {
	    Properties p = new Properties();
	    new ConfigLoader().loadFile(new File(args[0]), p);

	    PrintWriter stream = new PrintWriter(new FileWriter(args[1]));
	    stream.println("qualityofservice.reliability: " + p.getProperty("qualityofservice.reliability").asString());
	    stream.println("qualityofservice.ordering: " + p.getProperty("qualityofservice.ordering").asString());
	    stream.println("errorhandling.onhandlererror: " + p.getProperty("errorhandling.onhandlererror").asString());
	    stream.println("transport.inprocess.enabled: " + p.getProperty("transport.inprocess.enabled").asBoolean());

	    stream.println("transport.spread.host: " + p.getProperty("transport.spread.host").asString());
	    stream.println("transport.spread.port: " + p.getProperty("transport.spread.port").asString());
	    stream.println("transport.spread.enabled: " + p.getProperty("transport.spread.enabled").asBoolean());
	    stream.println("transport.spread.converter.java.utf-8-string: "
			   + p.getProperty("transport.spread.converter.java.utf-8-string").asString());
	    stream.close();
	} catch (IOException e) {
	    System.err.println("Failed to write to output file " + args[1]);
	    System.exit(1);
	}

    }

}
