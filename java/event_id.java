/* ============================================================
 *
 * This file is part of the RSB project.
 *
 * Copyright (C) 2011 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
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

import java.util.UUID;

import java.io.Reader;
import java.io.FileReader;
import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;

import rsb.EventId;
import rsb.ParticipantId;

public class event_id {

    public static void main(String[] args) throws IOException{
	File file = new File("data/event-id-cases.txt");
	BufferedReader reader = new BufferedReader(new FileReader(file));
	while (reader.ready()) {
	    String line = reader.readLine();
	    String[] tokens = line.split(" ");
	    ParticipantId originId = new ParticipantId(tokens[0]);
	    long sequenceNumber = Long.parseLong(tokens[1], 16);
	    UUID expectedId = UUID.fromString(tokens[2]);
	    EventId id = new EventId(originId, sequenceNumber);
	    if (!expectedId.equals(id.getAsUUID())) {
		System.exit(1);
	    }
	}

    }

};
