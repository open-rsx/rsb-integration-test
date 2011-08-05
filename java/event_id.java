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
	    System.out.println("" + originId + " " + sequenceNumber + " => " + id.getAsUUID() + " [" + expectedId + "]");
	    if (!expectedId.equals(id.getAsUUID())) {
		System.exit(1);
	    }
	}

    }

};