import java.io.FileWriter;
import java.io.PrintWriter;
import java.io.IOException;

import rsb.util.Properties;

public class config {

    public static void main(String[] args) {
	if (args.length != 2) {
	    System.exit(1);
	}

	try {
	    Properties p = new Properties();
	    p.loadFile(args[0]);

	    PrintWriter stream = new PrintWriter(new FileWriter(args[1]));
	    stream.println("qualityofservice.reliability: " + p.getProperty("qualityofservice.reliability"));
	    stream.println("qualityofservice.ordering: " + p.getProperty("qualityofservice.ordering"));
	    stream.println("errorhandling.onhandlererror: " + "EXIT"); // TODO fake
	    stream.println("transport.inprocess.enabled: " + "false"); // TODO fake

	    stream.println("transport.spread.host: " + p.getProperty("transport.spread.host"));
	    stream.println("transport.spread.port: " + p.getPropertyAsInt("transport.spread.port"));
	    stream.println("transport.spread.enabled: " + p.getPropertyAsBool("transport.spread.enabled"));
	    stream.println("transport.spread.converter.java.utf-8-string: "
			   + p.getProperty("transport.spread.converter.java.utf-8-string"));
	    stream.close();
	} catch (IOException e) {
	    System.err.println("Failed to write to output file " + args[1]);
	    System.exit(1);
	}

    }

}
