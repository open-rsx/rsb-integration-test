import rsb.Factory;
import rsb.InitializeException;
import rsb.Scope;
import rsb.patterns.RemoteServer;

public class client {

    public static void main(String[] args) throws Throwable {

	Scope scope = new Scope("/rsbtest/clientserver");

	System.out.println("[Java   Client] Communication with remote server at " + scope);

	try {
	    RemoteServer server = Factory.getInstance().createRemoteServer(scope);
	    server.activate();

	    // Call "echo" method.
	    System.out.println("[Java   Client] Calling \"echo\" method"); 
	    String result = server.call("echo", "ping from Java");
	    if (!result.equals("ping from Java")) {
	    	System.exit(1);
	    }

	    // Call "error" method.
	    System.out.println("[Java   Client] Calling \"error\" method");
	    boolean error = false;
	    try {
	        result = server.call("error", "no sense"); 	
		error = false;
	    } catch (Throwable t) {
		error = true;
	    }
	    if (!error) {
	    	System.exit(1);
	    }

	    // Call "terminate" method.
	    System.out.println("[Java   Client] Calling \"terminate\" method");
	    result = server.call("terminate", "no sense"); 
	    server.deactivate();
	} catch (InitializeException e) {
	    e.printStackTrace();
	    System.exit(1);
	}

	System.out.println("[Java   Client] Done!");
    }

}
