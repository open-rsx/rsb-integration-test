import java.lang.Throwable;
import java.lang.Thread;

import rsb.InitializeException;
import rsb.Scope;
import rsb.Factory;

import rsb.patterns.RemoteServer;

public class client {

    public static void main(String[] args) throws Throwable {

	Scope scope = new Scope("/rsbtest/clientserver");

	System.out.println("[Java   Client] Communication with remote server at " + scope);

	try {
	    RemoteServer server = Factory.getInstance().createRemoteServer(scope);
	    server.activate();

	    // Call "echo" method.
	    System.out.println("[Java   Client] calling \"echo\" method");
	    //assert(server.call<String, String>("echo", "bla") == "bla");

	    // Call "error" method.
	    System.out.println("[Java   Client] calling \"error\" method");
	    boolean error = false;
	    try {
		//server.call<String, String>("echo", "error");
		error = true;
	    } catch (Throwable t) {
	    }
	    assert(error);

	    // Call "terminate" method.
	    System.out.println("[Java   Client] calling \"terminate\" method");
	    //server.call<String, String>("terminate", "");

	    server.deactivate();

	    throw new InitializeException(); // TODO will be thrown for real later
	} catch (InitializeException e) {
	    e.printStackTrace();
	    System.exit(1);
	}

	System.out.println("[Java   Client] done!");
    }

}
