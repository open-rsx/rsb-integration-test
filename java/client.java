import java.lang.Thread;

import rsb.InitializeException;
import rsb.Scope;
import rsb.Factory;

public class client {

    public static void main(String[] args) {

	Scope scope = new Scope(scopeString);
	System.err.println("[Java   Client] Communication with remote server at " + scope);

	try {
	    /*RemoteServer s = Factory.getInstance().createRemoteServer(scope);
	    s.activate();

	    // TODO

	    s.deactivate();*/

	    Thread.sleep(1000);

	} catch (InitializeException e) {
	    e.printStackTrace();
	    System.exit(1);
	}

	System.err.println("[Java   Client] done!");
    }

}
