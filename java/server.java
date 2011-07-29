import java.lang.Thread;

import rsb.InitializeException;
import rsb.Scope;
import rsb.Factory;

public class server {

    public static void main(String[] args) throws Throwable {

	Scope scope = new Scope("/rsbtest/clientserver");

	System.out.println("[Java   Server] Providing service on " + scope);

	try {
	    /*Server s = Factory.getInstance().createServer(scope);
	    s.activate();

	    // TODO

	    s.deactivate();*/

	    Thread.sleep(1000);

	} catch (InitializeException e) {
	    e.printStackTrace();
	    System.exit(1);
	}

	System.out.println("[Java   Server] Sent messages");
    }

}
