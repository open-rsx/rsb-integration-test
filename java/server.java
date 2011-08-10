import java.lang.InterruptedException;
import java.lang.Throwable;
import java.lang.Thread;

import rsb.InitializeException;
import rsb.Scope;
import rsb.Factory;

import rsb.patterns.LocalServer;
import rsb.patterns.DataCallback;

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

		Scope scope = new Scope("/rsbtest/clientserver");

		System.out.println("[Java   Server] Providing service on " + scope);

		try {
			LocalServer s = Factory.getInstance().createLocalServer(scope);
			s.activate();

			// Implement and register "echo" method.
			DataCallback<String, String> echo = new DataCallback<String, String>() {
				public String invoke(String request) {
					System.out
							.println("[Java   Server] \"echo\" method called");
					return request;
				}
			};
			s.addMethod("echo", echo);

			// Implement and register "error" method.
			DataCallback<String, String> error = new DataCallback<String, String>() {
				public String invoke(String request) throws Throwable {
					System.out
							.println("[Java   Server] \"error\" method called");
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
