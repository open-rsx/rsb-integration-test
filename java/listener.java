import java.lang.Integer;
import java.lang.Runnable;
import java.lang.Throwable;
import java.lang.Thread;
import java.lang.InterruptedException;

import java.io.File;
import java.io.IOException;

import java.util.ArrayList;

import rsb.Subscriber;
import rsb.event.RSBDataListener;
import rsb.event.Subscription;
import rsb.transport.TransportFactory;
import rsb.Scope;

public class listener {

    private static class DataHandler<T> extends RSBDataListener<T>
	                                implements Runnable {
        public DataHandler(Scope scope, int size, int expected) throws Throwable {
            this.scope = scope;
            this.size = size;
            this.expected = expected;

	    this.subscriber = new Subscriber(scope, scope, TransportFactory.getInstance());
	    this.subscriber.activate();
	    this.subscription = subscriber.addListener(this);
        }

        @Override
        public void handleEvent(T data) {
            this.count++;
            System.out.println("[Java  Listener] Scope " + scope
                    + ": received " + this.count + "/" + this.expected + ": "
                    + data);
        }

        public boolean isDone() {
            return this.count == this.expected;
        }

	public void run() {
	    while (!isDone()) {
		try {
		    Thread.sleep(1);
		} catch (InterruptedException e) {
		    e.printStackTrace();
		}
	    }
	    this.subscriber.deactivate();
	}

        private Scope scope;
        private int size;
        private int count;
        private int expected;

	private Subscriber subscriber;
	private Subscription subscription;
    }

    public static void main(String[] args) {

        ArrayList<Integer> sizes = new ArrayList<Integer>();
        sizes.add(4);
        sizes.add(256);
        sizes.add(400000);
        ArrayList<String> components = new ArrayList<String>();
        components.add("/");
        components.add("sub1");
        components.add("/sub2");
	ArrayList<Thread> listeners = new ArrayList<Thread>();
        for (int size : sizes) {
            String scopeString = "/size" + size;
            for (String component : components) {
                scopeString += component;
                Scope scope = new Scope(scopeString);
                try {
		    Thread thread = new Thread(new DataHandler<String>(scope, size, 120));
		    thread.start();
		    listeners.add(thread);
                } catch (java.lang.Throwable e) {
                    System.err.println("[Java Listener] Failure for size "
                            + size + ": " + e);
		    System.exit(1);
                }
            }
        }

	try {
	    File file = new File("test/java-listener-ready");
	    file.createNewFile();
	    System.err.println("[Java Listener] Created marker file.");
	} catch (IOException e) {
	    System.err.println("[Java Listener] Could not create marker file.");
	    System.exit(1);
	}

	for (Thread listener : listeners) {
	    try {
		listener.join();
	    } catch (InterruptedException e) {
		System.err.println("[Java Listener] Interrupted while waiting for thread.");
		System.exit(1);
	    }
	}
    }

}
