import java.lang.Integer;

import java.util.ArrayList;

import rsb.Subscriber;
import rsb.event.RSBDataListener;
import rsb.event.Subscription;
import rsb.transport.TransportFactory;
import rsb.Scope;

public class listener {

    private static class DataHandler<T> extends RSBDataListener<T> {
        public DataHandler(String scope, int size, int expected) {
            this.scope = scope;
            this.size = size;
            this.expected = expected;
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

        private String scope;
        private int size;
        private int count;
        private int expected;
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
        for (int size : sizes) {
            String scopeString = "/size" + size;
            for (String component : components) {
                scopeString += component;
                Scope scope = new Scope(scopeString);
                try {
                    Subscriber subscriber = new Subscriber(scope, scope,
                            TransportFactory.getInstance());
                    subscriber.activate();
                    DataHandler<String> handler = new DataHandler<String>(
                            scope, size, 120);
                    Subscription subscription = subscriber.addListener(handler);

                    while (!handler.isDone()) {
                        Thread.sleep(10);
                    }

                    subscriber.deactivate();
                } catch (java.lang.Throwable e) {
                    System.err.println("[Java Listener] Failure for size "
                            + size + ": " + e);
                }
            }
        }
    }

}
