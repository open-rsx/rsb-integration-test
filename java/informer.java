import java.util.ArrayList;

import rsb.Factory;
import rsb.Informer;
import rsb.InitializeException;
import rsb.Scope;
import rsb.MetaData;
import rsb.Event;

public class informer {

    public static void main(String[] args) {

        long startTime = System.currentTimeMillis() * 1000;
        int listenerPid = Integer.parseInt(args[1]);

        ArrayList<Integer> sizes = new ArrayList<Integer>();
        sizes.add(4);
        sizes.add(256);
        sizes.add(400000);
        for (int size : sizes) {
            Scope scope = new Scope("/size" + size + "/sub1/sub2");

            System.out.println("[Java   Informer] Processing scope " + scope);
            try {
                Informer<String> p = Factory.getInstance().createInformer(scope);
                p.activate();

                StringBuilder builder = new StringBuilder();
                for (int i = 0; i < size; ++i) {
                    builder.append('c');
                }
                String data = builder.toString();

                Event event = new Event(scope, "string", data);
                MetaData metaData = event.getMetaData();
                metaData.setUserInfo("informer-lang", "Java");
                metaData.setUserTime("informer-start", startTime);

                for (int j = 1; j <= 120; j++) {
                    metaData.setUserInfo("index", "" + (listenerPid + j));
                    p.send(event);
                }

                System.out.println("[Java   Informer] Sent messages");

                p.deactivate();

            } catch (InitializeException e) {
                e.printStackTrace();
                System.exit(1);
            }
        }
    }

}
