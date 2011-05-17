import java.lang.StringBuilder;

import java.util.ArrayList;

import rsb.Publisher;
import rsb.InitializeException;

public class informer {

    public static void main(String[] args) {

        ArrayList<Integer> sizes = new ArrayList<Integer>();
        sizes.add(4);
        sizes.add(256);
        sizes.add(400000);
        for (int size : sizes) {
            String scope = "/size" + size + "/sub1/sub2";

            System.out.println("[Java   Informer] Processing scope " + scope);
            try {
                Publisher<String> p = new Publisher<String>(scope);
                p.activate();

                StringBuilder builder = new StringBuilder();
                for (int i = 0; i < size; ++i) {
                    builder.append('c');
                }
                String data = builder.toString();

                for (int j = 1; j <= 120; j++) {
                    p.send(data);
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
