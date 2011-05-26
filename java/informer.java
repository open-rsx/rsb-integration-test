import java.lang.StringBuilder;

import java.util.ArrayList;

import rsb.Informer;
import rsb.InitializeException;
import rsb.Scope;

public class informer {

    public static void main(String[] args) {

        ArrayList<Integer> sizes = new ArrayList<Integer>();
        sizes.add(4);
        sizes.add(256);
        sizes.add(400000);
        for (int size : sizes) {
            Scope scope = new Scope("/size" + size + "/sub1/sub2");

            System.out.println("[Java   Informer] Processing scope " + scope);
            try {
                Informer<String> p = new Informer<String>(scope);
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
