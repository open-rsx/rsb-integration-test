
import rsb.Publisher;
import rsb.InitializeException;

public class informer {

	public static void main(String[] args) {

		try	{
		
			Publisher<String> p = new Publisher<String>("/example/informer");
			p.activate();
			
			for (int j = 1; j <= 1200; j++) {
				p.send("This is a test message " + j);
			}
			
			System.out.println("Sent messages");
			
			p.deactivate();
			
		} catch(InitializeException e) {
			e.printStackTrace();
			System.exit(1);
		}
		
	}

}
