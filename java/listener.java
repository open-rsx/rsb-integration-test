import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingDeque;
import java.util.concurrent.TimeUnit;

import rsb.RSBException;
import rsb.Subscriber;
import rsb.event.RSBDataListener;
import rsb.event.Subscription;
import rsb.transport.TransportFactory;
import rsb.InitializeException;
import rsb.naming.NotFoundException;

public class listener {

	private static class RSBQueueAdapter<T> extends RSBDataListener<T> {
		BlockingQueue<T> queue;

		public RSBQueueAdapter() {
			this(new LinkedBlockingDeque<T>());
		}

		public RSBQueueAdapter(BlockingQueue<T> queue) {
			this.queue = queue;
		}

		@Override
		public void handleEvent(T data) {
			queue.add(data);
		}

		public BlockingQueue<T> getQueue() {
			return queue;
		}
	}

	public static void main(String[] args) {

		try {

			Subscriber subscriber = new Subscriber("/example/informer",
					"/example/informer", TransportFactory.getInstance());
			subscriber.activate();
			RSBQueueAdapter<String> queueAdapter = new RSBQueueAdapter<String>();
			Subscription subscription = subscriber.addListener(queueAdapter);
			BlockingQueue<String> inputQueue = queueAdapter.getQueue();

			for (int j = 1; j <= 1200; j++) {
				String message = inputQueue.poll(10000, TimeUnit.MILLISECONDS);
				if (message == null) {
					System.err.println("No messages received in 10 seconds");
					System.exit(1);
				} else {
					System.out.println("Received " + j + "/1200: " + message);
				}
			}
			
			subscriber.deactivate();

		} catch (Throwable e) {
			e.printStackTrace();
			System.exit(1);
		}

	}

}
