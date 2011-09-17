import java.util.List;
import java.util.ArrayList;

import java.util.concurrent.Future;

import com.google.protobuf.ByteString;

import rsb.Factory;
import rsb.InitializeException;
import rsb.Scope;

import rsb.converter.ProtocolBufferConverter;
import rsb.converter.DefaultConverterRepository;

import rsb.patterns.RemoteServer;

import running.example.RunningExample.Image;

public class client {

    public static void main(String[] args) throws Throwable {
	ProtocolBufferConverter<Image> converter = new ProtocolBufferConverter<Image>(Image.getDefaultInstance());
	DefaultConverterRepository.getDefaultConverterRepository().addConverter(converter);

	Long cookie = 0L;
	if (args[0].equals("--cookie")) {
	    cookie = Long.parseLong(args[1]);
	}

	Scope scope = new Scope("/rsbtest/clientserver");

	System.out.println("[Java   Client] Communicating with remote server at " + scope);

	try {
	    RemoteServer server = Factory.getInstance().createRemoteServer(scope);
	    server.activate();

	    // Call "ping" method.
	    System.out.println("[Java   Client] Calling \"ping\" method");
	    if (!server.call("ping", cookie).equals("pong")) {
		System.err.println("Incorrect reply from \"ping\" method");
		System.exit(1);
	    }

	    // Call "echo" method.
	    System.out.println("[Java   Client] Calling \"echo\" method");
	    {
		String result = server.call("echo", "hello from Java");
		if (!result.equals("hello from Java")) {
		    System.err.println("Incorrect reply from \"echo\" method");
		    System.exit(1);
		}
	    }

	    // Test calling "addone" method synchronously and asynchronously.
	    System.out.println("[Java   Client] Calling \"addone\" method (100 times, synchronous)");
	    for (long i = 0; i < 100; ++i) {
		Long result = server.call("addone", i);
		if (result != i + 1) {
		    System.err.println("Incorrect result for " + i + "-th call: " + result);
		    System.exit(1);
		}
	    }

	    System.out.println("[Java   Client] Calling \"addone\" method (100 times, asynchronous)");
	    List< Future<Long> > futures = new ArrayList< Future<Long> >();
	    for (long i = 0; i < 100; ++i) {
		Future<Long> future = server.callAsync("addone", i);
		futures.add(future);
		}
	    int i = 0;
	    for (Future<Long> future : futures) {
		Long result = future.get();
		if (result != i + 1) {
		    System.err.println("Incorrect result for " + i + "-th call: " + result);
		    System.exit(1);
		}
		i += 1;
	    }

	    // Call "putimage" method
	    System.out.println("[Java   Client] Calling \"putimage\" method");
	    byte[] temp = new byte[3 * 1024 * 1024];
	    Image image = Image.newBuilder()
		.setWidth(100)
		.setHeight(100)
		.setData(com.google.protobuf.ByteString.copyFrom(temp))
		.build();
	    server.call("putimage", image);

	    // Call "error" method.
	    {
		System.out.println("[Java   Client] Calling \"error\" method");
		String result = null;
		boolean error = false;
		try {
		    result = server.call("error", "no sense");
		    error = false;
		} catch (Throwable t) {
		    error = true;
		}
		if (!error) {
		    System.exit(1);
		}
	    }

	    // Call "terminate" method.
	    {
		System.out.println("[Java   Client] Calling \"terminate\" method");
		String result = server.call("terminate", "no sense");
	    }

	    server.deactivate();

	    System.out.println("[Java   Client] Deactivate complete");

	} catch (InitializeException e) {
	    e.printStackTrace();
	    System.exit(1);
	}

	System.out.println("[Java   Client] Done!");
    }

}
