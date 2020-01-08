

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.nio.channels.SocketChannel;
import pb.Msgs;

public class Main {
	
	public void connect1() throws IOException, InterruptedException {
		SocketAddress addr = new InetSocketAddress("localhost", 20000);
		SocketChannel channel = SocketChannel.open(addr);
		PbMsgProtocol pbProtocol = new PbMsgProtocol(channel);
		
		Msgs.Msg.Builder mb = Msgs.Msg.newBuilder();
		mb.setName("wen");
		mb.setAge(40);

		Msgs.Msg message = mb.build();
		
		System.out.println(message);
		
		pbProtocol.writeMsg(message);
		pbProtocol.readMsg();
	}

	public static void main(String[] args) throws IOException, InterruptedException {
		new Main().connect1();
	}
}