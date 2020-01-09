

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.nio.channels.SocketChannel;
import pb.Msgs;

public class Main {
	
	public void connect() throws IOException, InterruptedException {
		SocketAddress addr = new InetSocketAddress("localhost", 20000);
		SocketChannel channel = SocketChannel.open(addr);
		PbMsgProtocol protocol = new PbMsgProtocol(channel);

		Msgs.Login.Builder loginBuilder = Msgs.Login.newBuilder();
		loginBuilder.setPid("wen");
		loginBuilder.setPassword("123");
		
		Msgs.Msg.Builder mb = Msgs.Msg.newBuilder();
		mb.setType(Msgs.Type.LOGIN);
		mb.setLogin(loginBuilder);

		Msgs.Msg message = mb.build();
		
		System.out.println(message);
		
		protocol.writeMsg(message);
		protocol.readMsg();
	}

	public static void main(String[] args) throws IOException, InterruptedException {
		new Main().connect();
	}
}