package snowing;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.nio.channels.SocketChannel;

import snowing.commands.LoginCommand;

public class Client {

	public static void main(String[] args) throws IOException, ClassNotFoundException {
		SocketAddress addr = new InetSocketAddress("localhost", 20000);
		SocketChannel channel = SocketChannel.open(addr);
		ClientMsgProtocol protocol = new ClientMsgProtocol(channel);
		
		LoginCommand lc = new LoginCommand();
		lc.setPid("wen");
		lc.setPassword("123");
		
		protocol.writeMsg(lc.encode());
		protocol.readMsg();
	}

}
