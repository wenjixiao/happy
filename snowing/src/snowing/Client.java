package snowing;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.nio.channels.SocketChannel;

import com.fasterxml.jackson.databind.ObjectMapper;

import snowing.messages.Login;
import snowing.messages.Message;

public class Client {
	private ObjectMapper mapper;
	
	public Client() {
		mapper = new ObjectMapper();
	}

	public static void main(String[] args) throws ClassNotFoundException, IOException {
		Client client = new Client();
		client.connect();
	}
	
	public ObjectMapper getMapper() {
		return mapper;
	}
	
	public void connect() throws IOException, ClassNotFoundException {
		SocketAddress addr = new InetSocketAddress("localhost", 20000);
		SocketChannel channel = SocketChannel.open(addr);
		ClientMsgProtocol protocol = new ClientMsgProtocol(this,channel);
		
		Login login = new Login();
		login.setPid("wen");
		login.setPassword("123");
		
		Message<Login> message = new Message<Login>();
		message.setType(Message.Type.Login);
		message.setMsg(login);
		
		protocol.writeMsg(mapper.writeValueAsBytes(message));
		
		protocol.readMsg();
	}

}
