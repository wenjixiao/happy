package snowing;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.nio.channels.SocketChannel;

import com.fasterxml.jackson.databind.ObjectMapper;

import snowing.messages.json.Login;
import snowing.messages.json.Message;
import snowing.messages.pb.Msgs;
import snowing.protocols.JsonClientMsgProtocol;
import snowing.protocols.PbClientMsgProtocol;

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
		PbClientMsgProtocol protocol = new PbClientMsgProtocol(this,channel);
		
		Msgs.Msg.Builder mb = Msgs.Msg.newBuilder();
		mb.setType(Msgs.Type.LOGIN);
		mb.getLoginBuilder().setPid("wen");
		mb.getLoginBuilder().setPassword("123");
		
		protocol.writeMsg(mb.build().toByteArray());
		
		protocol.readMsg();
	}

}
