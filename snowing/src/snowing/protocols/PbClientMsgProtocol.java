package snowing.protocols;

import java.io.IOException;
import java.nio.channels.SocketChannel;

import snowing.Client;
import snowing.messages.pb.Msgs;

public class PbClientMsgProtocol extends AMsgProtocol {
	private Client client;
	
	public PbClientMsgProtocol(Client client,SocketChannel channel) {
		super(channel);
		this.client = client;
		// TODO Auto-generated constructor stub
	}

	@Override
	public void processMsg(byte[] data) throws IOException {
		// TODO Auto-generated method stub
		Msgs.Msg msg = Msgs.Msg.parseFrom(data);
		System.out.println(msg);
	}

}
