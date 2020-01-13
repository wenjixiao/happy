package snowing.protocols;

import java.io.IOException;
import java.nio.channels.SocketChannel;

import snowing.Server;
import snowing.messages.pb.Msgs;

public class PbServerMsgProtocol extends AMsgProtocol {
	private Server server;
	
	public PbServerMsgProtocol(Server server,SocketChannel channel) {
		super(channel);
		this.server = server;
		// TODO Auto-generated constructor stub
	}

	@Override
	public void processMsg(byte[] data) throws IOException {
		// TODO Auto-generated method stub
		Msgs.Msg msg = Msgs.Msg.parseFrom(data);
		System.out.println(msg);
		Msgs.Msg.Builder mb = Msgs.Msg.newBuilder();
		mb.setType(Msgs.Type.LOGIN_RESULT);
		mb.getLoginResultBuilder().setSuccess(true);
		writeMsg(mb.build().toByteArray());
	}

}
