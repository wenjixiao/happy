

import java.io.IOException;
import java.nio.channels.SocketChannel;
import pb.Msgs;

import com.google.protobuf.InvalidProtocolBufferException;

public class ClientMsgProtocol extends AMsgProtocol {

	public ClientMsgProtocol(SocketChannel channel) {
		super(channel);
	}

	@Override
	public void processMsg(byte[] data) {
		try {
			Msgs.Msg msg = Msgs.Msg.parseFrom(data);
			System.out.printf("client get msg: %s",msg);
		} catch (InvalidProtocolBufferException e) {
			e.printStackTrace();
		}
	}
	
	public void writeMsg(Msgs.Msg msg) throws IOException {
		writeMsg(msg.toByteArray());
	}
	
}
