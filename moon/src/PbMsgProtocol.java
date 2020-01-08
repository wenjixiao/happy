

import java.io.IOException;
import java.nio.channels.SocketChannel;
import pb.Msgs;

import com.google.protobuf.InvalidProtocolBufferException;

public class PbMsgProtocol extends AMsgProtocol {

	public PbMsgProtocol(SocketChannel channel) {
		super(channel);
		// TODO Auto-generated constructor stub
	}

	@Override
	public void processMsg(byte[] data) {
		try {
			Msgs.Msg msg = Msgs.Msg.parseFrom(data);
			System.out.println(msg);
		} catch (InvalidProtocolBufferException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	public void writeMsg(Msgs.Msg msg) throws IOException {
		writeMsg(msg.toByteArray());
	}
	
}
