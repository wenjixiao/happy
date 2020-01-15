package snowing.nio;

import com.google.protobuf.InvalidProtocolBufferException;

import snowing.messages.pb.Msgs;
import snowing.messages.pb.Msgs.Msg;

public class ClientDataReceiver extends DataReceiver {

	@Override
	public void processMsg(byte[] data) {
		Msg msg;
		try {
			msg = Msgs.Msg.parseFrom(data);
			System.out.println("----client----");
			System.out.println(msg);
		} catch (InvalidProtocolBufferException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		
	}

}
