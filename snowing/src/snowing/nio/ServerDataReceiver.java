package snowing.nio;

import com.google.protobuf.InvalidProtocolBufferException;

import snowing.messages.pb.Msgs;

public class ServerDataReceiver extends DataReceiver {
	
	@Override
	public void processData(byte[] data) {

		try {
			Msgs.Msg msg = Msgs.Msg.parseFrom(data);
			System.out.println("----server----");
			System.out.println(msg);
		} catch (InvalidProtocolBufferException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

}
