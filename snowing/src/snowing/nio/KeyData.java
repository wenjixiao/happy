package snowing.nio;

public class KeyData {
	private ServerDataReceiver dataReceiver;
	private byte[] msgBody;

	public KeyData() {
		dataReceiver = new ServerDataReceiver();
	}

	public ServerDataReceiver getDataReceiver() {
		return dataReceiver;
	}

	public byte[] getMsgBody() {
		return msgBody;
	}

	public void setMsgBody(byte[] msgBody) {
		this.msgBody = msgBody;
	}


}
