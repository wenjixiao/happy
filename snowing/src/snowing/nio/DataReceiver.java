package snowing.nio;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.channels.SocketChannel;

import com.google.protobuf.InvalidProtocolBufferException;

public abstract class DataReceiver {
	private  static final int BUF_LEN = 1024 * 6;
	public static final int HEADER_LEN = 4;
	
	private int bodyLen;
	private ByteBuffer buf;

	public DataReceiver() {
		bodyLen = 0;
		buf = ByteBuffer.allocate(BUF_LEN);
	}

	public void receiveData(ByteBuffer buffer) throws IOException {
		buf.put(buffer);
		buf.flip();

		while (buf.hasRemaining()) {
			int n = buf.remaining();
			if (bodyLen == 0) {
				if (n >= HEADER_LEN) {
					byte[] header = new byte[HEADER_LEN];
					buf = buf.get(header);
					ByteBuffer headBuf = ByteBuffer.wrap(header).order(ByteOrder.LITTLE_ENDIAN);
					bodyLen = headBuf.getInt();
				} else {
					break;
				}
			}

			if (bodyLen > 0) {
				if (buf.remaining() >= bodyLen) {
					byte[] body = new byte[bodyLen];
					buf = buf.get(body);
					processMsg(body);
					bodyLen = 0;
				}
				break;
			}
		}

		buf.compact();
	}

	public static void writeMsg(SocketChannel channel,byte[] msgBody) throws IOException {
		int bodyLen = msgBody.length;
		ByteBuffer buf = ByteBuffer.allocate(HEADER_LEN + bodyLen);

		ByteBuffer headBuf = ByteBuffer.allocate(HEADER_LEN).order(ByteOrder.LITTLE_ENDIAN);
		headBuf.putInt(bodyLen);

		headBuf.flip();
		buf.put(headBuf);
		buf.put(msgBody);

		buf.flip();
		channel.write(buf);
	}

	public abstract void processMsg(byte[] data) throws IOException;

}
