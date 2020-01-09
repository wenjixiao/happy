package snowing;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.channels.SocketChannel;

public abstract class AMsgProtocol {
	private final int HEADER_LEN = 4;

	private SocketChannel channel;

	public AMsgProtocol(SocketChannel channel) {
		this.channel = channel;
	}

	/**
	 * keep reading msg forever
	 * 
	 * @throws IOException
	 * @throws ClassNotFoundException 
	 */
	public void readMsg() throws IOException, ClassNotFoundException {
		final int BUF_LEN = 1024 * 6;
		ByteBuffer buf = ByteBuffer.allocate(BUF_LEN);
		int bodyLen = 0;
		while (channel.read(buf) != -1) {
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
					} else {
						break;
					}
				}
			}

			buf.compact();
		}
	}

	public abstract void processMsg(byte[] data) throws IOException, ClassNotFoundException;

	public void writeMsg(byte[] msgBody) throws IOException {
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
}