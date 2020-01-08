package mynet;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.channels.SocketChannel;

/**
 * 1，把从SocketChannel读出的字节流，变成一个个的Message。 
 * 2，把Message编码成字节流写入SocketChannel。
 * 3，Message结构为：head(4byte,value=length(body)) + body(byte[])
 * 
 * 我这个实现方法，是最简单明了的，复杂的我晕！
 * 
 * @author wenjixiao
 *
 */
public abstract class AMsgProtocol {
	private final int HEADER_LEN = 4;

	private SocketChannel channel;

	public AMsgProtocol(SocketChannel channel) {
		this.channel = channel;
	}

	/**
	 * keep reading msg forever
	 */
	public void readMsg() throws IOException {
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

	public abstract void processMsg(byte[] data);

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