package snowing.nio;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.channels.ServerSocketChannel;
import java.nio.channels.SocketChannel;
import java.util.Iterator;

import snowing.messages.pb.Msgs;

public class NioServer {

	public static void main(String[] args) {
		NioServer myserver = new NioServer();
		try {
			myserver.startServer("localhost", 20000);
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	public void startServer(String serverIp, int serverPort) throws IOException {
		ServerSocketChannel serviceChannel = ServerSocketChannel.open();
		InetSocketAddress localAddr = new InetSocketAddress(serverIp, serverPort);
		serviceChannel.bind(localAddr);
		serviceChannel.configureBlocking(false);
		Selector selector = Selector.open();
		serviceChannel.register(selector, SelectionKey.OP_ACCEPT);
		while (true) {
			selector.select();
			Iterator<SelectionKey> keys = selector.selectedKeys().iterator();
			while (keys.hasNext()) {
				SelectionKey key = keys.next();
				keys.remove();
				try {
					if (key.isAcceptable()) {
						ServerSocketChannel server = (ServerSocketChannel) key.channel();
						SocketChannel channel = server.accept();
						channel.configureBlocking(false);
						SelectionKey clientKey = channel.register(selector, SelectionKey.OP_READ);
						clientKey.attach(new KeyData());
					} else if (key.isReadable()) {
						SocketChannel channel = (SocketChannel) key.channel();
						ByteBuffer buffer = ByteBuffer.allocate(1024);
						channel.read(buffer);
						buffer.flip();

						KeyData keyData = (KeyData) key.attachment();
						keyData.getDataReceiver().receiveData(buffer);

						Msgs.Msg.Builder mb = Msgs.Msg.newBuilder();
						mb.setType(Msgs.Type.LOGIN_RESULT);
						mb.getLoginResultBuilder().setSuccess(true);
						keyData.setMsgBody(mb.build().toByteArray());

						key.interestOps(SelectionKey.OP_WRITE);
					} else if (key.isWritable()) {
						KeyData keyData = (KeyData) key.attachment();
						byte[] msgBody = keyData.getMsgBody();
						if (msgBody != null) {
							SocketChannel channel = (SocketChannel) key.channel();
							DataReceiver.writeMsg(channel, msgBody);
							keyData.setMsgBody(null);
						}
						key.interestOps(SelectionKey.OP_READ);
					}
				} catch (IOException e) {
					key.cancel();
					key.channel().close();
				}
			}
		}
	}

}
