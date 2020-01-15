package snowing.nio;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.channels.ServerSocketChannel;
import java.nio.channels.SocketChannel;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class NioServer {

	public static void main(String[] args) {
		NioServer myserver = new NioServer();
		try {
			myserver.startServer("localhost", 20000);
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	public NioServer() {
		clientKeys = new ArrayList<SelectionKey>();
	}

	private List<SelectionKey> clientKeys;

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
						Protocol protocol = new Protocol(this,clientKey);
						clientKey.attach(protocol);
						
						clientKeys.add(clientKey);
					} else if (key.isReadable()) {
						SocketChannel channel = (SocketChannel) key.channel();
						ByteBuffer buffer = ByteBuffer.allocate(1024);
						channel.read(buffer);
						buffer.flip();

						Protocol protocol = (Protocol) key.attachment();
						protocol.receiveData(buffer);
					}
				} catch (IOException e) {
					e.printStackTrace();
					key.cancel();
					key.channel().close();
					clientKeys.remove(key);
					System.out.println("****exit****");
				}
			}
		}
	}

}
