package main;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.nio.channels.SocketChannel;

import com.alibaba.fastjson.JSONObject;

public class Main {
	private JsonMsgProtocol protocol;

	public void connect() throws IOException {
		SocketChannel channel = null;
		try {
			SocketAddress addr = new InetSocketAddress("localhost", 20000);
			channel = SocketChannel.open(addr);
			protocol = new JsonMsgProtocol(channel);

			Thread t = new Thread(new Runnable() {

				@Override
				public void run() {
					try {
						protocol.readMsg();
					} catch (IOException e) {
//						System.out.printf("channel closed: %s", e);
						e.printStackTrace();
					}
				}

			});

//			t.setDaemon(true);
			t.start();

			test();

			Thread.sleep(1000);
		} catch (IOException e) {
			e.printStackTrace();
		} catch (InterruptedException e) {
			e.printStackTrace();
		} finally {
			if (channel != null) {
				channel.close();
			}
		}
	}

	public void test() throws IOException {
		JSONObject json = new JSONObject();
		json.put("type", "login");
		json.put("pid", "wen");
		json.put("passwd", "123");
		protocol.writeMsg(json);
	}

	public static void main(String[] args) throws IOException {
		new Main().connect();
	}
}