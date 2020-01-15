package snowing.nio;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.SocketChannel;

import snowing.messages.pb.Msgs;

public class NioClient {
	public static void main(String[] args) {
		NioClient client = new NioClient();
		client.connect("localhost", 20000);
	}

	public void connect(String serverIp, int serverPort) {
		try {
			SocketChannel channel = SocketChannel.open(new InetSocketAddress(serverIp, serverPort));
			Msgs.Msg.Builder mb = Msgs.Msg.newBuilder();
			mb.setType(Msgs.Type.LOGIN);
			mb.getLoginBuilder().setPid("wen");
			mb.getLoginBuilder().setPassword("123");
			
			DataReceiver.writeMsg(channel,mb.build().toByteArray());

			ByteBuffer buffer = ByteBuffer.allocate(1024);
			channel.read(buffer);
			buffer.flip();
			
			ClientDataReceiver dataReceiver = new ClientDataReceiver();
			dataReceiver.receiveData(buffer);
			
			channel.close();
			Thread.sleep(6000);
			System.out.println("----client exit----");
		} catch (IOException e) {
			e.printStackTrace();
		} catch (InterruptedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

	}
}
