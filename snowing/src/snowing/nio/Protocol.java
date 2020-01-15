package snowing.nio;

import java.io.IOException;
import java.nio.channels.SelectionKey;
import java.nio.channels.SocketChannel;

import com.google.protobuf.InvalidProtocolBufferException;

import snowing.domain.Player;
import snowing.messages.pb.Msgs;
import snowing.messages.pb.Msgs.Login;

public class Protocol extends DataReceiver {
	private SelectionKey key;
	private NioServer server;
	
	private Player player;

	public Player getPlayer() {
		return player;
	}

	public void setPlayer(Player player) {
		this.player = player;
	}

	public Protocol(NioServer nioServer, SelectionKey key) {
		server = nioServer;
		this.key = key;
	}

	@Override
	public void processMsg(byte[] data) throws IOException {
		try {
			Msgs.Msg msg = Msgs.Msg.parseFrom(data);
			switch(msg.getType()) {
			case LOGIN:
				Login login = msg.getLogin();
				Player p = new Player();
				p.setPid(login.getPid());
				p.setPassword(login.getPassword());
				p.setLevel("3d");
				
				Msgs.Msg.Builder mb = Msgs.Msg.newBuilder();
				mb.setType(Msgs.Type.LOGIN_RESULT);
				mb.getLoginResultBuilder().setSuccess(true);
				DataReceiver.writeMsg((SocketChannel) key.channel(), mb.build().toByteArray());
				
			default:
				break;
			}
			System.out.println("----server----");
			System.out.println(msg);
		} catch (InvalidProtocolBufferException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
	}


}
