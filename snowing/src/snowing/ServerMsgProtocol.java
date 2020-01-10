package snowing;

import java.io.IOException;
import java.nio.channels.SocketChannel;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

import snowing.domain.Player;
import snowing.messages.Login;
import snowing.messages.LoginFail;
import snowing.messages.LoginOk;
import snowing.messages.Message;

public class ServerMsgProtocol extends AMsgProtocol {
	private Server server;
	private Player player;

	public Server getServer() {
		return server;
	}

	public Player getPlayer() {
		return player;
	}

	public void setPlayer(Player player) {
		this.player = player;
	}

	public ServerMsgProtocol(Server server, SocketChannel channel) {
		super(channel);
		this.server = server;
	}

	@Override
	public void processMsg(byte[] data) throws IOException, ClassNotFoundException {
		ObjectMapper mapper = getServer().getMapper();
		
		JsonNode rootNode = mapper.readTree(data);
		System.out.printf("server got message: %s\n", rootNode);
		
		Message.Type type = mapper.treeToValue(rootNode.get("type"), Message.Type.class);
		JsonNode msgNode = rootNode.get("msg");

		switch (type) {
		case Login:
			Login login = mapper.treeToValue(msgNode, Login.class);
			Player p = server.getPlayer(login.getPid(), login.getPassword());
			if (p != null) {
				setPlayer(p);
				server.addProtocol(this);
				
				LoginOk loginOk = new LoginOk();
				loginOk.setPlayer(p);

				Message<LoginOk> message = new Message<LoginOk>();
				message.setType(Message.Type.LoginOk);
				message.setMsg(loginOk);

				writeMsg(mapper.writeValueAsBytes(message));
			} else {
				LoginFail loginFail = new LoginFail();
				
				Message<LoginFail> message = new Message<LoginFail>();
				message.setType(Message.Type.LoginFail);
				message.setMsg(loginFail);
				
				writeMsg(mapper.writeValueAsBytes(message));
			}
			break;
		default:
			System.out.println("not that type!");
		}

	}

}
