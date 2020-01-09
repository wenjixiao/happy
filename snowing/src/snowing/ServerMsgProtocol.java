package snowing;

import java.io.IOException;
import java.nio.channels.SocketChannel;

import snowing.commands.ACommand;
import snowing.commands.IRunOnServer;
import snowing.domain.Player;

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
		System.out.println("----server protocol process msg----");
		IRunOnServer rs = (IRunOnServer) ACommand.decode(data);
		rs.run(this);
	}

}
