package snowing;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.channels.ServerSocketChannel;
import java.nio.channels.SocketChannel;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import snowing.domain.Player;

public class Server {
	private List<Player> players;
	private List<ServerMsgProtocol> protocols;

	public Server() {
		players = new ArrayList<Player>();
		protocols = new ArrayList<ServerMsgProtocol>();
		initPlayers();
	}

	public void addProtocol(ServerMsgProtocol protocol) {
		protocols.removeIf(p->p.getPlayer().equals(protocol.getPlayer()));
		protocols.add(protocol);
	}

	public void initPlayers() {
		Player p1 = new Player();
		p1.setPid("wen");
		p1.setPassword("123");
		p1.setLevel("3d");

		Player p2 = new Player();
		p2.setPid("zhong");
		p2.setPassword("456");
		p2.setLevel("18k");

		players.add(p1);
		players.add(p2);
	}

	public Player getPlayer(String pid, String password) {
		Player result = null;
		Iterator<Player> iterator = players.iterator();
		while (iterator.hasNext()) {
			Player p = iterator.next();
			if (p.getPid().equals(pid) && p.getPassword().equals(password)) {
				result = p;
				break;
			}
		}
		return result;
	}

	public void listen() throws IOException {
		ServerSocketChannel serverChannel = ServerSocketChannel.open();
		serverChannel.socket().bind(new InetSocketAddress(20000));
		while (true) {
			SocketChannel channel = serverChannel.accept();
			ServerMsgProtocol protocol = new ServerMsgProtocol(this, channel);
			new Thread(new Runnable() {

				@Override
				public void run() {
					try {
						protocol.readMsg();
					} catch (IOException e) {
						System.out.printf("thread exit: %s\n", e.getMessage());
					} catch (ClassNotFoundException e) {
						System.out.printf("thread exit: %s\n", e.getMessage());
					}
				}

			}).start();
		}
	}

	public static void main(String[] args) throws IOException {
		new Server().listen();
	}
}
