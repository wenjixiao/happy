package snowing.commands;

import java.io.IOException;

import snowing.Server;
import snowing.ServerMsgProtocol;
import snowing.domain.Player;

public class LoginCommand extends ACommand implements IRunOnServer {
	private static final long serialVersionUID = 1L;
	private String pid;
	private String password;

	public String getPid() {
		return pid;
	}

	public void setPid(String pid) {
		this.pid = pid;
	}

	public String getPassword() {
		return password;
	}

	public void setPassword(String password) {
		this.password = password;
	}

	@Override
	public void run(ServerMsgProtocol protocol) throws IOException {
		Server server = protocol.getServer();
		Player p = server.getPlayer(pid, password);
		System.out.printf("player login command: %s",p);
		if(p != null) {
			protocol.setPlayer(p);
			server.addProtocol(protocol);
			LoginResultCommand lrc = new LoginResultCommand();
			lrc.setSuccess(true);
			protocol.writeMsg(lrc.encode());
		}
	}

}
