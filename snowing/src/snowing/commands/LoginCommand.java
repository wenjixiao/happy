package snowing.commands;

import java.io.IOException;

import snowing.Server;
import snowing.domain.Player;
import snowing.protocols.JsonServerMsgProtocol;

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
	public void run(JsonServerMsgProtocol protocol) throws IOException {
		Server server = protocol.getServer();
		Player p = server.getPlayer(pid, password);
		System.out.printf("player login command: %s\n",p);
		if(p != null) {
			protocol.setPlayer(p);
			server.addProtocol(protocol);
			LoginResultCommand lrc = new LoginResultCommand();
			lrc.setSuccess(true);
			protocol.writeMsg(lrc.encode());
		}
	}

}
