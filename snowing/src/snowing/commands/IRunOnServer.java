package snowing.commands;

import java.io.IOException;

import snowing.ServerMsgProtocol;

public interface IRunOnServer {
	public void run(ServerMsgProtocol protocol) throws IOException;
}
