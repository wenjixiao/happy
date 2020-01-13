package snowing.commands;

import java.io.IOException;

import snowing.protocols.JsonServerMsgProtocol;

public interface IRunOnServer {
	public void run(JsonServerMsgProtocol protocol) throws IOException;
}
