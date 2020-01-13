package snowing.commands;

import snowing.protocols.JsonClientMsgProtocol;

public interface IRunOnClient {
	public void run(JsonClientMsgProtocol protocol);
}
