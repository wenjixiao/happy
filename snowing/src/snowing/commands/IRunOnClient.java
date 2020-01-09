package snowing.commands;

import snowing.ClientMsgProtocol;

public interface IRunOnClient {
	public void run(ClientMsgProtocol protocol);
}
