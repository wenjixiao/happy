package snowing;

import java.io.IOException;
import java.nio.channels.SocketChannel;

import snowing.commands.ACommand;
import snowing.commands.IRunOnClient;

public class ClientMsgProtocol extends AMsgProtocol {

	public ClientMsgProtocol(SocketChannel channel) {
		super(channel);
	}

	@Override
	public void processMsg(byte[] data) throws IOException, ClassNotFoundException {
		IRunOnClient cmd = (IRunOnClient) ACommand.decode(data);
		cmd.run(this);
	}

}
