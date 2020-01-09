package snowing.commands;

import snowing.ClientMsgProtocol;

public class LoginResultCommand extends ACommand implements IRunOnClient {

	private static final long serialVersionUID = 1L;

	private boolean success;

	public boolean isSuccess() {
		return success;
	}

	public void setSuccess(boolean success) {
		this.success = success;
	}

	@Override
	public void run(ClientMsgProtocol protocol) {
		System.out.printf("login result: %s",success);
	}

}
