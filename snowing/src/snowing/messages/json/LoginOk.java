package snowing.messages.json;

import snowing.domain.Player;

public class LoginOk {
	private Player player;

	public Player getPlayer() {
		return player;
	}

	public void setPlayer(Player player) {
		this.player = player;
	}

	@Override
	public String toString() {
		return "LoginOk [player=" + player + "]";
	}
	
}
