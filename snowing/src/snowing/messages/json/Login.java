package snowing.messages.json;

public class Login {
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
	public String toString() {
		return "Login [pid=" + pid + "]";
	}

}
