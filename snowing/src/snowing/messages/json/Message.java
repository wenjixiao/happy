package snowing.messages.json;

public class Message<T> {
	public enum Type{ Login,LoginOk,LoginFail }
	
	private Type type;
	private T msg;
	
	public Type getType() {
		return type;
	}

	public void setType(Type type) {
		this.type = type;
	}

	public T getMsg() {
		return msg;
	}

	public void setMsg(T msg) {
		this.msg = msg;
	}

}
