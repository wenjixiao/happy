package snowing.domain;

public class Stone {
	public byte getX() {
		return x;
	}
	public void setX(byte x) {
		this.x = x;
	}
	public byte getY() {
		return y;
	}
	public void setY(byte y) {
		this.y = y;
	}
	public boolean isColor() {
		return color;
	}
	public void setColor(boolean color) {
		this.color = color;
	}
	public boolean isPass() {
		return pass;
	}
	public void setPass(boolean pass) {
		this.pass = pass;
	}
	private byte x;
	private byte y;
	private boolean color;
	private boolean pass;
}
