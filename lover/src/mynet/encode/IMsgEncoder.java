package mynet.encode;

public interface IMsgEncoder {
    public byte[] encode(Object obj);
    public Object decode(byte[] data);
}
