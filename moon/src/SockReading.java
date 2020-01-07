import java.net.Socket;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.channels.SocketChannel;
import java.io.IOException;
import java.nio.charset.Charset;
import com.alibaba.fastjson.JSONObject;
import com.alibaba.fastjson.JSON;

public class SockReading
{
    final int HEADER_LEN = 4;

    private Socket sock;
    private Charset jsonCharset;

    public SockReading(Socket sock){
        this.sock = sock;
        jsonCharset = Charset.forName("UTF-8");
    }

    /**
     * keep reading binary msg forever
     */
    public void readMsg() throws IOException {
        final int BUF_LEN = 1024*6;
        ByteBuffer buf = ByteBuffer.allocate(BUF_LEN);
        SocketChannel channel = sock.getChannel();
        int bodyLen = 0;
        while(channel.read(buf) != -1){
            buf.flip();
            while(buf.hasRemaining()){
                int n = buf.remaining();
                if(bodyLen == 0) {
                    if(n >= HEADER_LEN){
                        byte[] header = new byte[HEADER_LEN];
                        buf = buf.get(header);
                        ByteBuffer headBuf = ByteBuffer.wrap(header).order(ByteOrder.LITTLE_ENDIAN);
                        bodyLen = headBuf.getInt();
                    }else{
                        break;
                    }
                }

                if(bodyLen > 0){
                    if(buf.remaining() >= bodyLen){
                        byte[] body = new byte[bodyLen];
                        buf = buf.get(body);
                        processMsg(decodeJSON(body));
                        bodyLen = 0;
                    }else{
                        break;
                    }
                }
            }
            buf.compact();
        }
    }
    
    public void sendMsg(JSONObject jsonObj) throws IOException {
        byte[] msgBody = encodeJSON(jsonObj);
        int bodyLen = msgBody.length;
        ByteBuffer buf = ByteBuffer.allocate(HEADER_LEN+bodyLen);

        ByteBuffer headBuf = ByteBuffer.allocate(HEADER_LEN).order(ByteOrder.LITTLE_ENDIAN);
        headBuf.putInt(bodyLen);

        headBuf.flip();
        buf.put(headBuf);
        buf.put(msgBody);

        buf.flip();
        sock.getChannel().write(buf);
    }
    
    public void processMsg(JSONObject jsonObj){
        System.out.println(jsonObj);
    }

    public byte[] encodeJSON(JSONObject jsonObj){
        String jsonStr = JSON.toJSONString(jsonObj);
        return jsonStr.getBytes(jsonCharset);
    }
    
    public JSONObject decodeJSON(byte[] data){
        String jsonStr = new String(data,jsonCharset);
        return JSON.parseObject(jsonStr);
    }    

}