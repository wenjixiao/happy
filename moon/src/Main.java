import java.net.Socket;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.io.IOException;
import java.nio.channels.SocketChannel;
import com.alibaba.fastjson.JSONObject;
import java.util.concurrent.locks.*;

public class Main{
    private SockReading sr;

    public Main(){
        try{
            SocketAddress addr = new InetSocketAddress("localhost",20000);
            sr = new SockReading(SocketChannel.open(addr));

            Thread t = new Thread(sr);
            t.setDaemon(true);
            t.start();

            test();
            
            Thread.sleep(2000);

        }catch (IOException e){
            e.printStackTrace();
        }catch (InterruptedException e){
            e.printStackTrace();
        }
    }

    public void test() throws IOException {
        JSONObject json = new JSONObject();
        json.put("type","login");
        json.put("pid","wen");
        json.put("passwd","123");
        sr.sendMsg(json);
    }

    public static void main(String[] args){
        new Main();
    }
}