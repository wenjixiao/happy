import java.net.Socket;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.nio.channels.SocketChannel;
import com.alibaba.fastjson.JSONObject;
import java.util.concurrent.locks.*;

public class Main{
    public void connect(){
        try{
            SocketAddress addr = new InetSocketAddress("localhost",20000);
            SocketChannel channel = SocketChannel.open(addr);
            SockReading sr = new SockReading(channel);
            Thread t = new Thread(sr);
            t.setDaemon(true);
            t.start();
            t.join();
            System.out.println("----main thread exit----");
        } catch (Exception e){
            e.printStackTrace();
        }
    }

    public static void main(String[] args){
        Main m = new Main();
        m.connect();
    }
}