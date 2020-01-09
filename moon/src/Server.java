import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.channels.ServerSocketChannel;
import java.nio.channels.SocketChannel;

public class Server {
    public void listen() throws IOException {
        ServerSocketChannel ssc = ServerSocketChannel.open();
        ssc.socket().bind(new InetSocketAddress(20000));
        while(true) {
            SocketChannel channel = ssc.accept();
            new Thread(new Runnable() {

                @Override
                public void run() {
                    try{
                        ServerMsgProtocol protocol = new ServerMsgProtocol(channel);
                        protocol.readMsg();
                    }catch (IOException e){
                        e.printStackTrace();
                    }
                }
                
            }).start();
        }
    }
    public static void main(String[] args) throws IOException {
        new Server().listen();
    }
}