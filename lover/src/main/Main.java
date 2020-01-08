package main;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.nio.channels.SocketChannel;

import com.alibaba.fastjson.JSONObject;

import mynet.AMsgProtocol;
import mynet.JsonMsgProtocol;
import mynet.encode.IMsgEncoder;
import mynet.encode.JsonMsgEncoder;

public class Main{
    private JsonMsgProtocol protocol;

    public Main(){
        try{
            SocketAddress addr = new InetSocketAddress("localhost",20000);
            IMsgEncoder encoder = new JsonMsgEncoder();
            protocol = new JsonMsgProtocol(SocketChannel.open(addr),encoder);
            
            Thread t = new Thread(new Runnable() {

				@Override
				public void run() {
					try {
						protocol.readMsg();
					} catch (IOException e) {
						e.printStackTrace();
					}
				}
            	
            });
            
            t.setDaemon(true);
            t.start();
 
            test();
            
            Thread.sleep(2000);
        }catch (IOException e){
            e.printStackTrace();
        } catch (InterruptedException e) {
			e.printStackTrace();
		}
    }

    public void test() throws IOException {
        JSONObject json = new JSONObject();
        json.put("type","login");
        json.put("pid","wen");
        json.put("passwd","123");
        protocol.writeMsg(json);
    }

    public static void main(String[] args){
        new Main();
    }
}