package mynet;

import java.io.IOException;
import java.nio.channels.SocketChannel;

import com.alibaba.fastjson.JSONObject;

import mynet.encode.IMsgEncoder;

public class JsonMsgProtocol extends AMsgProtocol {
	
	private IMsgEncoder encoder;

	public JsonMsgProtocol(SocketChannel channel,IMsgEncoder encoder) {
		super(channel);
		this.encoder = encoder;
	}

	@Override
	public void processMsg(byte[] data) {
		JSONObject json = (JSONObject)encoder.decode(data);
		System.out.println(json);
	}

	public void writeMsg(JSONObject json) throws IOException {
		writeMsg(encoder.encode(json));
	}

	
  }
