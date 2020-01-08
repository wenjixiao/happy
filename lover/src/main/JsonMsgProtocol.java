package main;

import java.io.IOException;
import java.nio.channels.SocketChannel;
import java.nio.charset.Charset;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;

import mynet.AMsgProtocol;

public class JsonMsgProtocol extends AMsgProtocol {
	private Charset jsonCharset;

	public JsonMsgProtocol(SocketChannel channel) {
		super(channel);
		jsonCharset = Charset.forName("UTF-8");
	}

	@Override
	public void processMsg(byte[] data) {
		JSONObject json = decode(data);
		System.out.println(json);
	}

	public void writeMsg(JSONObject json) throws IOException {
		writeMsg(encode(json));
	}
	
	public byte[] encode(JSONObject obj) {
        String jsonStr = JSON.toJSONString(obj);
        return jsonStr.getBytes(jsonCharset);
	}

	public JSONObject decode(byte[] data) {
        String jsonStr = new String(data,jsonCharset);
        return JSON.parseObject(jsonStr);
	}
	
  }
