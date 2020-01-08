package mynet.encode;

import java.nio.charset.Charset;

import com.alibaba.fastjson.JSON;

public class JsonMsgEncoder implements IMsgEncoder {
	private Charset jsonCharset;
	
	public JsonMsgEncoder() {
		jsonCharset = Charset.forName("UTF-8");
	}
	
	@Override
	public byte[] encode(Object obj) {
        String jsonStr = JSON.toJSONString(obj);
        return jsonStr.getBytes(jsonCharset);
	}

	@Override
	public Object decode(byte[] data) {
        String jsonStr = new String(data,jsonCharset);
        return JSON.parseObject(jsonStr);
	}

}
