package snowing.protocols;

import java.io.IOException;
import java.nio.channels.SocketChannel;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

import snowing.Client;
import snowing.messages.json.LoginFail;
import snowing.messages.json.LoginOk;
import snowing.messages.json.Message;

public class JsonClientMsgProtocol extends AMsgProtocol {
	private Client client;

	public JsonClientMsgProtocol(Client client,SocketChannel channel) {
		super(channel);
		this.client = client;
	}
	
	public Client getClient() {
		return client;
	}

	@Override
	public void processMsg(byte[] data) throws IOException {
		ObjectMapper mapper = getClient().getMapper();
		JsonNode rootNode = mapper.readTree(data);
		System.out.printf("client get msg: %s\n", rootNode);
		Message.Type type = mapper.treeToValue(rootNode.get("type"), Message.Type.class);
		JsonNode msgNode = rootNode.get("msg");

		switch (type) {
		case LoginOk:
			LoginOk loginOk = mapper.treeToValue(msgNode, LoginOk.class);
			System.out.println(loginOk);
			break;
		case LoginFail:
			LoginFail loginFail = mapper.treeToValue(msgNode, LoginFail.class);
			System.out.println(loginFail);
			break;
		default:
			System.out.println("hoho");
		}
	}

}
