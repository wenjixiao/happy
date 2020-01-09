

import java.io.IOException;
import java.nio.channels.SocketChannel;
import pb.Msgs;

import com.google.protobuf.InvalidProtocolBufferException;

public class ServerMsgProtocol extends AMsgProtocol {

    public ServerMsgProtocol(SocketChannel channel) {
        super(channel);
    }

    @Override
    public void processMsg(byte[] data) {
        try {
            Msgs.Msg msg = Msgs.Msg.parseFrom(data);
            System.out.printf("server got msg: %s",msg);

            Msgs.LoginResult.Builder lr = Msgs.LoginResult.newBuilder();
            lr.setSuccess(true);
            Msgs.Msg.Builder mb = Msgs.Msg.newBuilder();
            mb.setType(Msgs.Type.LOGIN_RESULT);
            mb.setLoginResult(lr);

            writeMsg(mb.build());

        } catch (InvalidProtocolBufferException e) {
            e.printStackTrace();
        } catch (IOException e){
            e.printStackTrace();
        }
    }
    
    public void writeMsg(Msgs.Msg msg) throws IOException {
        writeMsg(msg.toByteArray());
    }
    
}
