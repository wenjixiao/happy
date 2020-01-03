package mynet

import (
	"bytes"
	"encoding/binary"
	"github.com/golang/protobuf/proto"
	"io"
	"log"
	"net"
	"pb"
)

const HeaderSize = 4

type MsgProtocol interface {
	ConnectionMade(conn net.Conn)
	MsgReceived(msg *pb.Msg)
	ConnectionLost(err error)
}

func HandleConn(conn net.Conn, protocol MsgProtocol) {
	defer conn.Close()

	protocol.ConnectionMade(conn)

	const MsgBufLen = 1024 * 10 //10KB
	const ReadBufLen = 1024     //1KB

	log.Printf("client: %v\n", conn.RemoteAddr())

	msgBuf := bytes.NewBuffer(make([]byte, 0, MsgBufLen))
	readBuf := make([]byte, ReadBufLen)

	head := uint32(0)
	bodyLen := 0 //bodyLen is a flag,when readed head,but body'len is not enougth

	for {
		n, err := conn.Read(readBuf)
		if err != nil {
			if err == io.EOF {
				log.Println("conn closed")
				//conn被关闭。正常退出！
				protocol.ConnectionLost(nil)
			} else {
				log.Printf("conn read: %v\n", err)
				//异常退出
				protocol.ConnectionLost(err)
			}
			break
		}

		_, err = msgBuf.Write(readBuf[:n])

		if err != nil {
			log.Fatalf("buf write: %v\n", err)
		}

		for {
			//read the pb head
			if bodyLen == 0 && msgBuf.Len() >= HeaderSize {
				err := binary.Read(msgBuf, binary.LittleEndian, &head)
				if err != nil {
					log.Printf("pb head decode: %v\n", err)
				}
				bodyLen = int(head)

				if bodyLen > MsgBufLen {
					log.Fatalf("pb body overflow: %d\n", bodyLen)
				}
			}
			//has head,now read body
			if bodyLen > 0 && msgBuf.Len() >= bodyLen {
				msg := &pb.Msg{}
				err := proto.Unmarshal(msgBuf.Next(bodyLen), msg)
				if err != nil {
					log.Fatalf("proto unmarshal: %v\n", err)
				}
				protocol.MsgReceived(msg)
				bodyLen = 0
			} else {
				//msgBuf.Len() < bodyLen ,one pb receiving is not complete
				//need to receive again
				break
			}
		}
	}
}

func AddHeader(msgBytes []byte) []byte {
	head := make([]byte, HeaderSize)
	binary.LittleEndian.PutUint32(head, uint32(len(msgBytes)))
	return append(head, msgBytes...)
}

func SendMsg(conn net.Conn, msg *pb.Msg) {
	data, err := proto.Marshal(msg)
	if err != nil {
		log.Fatalf("proto marshal: %v\n", err)
	}
	_, err = conn.Write(AddHeader(data))
	if err != nil {
		log.Fatalf("conn write: %v\n", err)
	}
}
