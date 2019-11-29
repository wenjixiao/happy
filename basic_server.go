import (
	"binary"
	"bytes"
	"fmt"
	"log"
	"net"
)

const HeaderSize = 4

type Msg {}

func Marshal() []byte {}

func Unmarshal(bin []byte) Msg {}

func Listen(){
	listener, err := net.Listen("tcp", ":5678")
	defer listener.Close()
	if err != nil {
		log.Fatal(err)
	}
	for {
		conn, err := listener.Accept()
		if err != nil {
			log.Fatal(err)
		}
		go HandleConn(conn)
	}
}

func HandleConn(conn net.Conn) {
	defer conn.Close()

	const MSG_BUF_LEN = 1024 * 10 //10KB 
	const READ_BUF_LEN = 1024       //1KB

	// log.Printf("Client: %s\n", conn.RemoteAddr())

	msgBuf := bytes.NewBuffer(make([]byte, 0, MSG_BUF_LEN))
	readBuf := make([]byte, READ_BUF_LEN)

	head := uint32(0)
	bodyLen := 0 //bodyLen is a flag,when readed head,but body'len is not enougth

	for {
		n, err := conn.Read(readBuf)
		if err != nil {
			if err == io.EOF {
				log.Printf("---connection lost normal---")
			} else {
				log.Fatalf("Read error: %s\n", err)
			}
			break
		}

		_, err = msgBuf.Write(readBuf[:n])

		if err != nil {
			log.Fatalf("Buffer write error: %s\n", err)
		}

		for {
			//read the msg head
			if bodyLen == 0 && msgBuf.Len() >= HeaderSize {
				err := binary.Read(msgBuf, binary.LittleEndian, &head)
				if err != nil {
					log.Printf("msg head Decode error: %s\n", err)
				}
				bodyLen = int(head)

				if bodyLen > MSG_BUF_LEN {
					log.Fatalf("msg body too long: %d\n", bodyLen)
				}
			}
			//has head,now read body
			if bodyLen > 0 && msgBuf.Len() >= bodyLen {
				ProcessMsg(Unmarshal(msgBuf.Next(bodyLen)))
				bodyLen = 0
			} else {
				//msgBuf.Len() < bodyLen ,one msg receiving is not complete
				//need to receive again
				break
			}
		}
	}
}

func ProcessMsg(msg Msg) {

}

func AddHeader(msgBytes []byte) []byte {
	head := make([]byte, HeaderSize)
	binary.LittleEndian.PutUint32(head, uint32(len(msgBytes)))
	return append(head, msgBytes...)
}

func SendMsg(conn net.Conn,msg Msg) {
	_,err = conn.Write(AddHeader(Marshal(msg)))
	if err != nil {
		log.Fatalf("write error: %s\n",err)
	}
}