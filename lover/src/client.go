package main

import (
	"bufio"
	"log"
	"mynet"
	"mynet/encoder"
	"net"
	"os"
	"pb"
	"strings"
)

var done = make(chan struct{})

type ClientProtocol struct {
	mynet.MsgProtocol
	Player *pb.Player
	Conn   net.Conn
}

func (protocol *ClientProtocol) ConnectionMade(conn net.Conn) {
	protocol.Conn = conn
}

func (protocol *ClientProtocol) ProcessMsg(msgBytes []byte) {
	msg := encoder.DecodeMsg(msgBytes)
	log.Printf("protocol received: %v", msg)
	switch msg.GetType() {
	case pb.Type_LOGIN_RESULT:
		loginResult := msg.GetLoginResult()
		log.Println(loginResult)
	}
}

func (protocol *ClientProtocol) ConnectionLost(err error) {
	log.Printf("connection lost: %v", err)
	done <- struct{}{}
}

func CmdLoop(protocol *ClientProtocol) {
	input := bufio.NewScanner(os.Stdin)
	for input.Scan() {
		line := input.Text()
		if line == "" {
			continue
		}

		cmdArray := strings.Split(line, " ")
		cmd, params := cmdArray[0], cmdArray[1:]

		if cmd == "login" {
			pid, password := params[0], params[1]
			msg := &pb.Msg{Type: pb.Type_LOGIN, Union: &pb.Msg_Login{&pb.Login{Pid: pid, Passwd: password}}}
			encoder.SendMsg(protocol.Conn, msg)
		} else if cmd == "logout" {
			msg := &pb.Msg{Type: pb.Type_LOGOUT, Union: &pb.Msg_Logout{&pb.Logout{}}}
			encoder.SendMsg(protocol.Conn, msg)
		} else if cmd == "exit" {
			//我关闭conn的时候，reading goroutine自然会退出，因为它在一直读那个conn
			protocol.Conn.Close()
			break
		} else {
			log.Println("*No* that cmd!")
		}
	}
	log.Println("cmd loop exited!")
}

func main() {
	addr := "localhost:20000"
	conn, err := net.Dial("tcp", addr)
	defer conn.Close()
	if err != nil {
		log.Fatalf("connect error: %v", err)
	}

	protocol := &ClientProtocol{Conn: conn}
	protocol.Receiver = protocol

	go mynet.HandleConn(conn, protocol)

	CmdLoop(protocol)

	<-done

	log.Println("-----main exit----")
}
