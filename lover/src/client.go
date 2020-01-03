package main

import (
	"bufio"
	"log"
	"mynet"
	"net"
	"os"
	"pb"
)

var done = make(chan struct{})

type ClientMsgProtocol struct {
	Player *pb.Player
	Conn   net.Conn
}

func (protocol *ClientMsgProtocol) ConnectionMade(conn net.Conn) {
	protocol.Conn = conn
}

func (protocol *ClientMsgProtocol) MsgReceived(msg *pb.Msg) {
	log.Printf("protocol received: %v", msg)
}

func (protocol *ClientMsgProtocol) ConnectionLost(err error) {
	log.Printf("connection lost: %v", err)
	done <- struct{}{}
}

func CmdLoop(protocol *ClientMsgProtocol) {
	input := bufio.NewScanner(os.Stdin)
	for input.Scan() {
		cmd := input.Text()
		if cmd == "login" {
			msg := &pb.Msg{Type: pb.Type_LOGIN, Union: &pb.Msg_Login{&pb.Login{Pid: "wen", Passwd: "123"}}}
			mynet.SendMsg(protocol.Conn, msg)
		} else if cmd == "logout" {
			msg := &pb.Msg{Type: pb.Type_LOGOUT, Union: &pb.Msg_Logout{&pb.Logout{}}}
			mynet.SendMsg(protocol.Conn, msg)
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

	protocol := &ClientMsgProtocol{}
	go mynet.HandleConn(conn, protocol)

	CmdLoop(protocol)

	<-done

	log.Println("-----main exit----")
}
