package main

import (
    "github.com/golang/protobuf/proto"
    "log"
    "mynet"
    "net"
    "pb"
)

func EncodeMsg(msg *pb.Msg) []byte {
    data, err := proto.Marshal(msg)
    if err != nil {
        log.Fatalf("protobuf marshal: %v\n", err)
    }
    return data
}

func DecodeMsg(msgBytes []byte) *pb.Msg {
    msg := &pb.Msg{}
    err := proto.Unmarshal(msgBytes, msg)
    if err != nil {
        log.Fatalf("protobuf unmarshal: %v", err)
    }
    return msg
}

func SendMsg(conn net.Conn, msg *pb.Msg) {
    data := EncodeMsg(msg)
    _, err := conn.Write(mynet.AddHeader(data))
    if err != nil {
        log.Fatalf("conn write: %v\n", err)
    }
}

//---------------------------------------------------------
