package jsonencoder

import (
    "github.com/buger/jsonparser"
    "encoding/json"
    "log"
    "net"
    "mynet"
)


type JsonMsg struct {
    MsgType string
    Msg interface{}
}

type Login struct {
    pid string
    password string
}

func EncodeMsg(msg *JsonMsg) []byte {
    data,err := json.Marshal(msg)
    if err != nil {
        log.Fatalf("json marshal: %v",err)
    }
    return data
}

func DecodeMsg(msgBytes []byte) *JsonMsg {
    msgTypeBin,_,_,err1 := jsonparser.Get(msgBytes, "MsgType")
    if err1 != nil {
        log.Fatalf("json parse msgType: %v",err1)
    }
    msgBin,_,_,err2 := jsonparser.Get(msgBytes, "Msg")
    if err2 != nil {
        log.Fatalf("json parse msg: %v",err2)
    }

    var msgType string
    err := json.Unmarshal(msgTypeBin,&msgType)
    if err != nil {
        log.Fatalf("json unmarshal: %v",err)
    }

    var msg interface{}

    switch(msgType){
    case "Login":
        login := Login{}
        err = json.Unmarshal(msgBin,&login)
        if err != nil {
            log.Fatalf("json unmarshal: %v",err)
        }
        msg = login
    }

    return &JsonMsg{msgType,msg}
}

func SendMsg(conn net.Conn, msg *JsonMsg) {
    data := EncodeMsg(msg)
    _, err := conn.Write(mynet.AddHeader(data))
    if err != nil {
        log.Fatalf("conn write: %v\n", err)
    }
}