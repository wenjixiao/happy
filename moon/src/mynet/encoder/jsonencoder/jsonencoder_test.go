package jsonencoder

import (
    "testing"
    "github.com/buger/jsonparser"
    "encoding/json"
    "fmt"
)

type MyMessage struct {
    MsgType int
    Msg interface{}
}

type Person struct {
    Name string
    Age int
}

func TestJsonParser(t *testing.T) {
    p1 := Person{"wen",20}
    m1 := MyMessage{1,p1}

    data,err := json.Marshal(m1)
    if err != nil {
        fmt.Println("json marshal: ",err)
    }
    fmt.Printf("str: %v\n",string(data))
    fmt.Printf("data: %v\n",data)
    fmt.Printf("data length: %v\n",len(data))


    msg,dataType,offset,err := jsonparser.Get(data, "Msg")
    fmt.Printf("first byte: %v\n",string(data[offset]))
    if err != nil {
        fmt.Printf("json parse: %v",err)
    }
    fmt.Printf("msg: %v,datatype: %v,offset: %v\n",msg,dataType,offset)

    p := Person{}
    err = json.Unmarshal(msg,&p)
    if err != nil {
        fmt.Printf("json unmarshal: %v",err)
    }
    fmt.Printf("person: %v",p)
}