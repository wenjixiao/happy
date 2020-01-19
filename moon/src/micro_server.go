package main

import (
    "panda"
    "mynet"
)

func main() {
    mynet.ListenForever(":20000", &panda.ProtocolFactory{})
}