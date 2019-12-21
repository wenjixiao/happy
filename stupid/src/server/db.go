package main

import (
	"pb"
	// "fmt"
	"os"
	"encoding/json"
	"log"
)

const FILE_NAME = "players.json"

func WriteFile(players []pb.Player) {
	file,err := os.Create(FILE_NAME)
	if err != nil {
		log.Fatalf("open file: %v",err)
	}
	defer file.Close()
	encoder := json.NewEncoder(file)
	err = encoder.Encode(players)
	if err != nil {
		log.Fatalf("encode: %v",err)
	}
}

func ReadFile() (players []pb.Player) {
	file,err := os.Open(FILE_NAME)
	if err != nil {
		log.Fatalf("read file: %v",err)
	}
	defer file.Close()

	decoder := json.NewDecoder(file)

	err = decoder.Decode(&players)
	if err != nil {
		log.Fatalf("decode: %v",err)
	}

	return
}

func GetPlayer(pid string,passwd string) (*pb.Player,bool) {
	for _,p := range ReadFile() {
		if p.Pid == pid && p.Passwd == passwd {
			return &p,true
		}
	}
	return nil,false
}

/*func main(){
	p1 := pb.Player{
		Pid: "wen",
		Level: "3d",
		Passwd: "123",
	}
	p2 := pb.Player{
		Pid: "zhong",
		Level: "18k",
		Passwd: "456",
	}
	players := []pb.Player{p1,p2}
	// fmt.Println(players)
	WriteFile(players)
	
	if p,ok := GetPlayer("wen","123"); ok {
		fmt.Println(p)
	}
}*/