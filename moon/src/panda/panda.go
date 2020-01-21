package panda

import (
	"log"
	"mynet"
	"mynet/encoder/pbencoder"
	"net"
	"pb"
)


var MyMsgSender *MsgSender
var MyProtocolsManager *ProtocolsManager
var MyGamesManager *GamesManager

//---------------------------------------------------------

func init() {
	MyMsgSender = NewMsgSender()
	MyProtocolsManager = NewProtocolsManager()
	MyGamesManager = NewGamesManager()
}

//---------------------------------------------------------

type ServerProtocol struct {
	mynet.MsgProtocol
	Conn   net.Conn
	Player *pb.Player
}

func (sp *ServerProtocol) ConnectionMade(conn net.Conn) {
	log.Printf("connection made: %v", conn.RemoteAddr())
	sp.Conn = conn
	MyProtocolsManager.AddProtocol(sp)
}

func (sp *ServerProtocol) ConnectionLost(err error) {
	log.Printf("connection lost: %v", err)
	MyProtocolsManager.RemoveProtocol(sp)
	if sp.Player != nil {
		MyGamesManager.PlayerLinebroken(sp.Player)
	}
}

// override MsgProcessor
func (sp *ServerProtocol) ProcessMsg(msgBytes []byte) {
	msg := pbencoder.DecodeMsg(msgBytes)
	log.Printf("get msg: %v", msg)
	switch msg.GetType() {
	case pb.Msg_LoginType:
		player := &pb.Player{Pid: msg.GetLogin().GetPid()}
		MyProtocolsManager.ChangeProtocol(sp,player)

		// linebroken,now come back
		MyGamesManager.PlayerComeback(player)

		loginResult := &pb.Msg{Type: pb.Msg_LoginResultType, Union: &pb.Msg_LoginResult{&pb.LoginResult{Success: true}}}
		MyProtocolsManager.SendPidMsg(player.GetPid(), loginResult)
	}
}

//---------------------------------------------------------

type ProtocolFactory struct{}

func (pf *ProtocolFactory) CreateProtocol() mynet.Protocol {
	protocol := &ServerProtocol{}
	protocol.MsgReceiver = protocol
	return protocol
}

//---------------------------------------------------------

type myPidMsg struct {
	pid string
	msg *pb.Msg
}

type myProtocolPlayer struct {
	protocol *ServerProtocol
	player *pb.Player
}

type ProtocolsManager struct {
	Protocols          []*ServerProtocol
	AddProtocolChan    chan *ServerProtocol
	RemoveProtocolChan chan *ServerProtocol
	ChangeProtocolChan chan *myProtocolPlayer
	PidMsgChan         chan *myPidMsg
	BesidesPidMsgChan  chan *myPidMsg
}

func NewProtocolsManager() *ProtocolsManager {
	mypm := &ProtocolsManager{
		AddProtocolChan:    make(chan *ServerProtocol, 6),
		RemoveProtocolChan: make(chan *ServerProtocol, 6),
		ChangeProtocolChan: make(chan *myProtocolPlayer, 6),
		PidMsgChan:         make(chan *myPidMsg, 6),
		BesidesPidMsgChan:  make(chan *myPidMsg, 6)}
	go mypm.Run()
	return mypm
}

func (pm *ProtocolsManager) AddProtocol(p *ServerProtocol) {
	pm.AddProtocolChan <- p
}

func (pm *ProtocolsManager) RemoveProtocol(p *ServerProtocol) {
	pm.RemoveProtocolChan <- p
}

func (pm *ProtocolsManager) ChangeProtocol(protocol *ServerProtocol,player *pb.Player){
	pm.ChangeProtocolChan <- &myProtocolPlayer{protocol,player}
}

func (pm *ProtocolsManager) SendPidMsg(pid string, msg *pb.Msg) {
	pm.PidMsgChan <- &myPidMsg{pid, msg}
}

func (pm *ProtocolsManager) SendWithoutPidMsg(pid string, msg *pb.Msg) {
	pm.BesidesPidMsgChan <- &myPidMsg{pid, msg}
}

func (pm *ProtocolsManager) Run() {
	for {
		select {
		case protocol := <-pm.AddProtocolChan:
			pm.Protocols = append(pm.Protocols, protocol)

		case protocol := <-pm.RemoveProtocolChan:
			pm.implRemoveProtocol(protocol)

		case protocolPlayer := <-pm.ChangeProtocolChan:
			for _, p := range pm.Protocols {
				if p == protocolPlayer.protocol {
					p.Player = protocolPlayer.player
				}
			}

		case pidMsg := <-pm.PidMsgChan:
			for _, protocol := range pm.Protocols {
				if protocol.Player.Pid == pidMsg.pid {
					MyMsgSender.SendMsg(protocol.Conn, pidMsg.msg)
				}
			}

		case withoutPidMsg := <-pm.BesidesPidMsgChan:
			for _, protocol := range pm.Protocols {
				if protocol.Player.Pid != withoutPidMsg.pid {
					MyMsgSender.SendMsg(protocol.Conn, withoutPidMsg.msg)
				}
			}
		}
	}
}

func (pm *ProtocolsManager) implRemoveProtocol(protocol *ServerProtocol) {
	index := -1
	for i, p := range pm.Protocols {
		if p == protocol {
			index = i
			break
		}
	}

	removeOk := index != -1

	if removeOk {
		left := pm.Protocols[:index]
		right := pm.Protocols[index+1:]
		pm.Protocols = append(left, right...)
	}
}

//---------------------------------------------------------

type myConnMsg struct {
	Conn net.Conn
	Msg  *pb.Msg
}

type MsgSender struct {
	myConnMsgChan chan *myConnMsg
}

func NewMsgSender() *MsgSender {
	msgSender := &MsgSender{make(chan *myConnMsg, 6)}
	go msgSender.Run()
	return msgSender
}

func (ms *MsgSender) SendMsg(conn net.Conn, msg *pb.Msg) {
	ms.myConnMsgChan <- &myConnMsg{conn, msg}
}

func (ms *MsgSender) Run() {
	for {
		myConnMsg := <-ms.myConnMsgChan
		pbencoder.SendMsg(myConnMsg.Conn, myConnMsg.Msg)
	}
}

//---------------------------------------------------------

func IndexOfColor(color pb.Stone_Color) int {
	if color == pb.Stone_BLACK {
		return 0
	}else {
		return 1
	}
}

func ColorOfIndex(index int) pb.Stone_Color {
	if index == 0 {
		return pb.Stone_BLACK
	}else{
		return pb.Stone_WHITE
	}
}

func OtherIndex(index int) int {
	if index == 0 {
		return 1
	}else{
		return 0
	}
}

func OtherColor(color pb.Stone_Color) pb.Stone_Color {
	if color == pb.Stone_BLACK {
		return pb.Stone_WHITE
	}else{
		return pb.Stone_BLACK
	}
}

//=============================================================================

type MyGame struct {
	Game *pb.Game
	LinebrokenPlayers []*pb.Player

	AddStoneChan chan *pb.Stone
	RemoveStoneChan chan struct{}
	ClockNotifyChan chan *pb.ClockNotify

	PlayerComebackChan chan *pb.Player
	PlayerLinebrokenChan chan *pb.Player
}

//---------------------------------------------------------
// inner function
//---------------------------------------------------------

func (g *MyGame) isPlayerInGame(player *pb.Player) (ok bool) {
	for _,p := range g.Game.GetPlayers() {
		if p.GetPid() == player.GetPid() {
			ok = true
			break
		}
	}
	return
}

func (g *MyGame) isPlayerInLineBroken(player *pb.Player) (ok bool) {
	for _,p := range g.LinebrokenPlayers {
		if p.GetPid() == player.GetPid() {
			ok = true
			break
		}
	}
	return
}

func (g *MyGame) getOtherPlayer(player *pb.Player) (other *pb.Player) {
	for _,p := range g.Game.GetPlayers() {
		if player.GetPid() != p.GetPid() {
			other = p
		}
	}
	return
}

func (g *MyGame) implRemoveLinebrokenPlayer(player *pb.Player) {
	index := -1
	for i, p := range g.LinebrokenPlayers {
		if p.GetPid() == player.GetPid() {
			index = i
			break
		}
	}

	removeOk := index != -1

	if removeOk {
		left := g.LinebrokenPlayers[:index]
		right := g.LinebrokenPlayers[index+1:]
		g.LinebrokenPlayers = append(left, right...)
	}
}

//---------------------------------------------------------
// interface
//---------------------------------------------------------

func (g *MyGame) AddStone(stone *pb.Stone) {
	g.AddStoneChan <- stone
}

func (g *MyGame) RemoveStone() {
	g.RemoveStoneChan <- struct{}{}
}

func (g *MyGame) UpdateClock(clockNotify *pb.ClockNotify) {
	g.ClockNotifyChan <- clockNotify
}

func (g *MyGame) PlayerComeback(player *pb.Player) {
	g.PlayerComebackChan <- player
}

func (g *MyGame) PlayerLinebroken(player *pb.Player) {
	g.PlayerLinebrokenChan	<- player
}

func (g *MyGame) Run() {
	for {
		select {
		case stone := <- g.AddStoneChan:
			g.Game.Stones = append(g.Game.Stones,stone)

		case <- g.RemoveStoneChan:
			length := len(g.Game.Stones)
			if length > 0 {
				g.Game.Stones = g.Game.Stones[:length-1]
			}

		case clockNotify := <- g.ClockNotifyChan:
			index := IndexOfColor(clockNotify.GetColor())
			g.Game.Clocks[index] = clockNotify.Clock

			player := g.Game.Players[OtherIndex(index)]
			msg := &pb.Msg{Type: pb.Msg_ClockNotifyType,Union: &pb.Msg_ClockNotify{clockNotify}}
			MyProtocolsManager.SendPidMsg(player.GetPid(),msg)

		case player := <- g.PlayerComebackChan:
			if g.isPlayerInGame(player) && g.isPlayerInLineBroken(player) {
				g.implRemoveLinebrokenPlayer(player)
				if len(g.LinebrokenPlayers) == 0 {
					g.Game.State = pb.Game_RUNNING
					other := g.getOtherPlayer(player)
					msg := &pb.Msg{
						Type: pb.Msg_ComebackNotifyType,
						Union: &pb.Msg_ComebackNotify{&pb.ComebackNotify{Gid: g.Game.GetGid(), Pid: player.GetPid()}}}
					MyProtocolsManager.SendPidMsg(other.GetPid(),msg)
				}
			}

		case player := <- g.PlayerLinebrokenChan:
			if g.isPlayerInGame(player) {
				if length := len(g.LinebrokenPlayers); length == 0 {
					g.Game.State = pb.Game_PAUSED
					other := g.getOtherPlayer(player)
					msg := &pb.Msg{
						Type: pb.Msg_LinebrokenNotifyType,
						Union: &pb.Msg_LinebrokenNotify{&pb.LinebrokenNotify{Gid: g.Game.GetGid(), Pid: player.GetPid()}}}
					MyProtocolsManager.SendPidMsg(other.GetPid(),msg)
				}
				g.LinebrokenPlayers = append(g.LinebrokenPlayers,player)
			}
		}
	}
}

//=============================================================================

type GamesManager struct {
	Games []*MyGame

	AddGameChan chan *MyGame
	RemoveGameChan chan *MyGame
	PlayerComebackChan chan *pb.Player
	PlayerLinebrokenChan chan *pb.Player
}

func NewGamesManager() *GamesManager {
	gm := &GamesManager{AddGameChan: make(chan *MyGame,6),RemoveGameChan: make(chan *MyGame,6)}
	go gm.Run()
	return gm
}

func (gm *GamesManager) AddGame(game *MyGame) {
	gm.AddGameChan <- game
}

func (gm *GamesManager) RemoveGame(game *MyGame) {
	gm.RemoveGameChan <- game
}

func (gm *GamesManager) PlayerComeback(player *pb.Player) {
	gm.PlayerComebackChan <- player
}

func (gm *GamesManager) PlayerLinebroken(player *pb.Player) {
	gm.PlayerLinebrokenChan <- player
}

func (gm *GamesManager) Run() {
	for {
		select {
		case game := <- gm.AddGameChan:
			gm.Games = append(gm.Games,game)
		case game := <- gm.RemoveGameChan:
			gm.implRemoveGame(game)
		case player := <- gm.PlayerComebackChan:
			gm.implPlayerComeback(player)
		case player := <- gm.PlayerLinebrokenChan:
			gm.implPlayerLinebroken(player)
		}
	}
}

func (gm *GamesManager) implPlayerLinebroken(player *pb.Player) {
	for _,mygame := range gm.Games {
		mygame.PlayerLinebroken(player)
	}
}

func (gm *GamesManager) implPlayerComeback(player *pb.Player) {
	for _,mygame := range gm.Games {
		mygame.PlayerComeback(player)
	}
}

func (gm *GamesManager) implRemoveGame(game *MyGame){
	index := -1
	for i, g := range gm.Games {
		if g == game {
			index = i
			break
		}
	}

	removeOk := index != -1

	if removeOk {
		left := gm.Games[:index]
		right := gm.Games[index+1:]
		gm.Games = append(left, right...)
	}
}
