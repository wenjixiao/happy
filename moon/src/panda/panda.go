package panda

import (
	"log"
	"mynet"
	"mynet/encoder/pbencoder"
	"net"
	"pb"
)

var msgSenderService *MsgSenderService
var protocolsService *ProtocolsService
var gamesService *GamesService

func init() {
	msgSenderService = NewMsgSenderService()
	protocolsService = NewProtocolsService()
	gamesService = NewGamesService()
}

//=============================================================================

type ServerProtocol struct {
	mynet.MsgProtocol
	Conn   net.Conn
	Player *pb.Player
}

func (sp *ServerProtocol) ConnectionMade(conn net.Conn) {
	log.Printf("connection made: %v", conn.RemoteAddr())
	sp.Conn = conn
	protocolsService.IAddProtocol(sp)
}

func (sp *ServerProtocol) ConnectionLost(err error) {
	log.Printf("connection lost: %v", err)
	protocolsService.IRemoveProtocol(sp)
	if sp.Player != nil {
		gamesService.IPlayerLinebroken(sp.Player)
	}
}

// override MsgProcessor
func (sp *ServerProtocol) ProcessMsg(msgBytes []byte) {
	msg := pbencoder.DecodeMsg(msgBytes)
	log.Printf("get msg: %v", msg)
	switch msg.GetType() {
	case pb.Msg_LoginType:
		player := &pb.Player{Pid: msg.GetLogin().GetPid()}
		protocolsService.IChangeProtocol(sp, player)

		// linebroken,now come back
		gamesService.IPlayerComeback(player)

		loginResult := &pb.Msg{Type: pb.Msg_LoginResultType, Union: &pb.Msg_LoginResult{&pb.LoginResult{Success: true}}}
		protocolsService.ISendPidMsg(player.GetPid(), loginResult)
	}
}

//=============================================================================

type ProtocolFactory struct{}

func (pf *ProtocolFactory) CreateProtocol() mynet.Protocol {
	protocol := &ServerProtocol{}
	protocol.MsgReceiver = protocol
	return protocol
}

//=============================================================================

type myPidMsg struct {
	pid string
	msg *pb.Msg
}

type myProtocolPlayer struct {
	protocol *ServerProtocol
	player   *pb.Player
}

type ProtocolsService struct {
	Protocols          []*ServerProtocol
	AddProtocolChan    chan *ServerProtocol
	RemoveProtocolChan chan *ServerProtocol
	ChangeProtocolChan chan *myProtocolPlayer
	PidMsgChan         chan *myPidMsg
	ExPidMsgChan       chan *myPidMsg
}

func NewProtocolsService() *ProtocolsService {
	myps := &ProtocolsService{
		AddProtocolChan:    make(chan *ServerProtocol, 6),
		RemoveProtocolChan: make(chan *ServerProtocol, 6),
		ChangeProtocolChan: make(chan *myProtocolPlayer, 6),
		PidMsgChan:         make(chan *myPidMsg, 6),
		ExPidMsgChan:       make(chan *myPidMsg, 6)}
	go myps.Run()
	return myps
}

func (ps *ProtocolsService) IAddProtocol(p *ServerProtocol) {
	ps.AddProtocolChan <- p
}

func (ps *ProtocolsService) IRemoveProtocol(p *ServerProtocol) {
	ps.RemoveProtocolChan <- p
}

func (ps *ProtocolsService) IChangeProtocol(protocol *ServerProtocol, player *pb.Player) {
	ps.ChangeProtocolChan <- &myProtocolPlayer{protocol, player}
}

func (ps *ProtocolsService) ISendPidMsg(pid string, msg *pb.Msg) {
	ps.PidMsgChan <- &myPidMsg{pid, msg}
}

func (ps *ProtocolsService) ISendExPidMsg(pid string, msg *pb.Msg) {
	ps.ExPidMsgChan <- &myPidMsg{pid, msg}
}

func (ps *ProtocolsService) Run() {
	for {
		select {
		case protocol := <-ps.AddProtocolChan:
			ps.Protocols = append(ps.Protocols, protocol)

		case protocol := <-ps.RemoveProtocolChan:
			ps.removeProtocol(protocol)

		case protocolPlayer := <-ps.ChangeProtocolChan:
			for _, p := range ps.Protocols {
				if p == protocolPlayer.protocol {
					p.Player = protocolPlayer.player
				}
			}

		case pidMsg := <-ps.PidMsgChan:
			for _, protocol := range ps.Protocols {
				if protocol.Player.Pid == pidMsg.pid {
					msgSenderService.ISendMsg(protocol.Conn, pidMsg.msg)
				}
			}

		case withoutPidMsg := <-ps.ExPidMsgChan:
			for _, protocol := range ps.Protocols {
				if protocol.Player.Pid != withoutPidMsg.pid {
					msgSenderService.ISendMsg(protocol.Conn, withoutPidMsg.msg)
				}
			}
		}
	}
}

func (ps *ProtocolsService) removeProtocol(protocol *ServerProtocol) {
	index := -1
	for i, p := range ps.Protocols {
		if p == protocol {
			index = i
			break
		}
	}

	removeOk := index != -1

	if removeOk {
		left := ps.Protocols[:index]
		right := ps.Protocols[index+1:]
		ps.Protocols = append(left, right...)
	}
}

//=============================================================================

type GameService struct {
	Game              *pb.Game
	LinebrokenPlayers []*pb.Player

	AddStoneChan    chan *pb.Stone
	RemoveStoneChan chan struct{}
	ClockNotifyChan chan *pb.ClockNotify

	PlayerComebackChan   chan *pb.Player
	PlayerLinebrokenChan chan *pb.Player
}

func (gs *GameService) isPlayerInGame(player *pb.Player) (ok bool) {
	for _, p := range gs.Game.GetPlayers() {
		if p.GetPid() == player.GetPid() {
			ok = true
			break
		}
	}
	return
}

func (gs *GameService) isPlayerInLineBroken(player *pb.Player) (ok bool) {
	for _, p := range gs.LinebrokenPlayers {
		if p.GetPid() == player.GetPid() {
			ok = true
			break
		}
	}
	return
}

func (gs *GameService) getOtherPlayer(player *pb.Player) (other *pb.Player) {
	for _, p := range gs.Game.GetPlayers() {
		if player.GetPid() != p.GetPid() {
			other = p
		}
	}
	return
}

func (gs *GameService) removeLinebrokenPlayer(player *pb.Player) {
	index := -1
	for i, p := range gs.LinebrokenPlayers {
		if p.GetPid() == player.GetPid() {
			index = i
			break
		}
	}

	removeOk := index != -1

	if removeOk {
		left := gs.LinebrokenPlayers[:index]
		right := gs.LinebrokenPlayers[index+1:]
		gs.LinebrokenPlayers = append(left, right...)
	}
}

func (gs *GameService) IAddStone(stone *pb.Stone) {
	gs.AddStoneChan <- stone
}

func (gs *GameService) IRemoveStone() {
	gs.RemoveStoneChan <- struct{}{}
}

func (gs *GameService) IUpdateClock(clockNotify *pb.ClockNotify) {
	gs.ClockNotifyChan <- clockNotify
}

func (gs *GameService) IPlayerComeback(player *pb.Player) {
	gs.PlayerComebackChan <- player
}

func (gs *GameService) IPlayerLinebroken(player *pb.Player) {
	gs.PlayerLinebrokenChan <- player
}

func (gs *GameService) Run() {
	for {
		select {
		case stone := <-gs.AddStoneChan:
			gs.Game.Stones = append(gs.Game.Stones, stone)

		case <-gs.RemoveStoneChan:
			length := len(gs.Game.Stones)
			if length > 0 {
				gs.Game.Stones = gs.Game.Stones[:length-1]
			}

		case clockNotify := <-gs.ClockNotifyChan:
			index := IndexOfColor(clockNotify.GetColor())
			gs.Game.Clocks[index] = clockNotify.Clock

			player := gs.Game.Players[OtherIndex(index)]
			msg := &pb.Msg{Type: pb.Msg_ClockNotifyType, Union: &pb.Msg_ClockNotify{clockNotify}}
			protocolsService.ISendPidMsg(player.GetPid(), msg)

		case player := <-gs.PlayerComebackChan:
			if gs.isPlayerInGame(player) && gs.isPlayerInLineBroken(player) {
				gs.removeLinebrokenPlayer(player)
				if len(gs.LinebrokenPlayers) == 0 {
					gs.Game.State = pb.Game_RUNNING
					other := gs.getOtherPlayer(player)
					msg := &pb.Msg{
						Type:  pb.Msg_ComebackNotifyType,
						Union: &pb.Msg_ComebackNotify{&pb.ComebackNotify{Gid: gs.Game.GetGid(), Pid: player.GetPid()}}}
					protocolsService.ISendPidMsg(other.GetPid(), msg)
				}
			}

		case player := <-gs.PlayerLinebrokenChan:
			if gs.isPlayerInGame(player) {
				if length := len(gs.LinebrokenPlayers); length == 0 {
					gs.Game.State = pb.Game_PAUSED
					other := gs.getOtherPlayer(player)
					msg := &pb.Msg{
						Type:  pb.Msg_LinebrokenNotifyType,
						Union: &pb.Msg_LinebrokenNotify{&pb.LinebrokenNotify{Gid: gs.Game.GetGid(), Pid: player.GetPid()}}}
					protocolsService.ISendPidMsg(other.GetPid(), msg)
				}
				gs.LinebrokenPlayers = append(gs.LinebrokenPlayers, player)
			}
		}
	}
}

//=============================================================================

type GamesService struct {
	Games []*GameService

	AddGameChan          chan *GameService
	RemoveGameChan       chan *GameService
	PlayerComebackChan   chan *pb.Player
	PlayerLinebrokenChan chan *pb.Player
}

func NewGamesService() *GamesService {
	gm := &GamesService{AddGameChan: make(chan *GameService, 6), RemoveGameChan: make(chan *GameService, 6)}
	go gm.Run()
	return gm
}

func (gss *GamesService) IAddGame(game *GameService) {
	gss.AddGameChan <- game
}

func (gss *GamesService) IRemoveGame(game *GameService) {
	gss.RemoveGameChan <- game
}

func (gss *GamesService) IPlayerComeback(player *pb.Player) {
	gss.PlayerComebackChan <- player
}

func (gss *GamesService) IPlayerLinebroken(player *pb.Player) {
	gss.PlayerLinebrokenChan <- player
}

func (gss *GamesService) Run() {
	for {
		select {
		case game := <-gss.AddGameChan:
			gss.Games = append(gss.Games, game)
		case game := <-gss.RemoveGameChan:
			gss.removeGame(game)
		case player := <-gss.PlayerComebackChan:
			gss.playerComeback(player)
		case player := <-gss.PlayerLinebrokenChan:
			gss.playerLinebroken(player)
		}
	}
}

func (gss *GamesService) playerLinebroken(player *pb.Player) {
	for _, myGame := range gss.Games {
		myGame.IPlayerLinebroken(player)
	}
}

func (gss *GamesService) playerComeback(player *pb.Player) {
	for _, myGame := range gss.Games {
		myGame.IPlayerComeback(player)
	}
}

func (gss *GamesService) removeGame(game *GameService) {
	index := -1
	for i, g := range gss.Games {
		if g == game {
			index = i
			break
		}
	}

	removeOk := index != -1

	if removeOk {
		left := gss.Games[:index]
		right := gss.Games[index+1:]
		gss.Games = append(left, right...)
	}
}

//=============================================================================

type myConnMsg struct {
	Conn net.Conn
	Msg  *pb.Msg
}

type MsgSenderService struct {
	myConnMsgChan chan *myConnMsg
}

func NewMsgSenderService() *MsgSenderService {
	msgSender := &MsgSenderService{make(chan *myConnMsg, 6)}
	go msgSender.Run()
	return msgSender
}

func (ms *MsgSenderService) ISendMsg(conn net.Conn, msg *pb.Msg) {
	ms.myConnMsgChan <- &myConnMsg{conn, msg}
}

func (ms *MsgSenderService) Run() {
	for {
		myConnMsg := <-ms.myConnMsgChan
		pbencoder.SendMsg(myConnMsg.Conn, myConnMsg.Msg)
	}
}

//=============================================================================

func IndexOfColor(color pb.Stone_Color) int {
	if color == pb.Stone_BLACK {
		return 0
	} else {
		return 1
	}
}

func ColorOfIndex(index int) pb.Stone_Color {
	if index == 0 {
		return pb.Stone_BLACK
	} else {
		return pb.Stone_WHITE
	}
}

func OtherIndex(index int) int {
	if index == 0 {
		return 1
	} else {
		return 0
	}
}

func OtherColor(color pb.Stone_Color) pb.Stone_Color {
	if color == pb.Stone_BLACK {
		return pb.Stone_WHITE
	} else {
		return pb.Stone_BLACK
	}
}

//=============================================================================
