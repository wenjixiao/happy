package main

import (
	"./pb"
	"log"
	"net"
	"math/rand"
	"time"

)

const (
	CMD_ADD_SESSION = 1;
	CMD_REMOVE_SESSION = 2;
	CMD_DATA =3;
	CMD_INVITE = 4;
	CMD_INVITE_ANSWER = 5;
	CMD_HAND = 6;
)

// ------------------------------------------------------------

var sessions []*Session
var games []*pb.Game

var cmdChan chan int
var sessionChan chan *Session
var inviteChan chan *pb.Invite
var inviteAnswerChan chan *pb.InviteAnswer
var handChan chan *pb.Hand

var idPool *IdPool

// ------------------------------------------------------------

type Session struct {
	Conn   net.Conn
	Player *pb.Player
}

// ------------------------------------------------------------

func Init(){
	const IdPoolSize = 100
	const ChanBuf = 5

	rand.Seed(time.Now().UnixNano())

	sessions  = []*Session{}
	games = []*pb.Game{}

	cmdChan = make(chan int,ChanBuf)
	sessionChan = make(chan *Session,ChanBuf)
	inviteChan = make(chan *pb.Invite,ChanBuf)
	inviteAnswerChan = make(chan *pb.InviteAnswer,ChanBuf)
	handChan = make(chan *pb.Hand,ChanBuf)

	idPool = NewIdPool(IdPoolSize)
}

// ------------------------------------------------------------

func AddSession(session *Session) {
	sessions = append(sessions, session)
}

func RemoveSession(session *Session) {
	var index int
	var mysession *Session
	for index, mysession = range sessions {
		if mysession == session {
			break
		}
	}
	copy(sessions[index:], sessions[index+1:])
	sessions = sessions[:len(sessions)-1]
}

func GetSession(pid string) (*Session,bool) {
	for _, session := range sessions {
		if session.Player.Pid == pid {
			return session,true
		}
	}
	return nil,false
}

func GetGame(gid int32) (*pb.Game,bool) {
	for _,game := range games {
		if game.Gid == gid {
			return game,true
		}
	}
	return nil,false
}

func GetPlayers() (players []*pb.Player) {
	for _, session := range sessions {
		players = append(players, session.Player)
	}
	return
}

func Leave(session *Session) {
	if session.Player != nil {
		session.Player = nil
		cmdChan <- CMD_REMOVE_SESSION
		sessionChan <- session
	}
}

func GetBlackIndex(proto *pb.Proto) (r int32) {
	if proto.WhoFirst == pb.WhoFirst_RANDOM {
		r = int32(rand.Intn(2))
	}else{
		if proto.WhoFirst == pb.WhoFirst_ME {
			r = 0
		}else{
			r = 1
		}
	}
	return
}

func CreateGame(fromSession *Session,toSession *Session,proto *pb.Proto) *pb.Game {
	game := &pb.Game{
		Gid: idPool.GetId(),
		Proto: proto,
		BlackIndex: GetBlackIndex(proto),
		Players: []*pb.Player{fromSession.Player,toSession.Player},
		Clocks: []*pb.Clock{&pb.Clock{},&pb.Clock{}},
		State: pb.State_RUNNING,
	}
	// init the player's clock,as proto defined
	for _,clock := range game.Clocks {
		clock.BaoLiu = proto.BaoLiu
		clock.CiShu = proto.CiShu
		clock.DuMiao = proto.DuMiao
	}
	return game
}

func ExchangeWhoFirst(proto *pb.Proto) *pb.Proto {
	whoFirst := proto.GetWhoFirst()
	if whoFirst != pb.WhoFirst_RANDOM {
		if whoFirst == pb.WhoFirst_ME {
			proto.WhoFirst = pb.WhoFirst_YOU
		} else {
			proto.WhoFirst = pb.WhoFirst_ME
		}
	}
	return proto
}

func StartServ() {
	// must init all global things frist
	Init()
	// begin to serv
	for {
		switch cmd := <-cmdChan; cmd {
		case CMD_ADD_SESSION:
			session := <- sessionChan
			AddSession(session)
			msg := &pb.Msg{
				Type: pb.MsgType_LOGIN_OK,
				Union: &pb.Msg_LoginOk{
					&pb.LoginOk{
						Player: session.Player,
						Data: &pb.Data{
							Players: GetPlayers(),
						},
					},
				},
			}
			SendMsg(session, msg)

		case CMD_REMOVE_SESSION:
			session := <- sessionChan
			RemoveSession(session)

		case CMD_DATA:
			fromSession := <- sessionChan
			msg := &pb.Msg{
				Type: pb.MsgType_DATA,
				Union: &pb.Msg_Data{
					&pb.Data{
						Players: GetPlayers(),
					},
				},
			}
			SendMsg(fromSession, msg)

		case CMD_INVITE:
			fromSession := <- sessionChan
			invite := <- inviteChan
			if session,ok := GetSession(invite.Pid); ok {
				msg := &pb.Msg{
					Type: pb.MsgType_INVITE,
					Union: &pb.Msg_Invite{
						&pb.Invite{
							Pid: fromSession.Player.Pid,
							Proto: invite.Proto,
						},
					},
				}
				SendMsg(session, msg)
			}

		case CMD_INVITE_ANSWER:
			fromSession := <- sessionChan
			inviteAnswer := <- inviteAnswerChan
			if inviteAnswer.GetIsAgree() {
				// here, we need create the game and tell players to play
				if session,ok := GetSession(inviteAnswer.Pid); ok {
					game := CreateGame(fromSession,session,inviteAnswer.Proto)
					games = append(games,game)
					msg := &pb.Msg{
						Type: pb.MsgType_GAME,
						Union: &pb.Msg_Game{game},
					}
					SendMsg(fromSession,msg)
					SendMsg(session,msg)
				}
			}else{
				// here, we need to notify the player your answer who invited you
				if session,ok := GetSession(inviteAnswer.Pid); ok {
					msg := &pb.Msg{
						Type: pb.MsgType_INVITE_ANSWER,
						Union: &pb.Msg_InviteAnswer{
							&pb.InviteAnswer{
								IsAgree: inviteAnswer.IsAgree,
								Pid: fromSession.Player.Pid,
								Proto: ExchangeWhoFirst(inviteAnswer.Proto),
							},
						},
					}
					SendMsg(session, msg)
				}
			}

		case CMD_HAND:
			fromSession := <- sessionChan
			hand := <- handChan

			if game,ok := GetGame(hand.Gid); ok {
				for _,p := range game.Players {
					if p.Pid != fromSession.Player.Pid {
						if session,ok := GetSession(p.Pid); ok {
							msg := &pb.Msg{
								Type: pb.MsgType_HAND,
								Union: &pb.Msg_Hand{hand},
							}
							SendMsg(session,msg)
						}
					}
				}
			}
		}// switch end
	}
}

func ProcessMsg(session *Session, msg *pb.Msg) {
	log.Printf("Received msg: \n%s\n", msg)
	switch msg.GetType() {
	case pb.MsgType_LOGIN:
		login := msg.GetLogin()
		if myplayer,ok := GetPlayer(login.Pid,login.Passwd); ok {
			session.Player = myplayer
			cmdChan <- CMD_ADD_SESSION
			sessionChan <- session
		}else{
			// relogin
			session.Player = myplayer
		}

	case pb.MsgType_DATA:
		cmdChan <- CMD_DATA
		sessionChan <- session

	case pb.MsgType_LOGOUT:
		Leave(session)

	case pb.MsgType_INVITE:
		cmdChan <- CMD_INVITE
		sessionChan <- session
		inviteChan <- msg.GetInvite()

	case pb.MsgType_INVITE_ANSWER:
		cmdChan <- CMD_INVITE_ANSWER
		sessionChan <- session
		inviteAnswerChan <- msg.GetInviteAnswer()

	case pb.MsgType_HAND:
		cmdChan <- CMD_HAND
		sessionChan <- session
		handChan <- msg.GetHand()
	}
}

// ------------------------------------------------------------

func main() {
	go StartServ()
	Listen()
}
