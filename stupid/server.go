package main

import (
	"./pb"
	"log"
	"net"
)

const (
	CMD_ADD_SESSION = 1;
	CMD_REMOVE_SESSION = 2;
	CMD_DATA =3;
	CMD_INVITE = 4;
	CMD_INVITE_ANSWER = 5;
)

// ------------------------------------------------------------

var sessions []*Session = []*Session{}
var games []*pb.Game = []*pb.Game{}

var cmdChan chan int = make(chan int,5)
var sessionChan chan *Session = make(chan *Session,5)
var inviteChan chan *pb.Invite = make(chan *pb.Invite,5)
var inviteAnswerChan chan *pb.InviteAnswer = make(chan *pb.InviteAnswer,5)

var idPool *IdPool = NewIdPool(IdPoolSize)

// ------------------------------------------------------------

type Session struct {
	Conn   net.Conn
	Player *pb.Player
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

func GetSession(pid string) *Session {
	for _, session := range sessions {
		if session.Player.Pid == pid {
			return session
		}
	}
	return nil
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

func CreateGame(fromSession *Session,toSession *Session,proto *pb.Proto) *pb.Game {
	game := &pb.Game{
		Gid: idPool.GetId(),
		Proto: proto,
		Players: []*pb.Player{fromSession.Player,toSession.Player},
		Clocks: []*pb.Clock{&pb.Clock{},&pb.Clock{}},
	}

	for _,clock := range game.Clocks {
		clock.BaoLiu = proto.Clock.BaoLiu
		clock.DuMiao = proto.Clock.DuMiao
		clock.CiShu = proto.Clock.CiShu
		clock.MeiCi = proto.Clock.MeiCi
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
			
			if session := GetSession(invite.Pid); session != nil {
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
				if session := GetSession(inviteAnswer.Pid); session != nil {
					game := CreateGame(fromSession,session,inviteAnswer.Proto)
					games = append(games,game)
					msg := &pb.Msg{
						Type: pb.MsgType_GAME,
						Union: &pb.Msg_Game{game},
					}
					for _,mysession := range []*Session{fromSession,session} {
						SendMsg(mysession,msg)
					}
				}
			}else{
				// here, we need to notify the player your answer who invited you
				if session := GetSession(inviteAnswer.Pid); session != nil {
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
		// switch end
		}
	}
}

func ProcessMsg(session *Session, msg *pb.Msg) {
	log.Printf("Received msg: \n%s\n", msg)
	switch msg.GetType() {
	case pb.MsgType_LOGIN:
		login := msg.GetLogin()
		myplayer := &pb.Player{
			Pid:    login.Pid,
			Passwd: login.Passwd,
		}
		if session.Player == nil {
			session.Player = myplayer
			cmdChan <- CMD_ADD_SESSION
			sessionChan <- session
		} else {
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
		inviteChan <- msg.GetInvite()

	case pb.MsgType_INVITE_ANSWER:
		cmdChan <- CMD_INVITE_ANSWER
		inviteAnswerChan <- msg.GetInviteAnswer()
	}
}

// ------------------------------------------------------------

func main() {
	go StartServ()
	Listen()
}
