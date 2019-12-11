package main

import (
	"./pb"
	"log"
	"net"
	"math/rand"
	"time"

)

const (
	CMD_ADD_SESSION = 1
	CMD_REMOVE_SESSION = 2
	CMD_DATA =3
	CMD_INVITE = 4
	CMD_INVITE_ANSWER = 5
	CMD_HAND = 6
	CMD_COUNT_REQUEST = 7
	CMD_DEAD_STONES = 8
	CMD_COUNT_RESULT_ANSWER = 9
	CMD_GAME_OVER = 10
	CMD_COUNT_REQUEST_ANSWER = 11
)

// ------------------------------------------------------------

var sessions []*Session
var games []*pb.Game

var cmdChan chan int
var sessionChan chan *Session
var inviteChan chan *pb.Invite
var inviteAnswerChan chan *pb.InviteAnswer
var handChan chan *pb.Hand
var gameOverChan chan *pb.GameOver
var deadStonesChan chan *pb.DeadStones
var countResultAnswerChan chan *pb.CountResultAnswer
var countRequestChan chan *pb.CountRequest
var countRequestAnswerChan chan *pb.CountRequestAnswer

var gidResultChan chan *GidResult

var idPool *IdPool

// ------------------------------------------------------------

type Session struct {
	Conn   net.Conn
	Player *pb.Player
}

type GidResult struct {
	Gid int32
	Result *pb.Result
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
	gameOverChan = make(chan *pb.GameOver,ChanBuf)
	deadStonesChan = make(chan *pb.DeadStones,ChanBuf)
	countResultAnswerChan = make(chan *pb.CountResultAnswer,ChanBuf)
	countRequestChan = make(chan *pb.CountRequest,ChanBuf)
	countRequestAnswerChan = make(chan *pb.CountRequestAnswer,ChanBuf)
	gidResultChan = make(chan *GidResult,ChanBuf)

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

	// all deadStones messages saved here
	var recordDeadStones []*pb.DeadStones = make([]*pb.DeadStones,2)
	var recordCountResultAnswers []*pb.CountResultAnswer = make([]*pb.CountResultAnswer,2)

	// begin to serv
	for {
		select {
		case cmd := <-cmdChan:
			switch cmd {
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
				if inviteAnswer.GetAgree() {
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
									Agree: inviteAnswer.Agree,
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
					msg := &pb.Msg{
						Type: pb.MsgType_HAND,
						Union: &pb.Msg_Hand{hand},
					}
					SendToOtherPlayer(game,fromSession,msg)
				}

			case CMD_GAME_OVER:
				fromSession := <- sessionChan
				gameOver := <- gameOverChan

				if game,ok := GetGame(gameOver.Gid); ok {
					// change game state first
					game.State = pb.State_ENDED
					game.Result = gameOver.Result

					// and than resend the gameOver msg to the other player
					msg := &pb.Msg{
						Type: pb.MsgType_GAME_OVER,
						Union: &pb.Msg_GameOver{gameOver},
					}
					SendToOtherPlayer(game,fromSession,msg)
				}

			case CMD_DEAD_STONES:
				deadStones := <- deadStonesChan
				recordDeadStones = append(recordDeadStones,deadStones)
				if stones,count := GetDeadStones(recordDeadStones,deadStones.Gid); count == 2 {
					go ProcessCount(deadStones.Gid,stones)
				}

			case CMD_COUNT_RESULT_ANSWER:
				// gameover by count
				countResultAnswer := <- countResultAnswerChan
				recordCountResultAnswers = append(recordCountResultAnswers,countResultAnswer)
				if agree,all := AgreeCountResultAnswers(recordCountResultAnswers,countResultAnswer.Gid); all == 2 {
					if agree == 2 {
						// Gameover
						msg := &pb.Msg{
							Type: pb.MsgType_GAME_OVER,
							Union: &pb.Msg_GameOver{
								&pb.GameOver{
									Gid: countResultAnswer.Gid,
									Result: countResultAnswer.Result,
								},
							},
						}
						SendToAllPlayer(countResultAnswer.Gid,msg)
					}else{
						// Restart the game! Someone disagree,but i don't care who refuse. 
						msg := &pb.Msg{
							Type: pb.MsgType_DO_CONTINUE,
							Union: &pb.Msg_DoContinue{&pb.DoContinue{Gid: countResultAnswer.Gid}},
						}
						SendToAllPlayer(countResultAnswer.Gid,msg)
					}
				}

			case CMD_COUNT_REQUEST:
				fromSession := <- sessionChan
				countRequest := <- countRequestChan

				if game,ok := GetGame(countRequest.Gid); ok {
					msg := &pb.Msg{
						Type: pb.MsgType_COUNT_REQUEST,
						Union: &pb.Msg_CountRequest{countRequest},
					}
					SendToOtherPlayer(game,fromSession,msg)
				}

			case CMD_COUNT_REQUEST_ANSWER:
				fromSession := <- sessionChan
				countRequestAnswer := <- countRequestAnswerChan
				if game,ok := GetGame(countRequestAnswer.Gid); ok {

					game.State = pb.State_PAUSED

					msg := &pb.Msg{
						Type: pb.MsgType_COUNT_REQUEST_ANSWER,
						Union: &pb.Msg_CountRequestAnswer{countRequestAnswer},
					}
					SendToOtherPlayer(game,fromSession,msg)
				}

			} // switch ended

		case gidResult := <-gidResultChan:
			msg := &pb.Msg{
				Type: pb.MsgType_GAME_OVER,
				Union: &pb.Msg_GameOver{
					&pb.GameOver{
						Gid: gidResult.Gid,
						Result: gidResult.Result,
					},
				},
			}
			SendToAllPlayer(gidResult.Gid,msg)
		} // select ended
	} // for ended
}

func AgreeCountResultAnswers(theCountResultAnswers []*pb.CountResultAnswer,gid int32) (agree int,all int){
	for _,countResultAnswer := range theCountResultAnswers {
		if countResultAnswer.Gid == gid {
			all += 1
			if countResultAnswer.Agree {
				agree += 1
			}
		}
	}
	return
}

func ProcessCount(gid int32,deads []*pb.Stone) {
	var result *pb.Result
	// @todo Here,we compute the game result
	gidResultChan <- &GidResult{gid,result}
}

func CountForResult(gid int32,deads []*pb.Stone) (result *pb.Result) {
	// @todo 
	return
}

func GetDeadStones(theDeadStones []*pb.DeadStones,gid int32) (stones []*pb.Stone,count int) {
	for _,deadStones := range theDeadStones {
		if deadStones.Gid == gid {
			count += 1
			stones = append(stones,deadStones.Stones...)
		}
	}
	return
}

func SendToAllPlayer(gid int32,msg *pb.Msg) {
	if game,ok := GetGame(gid); ok {
		for _,p := range game.Players {
			if session,ok := GetSession(p.Pid); ok {
				SendMsg(session,msg)
			}
		}
	}
}

func SendToOtherPlayer(game *pb.Game,fromSession *Session,msg *pb.Msg) {
	for _,p := range game.Players {
		if p.Pid != fromSession.Player.Pid {
			if session,ok := GetSession(p.Pid); ok {
				SendMsg(session,msg)
			}
		}
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

	case pb.MsgType_GAME_OVER:
		cmdChan <- CMD_GAME_OVER
		sessionChan <- session
		gameOverChan <- msg.GetGameOver()

	case pb.MsgType_DEAD_STONES:
		cmdChan <- CMD_DEAD_STONES
		deadStonesChan <- msg.GetDeadStones()

	case pb.MsgType_COUNT_RESULT_ANSWER:
		cmdChan <- CMD_COUNT_RESULT_ANSWER
		countResultAnswerChan <- msg.GetCountResultAnswer()

	case pb.MsgType_COUNT_REQUEST:
		cmdChan <- CMD_COUNT_REQUEST
		sessionChan <- session
		countRequestChan <- msg.GetCountRequest()

	case pb.MsgType_COUNT_REQUEST_ANSWER:
		cmdChan <- CMD_COUNT_REQUEST_ANSWER
		sessionChan <- session
		countRequestAnswerChan <- msg.GetCountRequestAnswer()

	} //switch end
}

// ------------------------------------------------------------

func main() {
	go StartServ()
	Listen()
}
