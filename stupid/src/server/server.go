package main

import (
	"pb"
	"log"
	"net"
	"math/rand"
	"time"
)

const (
	OP_ADD_SESSION = 1
	OP_REMOVE_SESSION = 2
	OP_DATA_SESSION = 3

	COUNTING_DOWN = 60*2
)
// ------------------------------------------------------------
type Session struct {
	Conn   net.Conn
	Player *pb.Player
}

type Message struct {
	Msg *pb.Msg
	Session *Session
}

type OpSession struct {
	Session *Session	
	Op int
}
// ------------------------------------------------------------
var sessions []*Session
var games []*pb.Game

var messages chan *Message
var opSession chan *OpSession

var idPool *IdPool

func Init(){
	const IdPoolSize = 100
	const ChanBuf = 5

	sessions  = []*Session{}
	games = []*pb.Game{}

	messages = make(chan *Message,12)
	opSession = make(chan *OpSession,6)

	rand.Seed(time.Now().UnixNano())

	idPool = NewIdPool(IdPoolSize)
}

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

func GetGamesByPid(pid string) (mygames []*pb.Game) {
	for _,g := range games {
		if IsPlayerInGame(pid,g) {
			mygames = append(mygames,g)
		}
	}
	return
}

func IsPlayerInGame(pid string,game *pb.Game) (inGame bool) {
	for _,player := range game.Players {
		if player.Pid == pid {
			inGame = true
			break
		}
	}
	return
}

func GetPlayers() (players []*pb.Player) {
	for _, session := range sessions {
		players = append(players, session.Player)
	}
	return
}

func LineBroken(session *Session) {
	player := session.Player

	if player != nil {
		// remove from sessions
		Leave(session)

		for _,game := range GetGamesByPid(player.Pid) {
			game.LineBroken = true
			msg := &pb.Msg{
				Type: pb.MsgType_LINE_BROKEN,
				Union: &pb.Msg_LineBroken{&pb.LineBroken{Gid: game.Gid}}}

			SendToOtherPlayer(game,session,msg)
		}
	}
}

func OtherColor(color pb.Color) (other pb.Color) {
	if color == pb.Color_BLACK {
		other = pb.Color_WHITE
	}else{
		other = pb.Color_BLACK
	}
	return
}

func NotBrokenColor(game *pb.Game) (color pb.Color) {
	for index,player := range game.Players {
		if _,ok := GetSession(player.Pid); ok {
			if int32(index) == game.BlackIndex {
				color = pb.Color_BLACK
			}else{
				color = pb.Color_WHITE
			}
		}
	}
	return
}

func Leave(session *Session) {
	opSession <- &OpSession{session,OP_REMOVE_SESSION}
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

func CanGameStart(game *pb.Game) bool {
	c := 0
	for _,player := range game.Players {
		if _,ok := GetSession(player.Pid); ok {
			c++
		}
	}
	return c == 2
}

func Dispatch() {
	recordCountResults := make(map[int32][]*pb.CountResult)

	for {
		select {
		// ################################################
		case myOpSession := <-opSession:
			switch myOpSession.Op {
			case OP_ADD_SESSION:
				msg := &pb.Msg{
					Type: pb.MsgType_LOGIN_OK,
					Union: &pb.Msg_LoginOk{
						&pb.LoginOk{
							Player: myOpSession.Session.Player,
							Data: &pb.Data{Players: GetPlayers(),Games: games}}}}
				SendMsg(myOpSession.Session, msg)

				AddSession(myOpSession.Session)

				mypid := myOpSession.Session.Player.Pid
				for _,game := range GetGamesByPid(mypid) {
					if game.LineBroken && game.State != pb.State_ENDED {
						msg1 := &pb.Msg{Type: pb.MsgType_GAME, Union: &pb.Msg_Game{game}}
						SendMsg(myOpSession.Session,msg1)

						if CanGameStart(game) {
							// 都回来了
							game.LineBroken = false
							msg := &pb.Msg{
								Type: pb.MsgType_COMEBACK,
								Union: &pb.Msg_Comeback{&pb.Comeback{Gid: game.Gid}}}
							SendToOtherPlayer(game,myOpSession.Session,msg)
						}
					}
				}

			case OP_REMOVE_SESSION:
				RemoveSession(myOpSession.Session)

			case OP_DATA_SESSION:
				msg := &pb.Msg{
					Type: pb.MsgType_DATA,
					Union: &pb.Msg_Data{&pb.Data{Players: GetPlayers(),Games: games}}}
				SendMsg(myOpSession.Session, msg)
			}
		// ################################################
		case message := <-messages:
			msg := message.Msg
			fromSession := message.Session

			switch msg.GetType() {
			case pb.MsgType_LOGIN:
				login := msg.GetLogin()
				if myplayer,ok := GetPlayer(login.Pid,login.Passwd); ok {
					fromSession.Player = myplayer
					opSession <- &OpSession{fromSession,OP_ADD_SESSION}
				}else{
					// relogin
					fromSession.Player = myplayer
				}

			case pb.MsgType_DATA:
				opSession <- &OpSession{fromSession,OP_DATA_SESSION}

			case pb.MsgType_LOGOUT:
				Leave(fromSession)

			case pb.MsgType_INVITE:
				invite := msg.GetInvite()
				if session,ok := GetSession(invite.Pid); ok {
					msg := &pb.Msg{
						Type: pb.MsgType_INVITE,
						Union: &pb.Msg_Invite{
							&pb.Invite{
								Pid: fromSession.Player.Pid,
								Proto: invite.Proto}}}
					SendMsg(session, msg)
				}
			// --------------------------------------------------------
			case pb.MsgType_INVITE_ANSWER:
				inviteAnswer := msg.GetInviteAnswer()
				if inviteAnswer.GetAgree() {
					if session,ok := GetSession(inviteAnswer.Pid); ok {
						game := CreateGame(fromSession,session,inviteAnswer.Proto)
						games = append(games,game)
						msg := &pb.Msg{Type: pb.MsgType_GAME, Union: &pb.Msg_Game{game}}
						SendMsg(fromSession,msg)
						SendMsg(session,msg)
					}
				}else{
					if session,ok := GetSession(inviteAnswer.Pid); ok {
						msg := &pb.Msg{
							Type: pb.MsgType_INVITE_ANSWER,
							Union: &pb.Msg_InviteAnswer{
								&pb.InviteAnswer{
									Agree: inviteAnswer.Agree,
									Pid: fromSession.Player.Pid,
									Proto: ExchangeWhoFirst(inviteAnswer.Proto)}}}
						SendMsg(session, msg)
					}
				}
			// --------------------------------------------------------
			case pb.MsgType_HAND:
				hand := msg.GetHand()
				if game,ok := GetGame(hand.Gid); ok {
					game.Stones = append(game.Stones,hand.Stone)

					msg := &pb.Msg{Type: pb.MsgType_HAND, Union: &pb.Msg_Hand{hand}}
					SendToOtherPlayer(game,fromSession,msg)
				}
			// --------------------------------------------------------
			case pb.MsgType_CLOCK_NOTIFY:
				clockNotify := msg.GetClockNotify()
				if game,ok := GetGame(clockNotify.Gid); ok {
					for index,player := range game.Players {
						if clockNotify.Pid == player.Pid {
							game.Clocks[index] = clockNotify.Clock
							break
						}
					}
					msg := &pb.Msg{
						Type: pb.MsgType_CLOCK_NOTIFY,
						Union: &pb.Msg_ClockNotify{clockNotify}}
					SendToOtherPlayer(game,fromSession,msg)
				}
			// --------------------------------------------------------
			case pb.MsgType_GAME_OVER:
				gameOver := msg.GetGameOver()
				if game,ok := GetGame(gameOver.Gid); ok {
					// change game state first
					game.State = pb.State_ENDED
					game.Result = gameOver.Result

					// and than resend the gameOver msg to the other player
					msg := &pb.Msg{Type: pb.MsgType_GAME_OVER, Union: &pb.Msg_GameOver{gameOver}}
					SendToOtherPlayer(game,fromSession,msg)
				}
			// --------------------------------------------------------
			case pb.MsgType_WILL_DEAD_STONE:
				willDeadStone := msg.GetWillDeadStone()
				if game,ok := GetGame(willDeadStone.Gid); ok {
					msg := &pb.Msg{Type: pb.MsgType_WILL_DEAD_STONE, Union: &pb.Msg_WillDeadStone{willDeadStone}}
					SendToOtherPlayer(game,fromSession,msg)
				}
			// --------------------------------------------------------
			case pb.MsgType_COUNT_REQUEST:
				countRequest := msg.GetCountRequest()
				if game,ok := GetGame(countRequest.Gid); ok {
					game.State = pb.State_PAUSED
					msg := &pb.Msg{
						Type: pb.MsgType_COUNT_REQUEST,
						Union: &pb.Msg_CountRequest{countRequest}}
					SendToOtherPlayer(game,fromSession,msg)
				}

			case pb.MsgType_COUNT_REQUEST_ANSWER:
				countRequestAnswer := msg.GetCountRequestAnswer()
				if game,ok := GetGame(countRequestAnswer.Gid); ok {
					if !countRequestAnswer.GetAgree() {
						game.State = pb.State_RUNNING
					}
					msg := &pb.Msg{
						Type: pb.MsgType_COUNT_REQUEST_ANSWER,
						Union: &pb.Msg_CountRequestAnswer{countRequestAnswer}}
					SendToOtherPlayer(game,fromSession,msg)
				}
			// --------------------------------------------------------
			case pb.MsgType_COUNT_RESULT:
				// gameover by count
				countResult := msg.GetCountResult()
				records,ok := recordCountResults[countResult.Gid]
				if ok {
					recordCountResults[countResult.Gid] = append(records,countResult)
				}else{
					recordCountResults[countResult.Gid] = []*pb.CountResult{countResult}
				}

				myCountResults := recordCountResults[countResult.Gid]

				if len(myCountResults) == 2 {
					if AgreeCountResults(myCountResults) {
						// Gameover
						msg := &pb.Msg{
							Type: pb.MsgType_GAME_OVER,
							Union: &pb.Msg_GameOver{
								&pb.GameOver{
									Gid: countResult.Gid,
									Result: countResult.Result}}}
						SendToAllPlayer(countResult.Gid,msg)
					}else{
						// Restart the game! Someone disagree,but i don't care who refuse. 
						msg := &pb.Msg{
							Type: pb.MsgType_DO_CONTINUE,
							Union: &pb.Msg_DoContinue{&pb.DoContinue{Gid: countResult.Gid}}}
						SendToAllPlayer(countResult.Gid,msg)
					}

					// clear the gid's count results
					delete(recordCountResults,countResult.Gid)
				}
			// --------------------------------------------------------
			case pb.MsgType_CANCEL:
				mycancel := msg.GetCancel()
				if game,ok := GetGame(mycancel.Gid); ok {
					if game.State == pb.State_PAUSED {
						msg := &pb.Msg{
							Type: pb.MsgType_DO_CONTINUE,
							Union: &pb.Msg_DoContinue{&pb.DoContinue{Gid: mycancel.Gid}}}
						SendToAllPlayer(mycancel.Gid,msg)
					}
					// clear the gid's count results
					delete(recordCountResults,mycancel.Gid)
				}
			// --------------------------------------------------------
			} // switch ended
		} // select ended
	} // for ended
}

func AgreeCountResults(theCountResults []*pb.CountResult) bool {
	result := true
	for _,countResult := range theCountResults {
		if !countResult.Agree {
			result = false
			break
		}
	}
	return result
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
	log.Printf("received msg: \n%s\n", msg)
	messages <- &Message{msg,session}
}

// --------------------------------------------------------

func main() {
	Init()
	go Dispatch()
	ListenAndServ()
}
