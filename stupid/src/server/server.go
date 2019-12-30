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

type WithSession struct {
	Session *Session
}

type MyOpSession struct {
	WithSession
	Op int
}

type MySessionMsg struct {
	WithSession
	Msg *pb.Msg
}
// ------------------------------------------------------------
type MyInvite struct {
	WithSession
	Invite *pb.Invite
}

type MyInviteAnswer struct {
	WithSession
	InviteAnswer *pb.InviteAnswer
}
// ------------------------------------------------------------
type MyHand struct {
	WithSession
	Hand *pb.Hand
}

type MyClockNotify struct {
	WithSession
	ClockNotify *pb.ClockNotify
}
// ------------------------------------------------------------
type MyCountRequest struct {
	WithSession
	CountRequest *pb.CountRequest	
}

type MyCountRequestAnswer struct {
	WithSession
	CountRequestAnswer *pb.CountRequestAnswer
}
// ------------------------------------------------------------
type MyCountResult struct {
	CountResult *pb.CountResult
}

type MyGameOver struct {
	WithSession
	GameOver *pb.GameOver
}

type MyWillDeadStone struct {
	WithSession
	WillDeadStone *pb.WillDeadStone
} 

// ------------------------------------------------------------

var sessions []*Session
var games []*pb.Game

var myOpSessionChan chan *MyOpSession
var mySessionMsgChan chan *MySessionMsg
var myInviteChan chan *MyInvite
var myInviteAnswerChan chan *MyInviteAnswer
var myHandChan chan *MyHand
var myClockNotifyChan chan *MyClockNotify
var myWillDeadStoneChan chan *MyWillDeadStone
var myCountRequestChan chan *MyCountRequest
var myCountRequestAnswerChan chan *MyCountRequestAnswer
var myCountResultChan chan *MyCountResult
var myGameOverChan chan *MyGameOver

var idPool *IdPool

func Init(){
	const IdPoolSize = 100
	const ChanBuf = 5

	sessions  = []*Session{}
	games = []*pb.Game{}

	myOpSessionChan = make(chan *MyOpSession,ChanBuf)
	mySessionMsgChan = make(chan *MySessionMsg,ChanBuf*2)
	myInviteChan = make(chan *MyInvite,ChanBuf)
	myInviteAnswerChan = make(chan *MyInviteAnswer,ChanBuf)
	myHandChan = make(chan *MyHand,ChanBuf)
	myClockNotifyChan = make(chan *MyClockNotify,ChanBuf)
	myCountRequestChan = make(chan *MyCountRequest,ChanBuf)
	myCountRequestAnswerChan = make(chan *MyCountRequestAnswer,ChanBuf)
	myWillDeadStoneChan = make(chan *MyWillDeadStone,ChanBuf)
	myCountResultChan = make(chan *MyCountResult,ChanBuf)
	myGameOverChan = make(chan *MyGameOver,ChanBuf)

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
	myOpSessionChan <- &MyOpSession{WithSession{session},OP_REMOVE_SESSION}
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
		case myOpSession := <-myOpSessionChan:
			switch myOpSession.Op {
			case OP_ADD_SESSION:
				msg := &pb.Msg{
					Type: pb.MsgType_LOGIN_OK,
					Union: &pb.Msg_LoginOk{
						&pb.LoginOk{
							Player: myOpSession.Session.Player,
							Data: &pb.Data{Players: GetPlayers(),Games: games}}}}
				SendMessage(myOpSession.Session, msg)

				AddSession(myOpSession.Session)

				mypid := myOpSession.Session.Player.Pid
				for _,game := range GetGamesByPid(mypid) {
					if game.LineBroken && game.State != pb.State_ENDED {
						msg1 := &pb.Msg{Type: pb.MsgType_GAME, Union: &pb.Msg_Game{game}}
						SendMessage(myOpSession.Session,msg1)

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
				SendMessage(myOpSession.Session, msg)
			}

		case myInvite := <-myInviteChan:
			fromSession := myInvite.Session
			invite := myInvite.Invite
			if session,ok := GetSession(invite.Pid); ok {
				msg := &pb.Msg{
					Type: pb.MsgType_INVITE,
					Union: &pb.Msg_Invite{
						&pb.Invite{
							Pid: fromSession.Player.Pid,
							Proto: invite.Proto}}}
				SendMessage(session, msg)
			}

		case myInviteAnswer := <-myInviteAnswerChan:
			fromSession := myInviteAnswer.Session
			inviteAnswer := myInviteAnswer.InviteAnswer
			if inviteAnswer.GetAgree() {
				if session,ok := GetSession(inviteAnswer.Pid); ok {
					game := CreateGame(fromSession,session,inviteAnswer.Proto)
					games = append(games,game)
					msg := &pb.Msg{Type: pb.MsgType_GAME, Union: &pb.Msg_Game{game}}
					SendMessage(fromSession,msg)
					SendMessage(session,msg)
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
					SendMessage(session, msg)
				}
			}

		case myHand := <-myHandChan:
			if game,ok := GetGame(myHand.Hand.Gid); ok {
				game.Stones = append(game.Stones,myHand.Hand.Stone)

				msg := &pb.Msg{Type: pb.MsgType_HAND, Union: &pb.Msg_Hand{myHand.Hand}}
				SendToOtherPlayer(game,myHand.Session,msg)
			}

		case myClockNotify := <-myClockNotifyChan:
			if game,ok := GetGame(myClockNotify.ClockNotify.Gid); ok {
				for index,player := range game.Players {
					if myClockNotify.ClockNotify.Pid == player.Pid {
						game.Clocks[index] = myClockNotify.ClockNotify.Clock
						break
					}
				}
				msg := &pb.Msg{
					Type: pb.MsgType_CLOCK_NOTIFY,
					Union: &pb.Msg_ClockNotify{myClockNotify.ClockNotify}}
				SendToOtherPlayer(game,myClockNotify.Session,msg)
			}

		case myGameOver := <-myGameOverChan:
			fromSession := myGameOver.Session
			gameOver := myGameOver.GameOver

			if game,ok := GetGame(gameOver.Gid); ok {
				// change game state first
				game.State = pb.State_ENDED
				game.Result = gameOver.Result

				// and than resend the gameOver msg to the other player
				msg := &pb.Msg{Type: pb.MsgType_GAME_OVER, Union: &pb.Msg_GameOver{gameOver}}
				SendToOtherPlayer(game,fromSession,msg)
			}

		case myWillDeadStone := <-myWillDeadStoneChan:
			fromSession := myWillDeadStone.Session
			willDeadStone := myWillDeadStone.WillDeadStone
			if game,ok := GetGame(willDeadStone.Gid); ok {
				msg := &pb.Msg{Type: pb.MsgType_WILL_DEAD_STONE, Union: &pb.Msg_WillDeadStone{willDeadStone}}
				SendToOtherPlayer(game,fromSession,msg)
			}

		case myCountRequest := <-myCountRequestChan:
			fromSession := myCountRequest.Session
			countRequest := myCountRequest.CountRequest

			if game,ok := GetGame(countRequest.Gid); ok {

				game.State = pb.State_PAUSED

				msg := &pb.Msg{
					Type: pb.MsgType_COUNT_REQUEST,
					Union: &pb.Msg_CountRequest{countRequest}}
				SendToOtherPlayer(game,fromSession,msg)
			}

		case myCountRequestAnswer := <-myCountRequestAnswerChan:
			fromSession := myCountRequestAnswer.Session
			countRequestAnswer := myCountRequestAnswer.CountRequestAnswer

			if game,ok := GetGame(countRequestAnswer.Gid); ok {
				if !countRequestAnswer.GetAgree() {
					game.State = pb.State_RUNNING
				}
				msg := &pb.Msg{
					Type: pb.MsgType_COUNT_REQUEST_ANSWER,
					Union: &pb.Msg_CountRequestAnswer{countRequestAnswer}}
				SendToOtherPlayer(game,fromSession,msg)
			}

		case myCountResult := <-myCountResultChan:
			// gameover by count
			countResult := myCountResult.CountResult

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
				SendMessage(session,msg)
			}
		}
	}
}

func SendToOtherPlayer(game *pb.Game,fromSession *Session,msg *pb.Msg) {
	for _,p := range game.Players {
		if p.Pid != fromSession.Player.Pid {
			if session,ok := GetSession(p.Pid); ok {
				SendMessage(session,msg)
			}
		}
	}
}

func SendingMsg(){
	for mySessionMsg := range mySessionMsgChan {
		msg := mySessionMsg.Msg
		SendMsg(mySessionMsg.Session,msg)
		log.Printf("sended msg: \n%s\n",msg)
	}
}

func SendMessage(session *Session,msg *pb.Msg) {
	mySessionMsgChan <- &MySessionMsg{WithSession{session},msg}
}

func ProcessMsg(session *Session, msg *pb.Msg) {
	log.Printf("received msg: \n%s\n", msg)
	switch msg.GetType() {
	case pb.MsgType_LOGIN:
		login := msg.GetLogin()
		if myplayer,ok := GetPlayer(login.Pid,login.Passwd); ok {
			session.Player = myplayer
			myOpSessionChan <- &MyOpSession{WithSession{session},OP_ADD_SESSION}
		}else{
			// relogin
			session.Player = myplayer
		}

	case pb.MsgType_DATA:
		myOpSessionChan <- &MyOpSession{WithSession{session},OP_DATA_SESSION}

	case pb.MsgType_LOGOUT:
		Leave(session)

	case pb.MsgType_INVITE:
		myInviteChan <- &MyInvite{WithSession{session},msg.GetInvite()}

	case pb.MsgType_INVITE_ANSWER:
		myInviteAnswerChan <- &MyInviteAnswer{WithSession{session},msg.GetInviteAnswer()}

	case pb.MsgType_HAND:
		myHandChan <- &MyHand{WithSession{session},msg.GetHand()}

	case pb.MsgType_CLOCK_NOTIFY:
		myClockNotifyChan <- &MyClockNotify{WithSession{session},msg.GetClockNotify()}

	case pb.MsgType_GAME_OVER:
		myGameOverChan <- &MyGameOver{WithSession{session},msg.GetGameOver()}

	case pb.MsgType_WILL_DEAD_STONE:
		myWillDeadStoneChan <- &MyWillDeadStone{WithSession{session},msg.GetWillDeadStone()}

	case pb.MsgType_COUNT_RESULT:
		myCountResultChan <- &MyCountResult{msg.GetCountResult()}

	case pb.MsgType_COUNT_REQUEST:
		myCountRequestChan <- &MyCountRequest{WithSession{session},msg.GetCountRequest()}

	case pb.MsgType_COUNT_REQUEST_ANSWER:
		myCountRequestAnswerChan <- &MyCountRequestAnswer{WithSession{session},msg.GetCountRequestAnswer()}

	} //switch end
}

// ------------------------------------------------------------

func main() {
	Init()
	go SendingMsg()
	go Dispatch()
	ListenAndServ()
}
