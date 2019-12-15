package main

import (
	"./pb"
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

type Session struct {
	Conn   net.Conn
	Player *pb.Player
}

type WithSession struct {
	Session *Session
}

type MyGidResult struct {
	Gid int32
	Result *pb.Result
}

type MyOpSession struct {
	WithSession
	Op int
}

type MyInvite struct {
	WithSession
	Invite *pb.Invite
}

type MyInviteAnswer struct {
	WithSession
	InviteAnswer *pb.InviteAnswer
}

type MyHand struct {
	WithSession
	Hand *pb.Hand
}

type MyClockNotify struct {
	WithSession
	ClockNotify *pb.ClockNotify
}

type MyGameOver struct {
	WithSession
	GameOver *pb.GameOver
}

type MyDeadStones struct {
	DeadStones *pb.DeadStones		
} 

type MyCountResultAnswer struct {
	CountResultAnswer *pb.CountResultAnswer
}

type MyCountRequest struct {
	WithSession
	CountRequest *pb.CountRequest	
}

type MyCountRequestAnswer struct {
	WithSession
	CountRequestAnswer *pb.CountRequestAnswer
}

type MySessionMsg struct {
	WithSession
	Msg *pb.Msg
}

type GidPid struct {
	Gid int32
	Pid string
}

var sessions []*Session
var games []*pb.Game
var countdowns map[GidPid]int32

var myGidResultChan chan *MyGidResult
var myOpSessionChan chan *MyOpSession
var myInviteChan chan *MyInvite
var myInviteAnswerChan chan *MyInviteAnswer
var myHandChan chan *MyHand
var myGameOverChan chan *MyGameOver
var myDeadStonesChan chan *MyDeadStones
var myCountResultAnswerChan chan *MyCountResultAnswer
var myCountRequestChan chan *MyCountRequest
var myCountRequestAnswerChan chan *MyCountRequestAnswer
var mySessionMsgChan chan *MySessionMsg
var myClockNotifyChan chan *MyClockNotify

var idPool *IdPool

func Init(){
	const IdPoolSize = 100
	const ChanBuf = 5

	sessions  = []*Session{}
	games = []*pb.Game{}
	// record the linebroken game and players
	countdowns = make(map[GidPid]int32)

	myGidResultChan = make(chan *MyGidResult,ChanBuf)
	myOpSessionChan = make(chan *MyOpSession,ChanBuf)
	myInviteChan = make(chan *MyInvite,ChanBuf)
	myInviteAnswerChan = make(chan *MyInviteAnswer,ChanBuf)
	myHandChan = make(chan *MyHand,ChanBuf)
	myGameOverChan = make(chan *MyGameOver,ChanBuf)
	myDeadStonesChan = make(chan *MyDeadStones,ChanBuf)
	myCountResultAnswerChan = make(chan *MyCountResultAnswer,ChanBuf)
	myCountRequestChan = make(chan *MyCountRequest,ChanBuf)
	myCountRequestAnswerChan = make(chan *MyCountRequestAnswer,ChanBuf)
	mySessionMsgChan = make(chan *MySessionMsg,ChanBuf*2)
	myClockNotifyChan = make(chan *MyClockNotify,ChanBuf)

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
			game.State = pb.State_BROKEN

			msg := &pb.Msg{
				Type: pb.MsgType_LINE_BROKEN,
				Union: &pb.Msg_LineBroken{&pb.LineBroken{Gid: game.Gid}}}

			SendToOtherPlayer(game,session,msg)

			countdowns[GidPid{game.Gid,player.Pid}] = COUNTING_DOWN
		}
	}
}

func LineBrokenEnded(gidpid GidPid){
	if game,ok := GetGame(gidpid.Gid); ok {
		result := &pb.Result{
			Winner: NotBrokenColor(game), 
			EndType: pb.EndType_LINEBROKEN}

		game.State = pb.State_ENDED
		game.Result = result

		msg := &pb.Msg{
			Type: pb.MsgType_GAME_OVER,
			Union: &pb.Msg_GameOver{&pb.GameOver{Gid: game.Gid, Result: result}}}

		lineBrokenSend(game,gidpid.Pid,msg)
	}
}

func lineBrokenSend(game *pb.Game,brokenPid string,msg *pb.Msg) {
	for _,player := range game.Players {
		if player.Pid != brokenPid {
			if session,ok := GetSession(player.Pid); ok {
				SendMessage(session,msg)
			}
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

func sendCountDown(gidpid GidPid,count int32){
	msg := &pb.Msg{
		Type: pb.MsgType_COUNT_DOWN,
		Union: &pb.Msg_CountDown{&pb.CountDown{Count: count}}}

	if game,ok := GetGame(gidpid.Gid); ok {
		lineBrokenSend(game,gidpid.Pid,msg)
	}
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
	// all deadStones messages saved here
	var recordDeadStones []*pb.DeadStones = make([]*pb.DeadStones,2)
	var recordCountResultAnswers []*pb.CountResultAnswer = make([]*pb.CountResultAnswer,2)

	timer := time.NewTimer(time.Second)
	// defer timer.Stop()

	// begin
	for {
		select {
		case <-timer.C:
			if len(countdowns) > 0 {
				for gidpid,count := range countdowns {
					if count > 10 {
						if count % 10 == 0 {
							sendCountDown(gidpid,count)
						}
						countdowns[gidpid] -= 1
					}else{
						if count < 0 {
							LineBrokenEnded(gidpid)
							break
						}else{
							sendCountDown(gidpid,count)
							countdowns[gidpid] -= 1
						}
					}
				}
			}

		case myOpSession := <-myOpSessionChan:
			switch myOpSession.Op {
			case OP_ADD_SESSION:
				mypid := myOpSession.Session.Player.Pid
				mygames := GetGamesByPid(mypid)
				for _,game := range mygames {
					if game.State == pb.State_BROKEN {
						delete(countdowns,GidPid{game.Gid,mypid})

						stateChanged := false
						if CanGameStart(game) {
							game.State = pb.State_RUNNING
							stateChanged = true
						}

						// return game when already have game
						msg := &pb.Msg{Type: pb.MsgType_GAME, Union: &pb.Msg_Game{game}}
						SendMessage(myOpSession.Session,msg)

						// state change notify
						if stateChanged {
							msg1 := &pb.Msg{
								Type: pb.MsgType_STATE_CHANGED, 
								Union: &pb.Msg_StateChanged{&pb.StateChanged{Gid:game.Gid,State: pb.State_RUNNING}}}
							SendToOtherPlayer(game,myOpSession.Session,msg1)
						}
					}
				}

				AddSession(myOpSession.Session)

				msg := &pb.Msg{
					Type: pb.MsgType_LOGIN_OK,
					Union: &pb.Msg_LoginOk{
						&pb.LoginOk{
							Player: myOpSession.Session.Player,
							Data: &pb.Data{Players: GetPlayers(),Games: games}}}}
				SendMessage(myOpSession.Session, msg)

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
				// here, we need create the game and tell players to play
				if session,ok := GetSession(inviteAnswer.Pid); ok {
					game := CreateGame(fromSession,session,inviteAnswer.Proto)
					games = append(games,game)
					msg := &pb.Msg{Type: pb.MsgType_GAME, Union: &pb.Msg_Game{game}}
					SendMessage(fromSession,msg)
					SendMessage(session,msg)
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
								Proto: ExchangeWhoFirst(inviteAnswer.Proto)}}}
					SendMessage(session, msg)
				}
			}

		case myHand := <-myHandChan:
			if game,ok := GetGame(myHand.Hand.Gid); ok {
				msg := &pb.Msg{Type: pb.MsgType_HAND, Union: &pb.Msg_Hand{myHand.Hand}}
				SendToOtherPlayer(game,myHand.Session,msg)
			}

		case myClockNotify := <-myClockNotifyChan:
			if game,ok := GetGame(myClockNotify.ClockNotify.Gid); ok {
				// change the game's clock at server too
				for index,player := range game.Players {
					if myClockNotify.ClockNotify.Pid == player.Pid {
						game.Clocks[index] = myClockNotify.ClockNotify.Clock
						break
					}
				}
				// send clock msg to other player
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

		case myDeadStones := <-myDeadStonesChan:
			deadStones := myDeadStones.DeadStones
			recordDeadStones = append(recordDeadStones,deadStones)
			if stones,count := GetDeadStones(recordDeadStones,deadStones.Gid); count == 2 {
				go ProcessCount(deadStones.Gid,stones)
			}

		case myCountResultAnswer := <-myCountResultAnswerChan:
			// gameover by count
			countResultAnswer := myCountResultAnswer.CountResultAnswer
			recordCountResultAnswers = append(recordCountResultAnswers,countResultAnswer)
			if agree,all := AgreeCountResultAnswers(recordCountResultAnswers,countResultAnswer.Gid); all == 2 {
				if agree == 2 {
					// Gameover
					msg := &pb.Msg{
						Type: pb.MsgType_GAME_OVER,
						Union: &pb.Msg_GameOver{
							&pb.GameOver{
								Gid: countResultAnswer.Gid,
								Result: countResultAnswer.Result}}}
					SendToAllPlayer(countResultAnswer.Gid,msg)
				}else{
					// Restart the game! Someone disagree,but i don't care who refuse. 
					msg := &pb.Msg{
						Type: pb.MsgType_DO_CONTINUE,
						Union: &pb.Msg_DoContinue{&pb.DoContinue{Gid: countResultAnswer.Gid}}}
					SendToAllPlayer(countResultAnswer.Gid,msg)
				}
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

		case myGidResult := <-myGidResultChan:
			msg := &pb.Msg{
				Type: pb.MsgType_GAME_OVER,
				Union: &pb.Msg_GameOver{
					&pb.GameOver{
						Gid: myGidResult.Gid,
						Result: myGidResult.Result}}}
			SendToAllPlayer(myGidResult.Gid,msg)

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
	myGidResultChan <- &MyGidResult{gid,result}
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

func MySendMsg(){
	for mySessionMsg := range mySessionMsgChan {
		SendMsg(mySessionMsg.Session,mySessionMsg.Msg)
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

	case pb.MsgType_DEAD_STONES:
		myDeadStonesChan <- &MyDeadStones{msg.GetDeadStones()}

	case pb.MsgType_COUNT_RESULT_ANSWER:
		myCountResultAnswerChan <- &MyCountResultAnswer{msg.GetCountResultAnswer()}

	case pb.MsgType_COUNT_REQUEST:
		myCountRequestChan <- &MyCountRequest{WithSession{session},msg.GetCountRequest()}

	case pb.MsgType_COUNT_REQUEST_ANSWER:
		myCountRequestAnswerChan <- &MyCountRequestAnswer{WithSession{session},msg.GetCountRequestAnswer()}

	} //switch end
}

// ------------------------------------------------------------

func main() {
	Init()
	go MySendMsg()
	go Dispatch()
	ListenAndServ()
}
