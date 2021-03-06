// Code generated by protoc-gen-go. DO NOT EDIT.
// source: msg.proto

package pb

import (
	fmt "fmt"
	proto "github.com/golang/protobuf/proto"
	math "math"
)

// Reference imports to suppress errors if they are not otherwise used.
var _ = proto.Marshal
var _ = fmt.Errorf
var _ = math.Inf

// This is a compile-time assertion to ensure that this generated file
// is compatible with the proto package it is being compiled against.
// A compilation error at this line likely means your copy of the
// proto package needs to be updated.
const _ = proto.ProtoPackageIsVersion3 // please upgrade the proto package

type Type int32

const (
	Type_LOGIN        Type = 0
	Type_LOGOUT       Type = 1
	Type_PLAYER       Type = 2
	Type_LOGIN_RESULT Type = 3
)

var Type_name = map[int32]string{
	0: "LOGIN",
	1: "LOGOUT",
	2: "PLAYER",
	3: "LOGIN_RESULT",
}

var Type_value = map[string]int32{
	"LOGIN":        0,
	"LOGOUT":       1,
	"PLAYER":       2,
	"LOGIN_RESULT": 3,
}

func (x Type) String() string {
	return proto.EnumName(Type_name, int32(x))
}

func (Type) EnumDescriptor() ([]byte, []int) {
	return fileDescriptor_c06e4cca6c2cc899, []int{0}
}

type Msg struct {
	Type Type `protobuf:"varint,1,opt,name=type,proto3,enum=pb.Type" json:"type,omitempty"`
	// Types that are valid to be assigned to Union:
	//	*Msg_Player
	//	*Msg_Login
	//	*Msg_Logout
	//	*Msg_LoginResult
	Union                isMsg_Union `protobuf_oneof:"union"`
	XXX_NoUnkeyedLiteral struct{}    `json:"-"`
	XXX_unrecognized     []byte      `json:"-"`
	XXX_sizecache        int32       `json:"-"`
}

func (m *Msg) Reset()         { *m = Msg{} }
func (m *Msg) String() string { return proto.CompactTextString(m) }
func (*Msg) ProtoMessage()    {}
func (*Msg) Descriptor() ([]byte, []int) {
	return fileDescriptor_c06e4cca6c2cc899, []int{0}
}

func (m *Msg) XXX_Unmarshal(b []byte) error {
	return xxx_messageInfo_Msg.Unmarshal(m, b)
}
func (m *Msg) XXX_Marshal(b []byte, deterministic bool) ([]byte, error) {
	return xxx_messageInfo_Msg.Marshal(b, m, deterministic)
}
func (m *Msg) XXX_Merge(src proto.Message) {
	xxx_messageInfo_Msg.Merge(m, src)
}
func (m *Msg) XXX_Size() int {
	return xxx_messageInfo_Msg.Size(m)
}
func (m *Msg) XXX_DiscardUnknown() {
	xxx_messageInfo_Msg.DiscardUnknown(m)
}

var xxx_messageInfo_Msg proto.InternalMessageInfo

func (m *Msg) GetType() Type {
	if m != nil {
		return m.Type
	}
	return Type_LOGIN
}

type isMsg_Union interface {
	isMsg_Union()
}

type Msg_Player struct {
	Player *Player `protobuf:"bytes,2,opt,name=player,proto3,oneof"`
}

type Msg_Login struct {
	Login *Login `protobuf:"bytes,3,opt,name=login,proto3,oneof"`
}

type Msg_Logout struct {
	Logout *Logout `protobuf:"bytes,4,opt,name=logout,proto3,oneof"`
}

type Msg_LoginResult struct {
	LoginResult *LoginResult `protobuf:"bytes,5,opt,name=loginResult,proto3,oneof"`
}

func (*Msg_Player) isMsg_Union() {}

func (*Msg_Login) isMsg_Union() {}

func (*Msg_Logout) isMsg_Union() {}

func (*Msg_LoginResult) isMsg_Union() {}

func (m *Msg) GetUnion() isMsg_Union {
	if m != nil {
		return m.Union
	}
	return nil
}

func (m *Msg) GetPlayer() *Player {
	if x, ok := m.GetUnion().(*Msg_Player); ok {
		return x.Player
	}
	return nil
}

func (m *Msg) GetLogin() *Login {
	if x, ok := m.GetUnion().(*Msg_Login); ok {
		return x.Login
	}
	return nil
}

func (m *Msg) GetLogout() *Logout {
	if x, ok := m.GetUnion().(*Msg_Logout); ok {
		return x.Logout
	}
	return nil
}

func (m *Msg) GetLoginResult() *LoginResult {
	if x, ok := m.GetUnion().(*Msg_LoginResult); ok {
		return x.LoginResult
	}
	return nil
}

// XXX_OneofWrappers is for the internal use of the proto package.
func (*Msg) XXX_OneofWrappers() []interface{} {
	return []interface{}{
		(*Msg_Player)(nil),
		(*Msg_Login)(nil),
		(*Msg_Logout)(nil),
		(*Msg_LoginResult)(nil),
	}
}

type Login struct {
	Pid                  string   `protobuf:"bytes,1,opt,name=pid,proto3" json:"pid,omitempty"`
	Passwd               string   `protobuf:"bytes,2,opt,name=passwd,proto3" json:"passwd,omitempty"`
	XXX_NoUnkeyedLiteral struct{} `json:"-"`
	XXX_unrecognized     []byte   `json:"-"`
	XXX_sizecache        int32    `json:"-"`
}

func (m *Login) Reset()         { *m = Login{} }
func (m *Login) String() string { return proto.CompactTextString(m) }
func (*Login) ProtoMessage()    {}
func (*Login) Descriptor() ([]byte, []int) {
	return fileDescriptor_c06e4cca6c2cc899, []int{1}
}

func (m *Login) XXX_Unmarshal(b []byte) error {
	return xxx_messageInfo_Login.Unmarshal(m, b)
}
func (m *Login) XXX_Marshal(b []byte, deterministic bool) ([]byte, error) {
	return xxx_messageInfo_Login.Marshal(b, m, deterministic)
}
func (m *Login) XXX_Merge(src proto.Message) {
	xxx_messageInfo_Login.Merge(m, src)
}
func (m *Login) XXX_Size() int {
	return xxx_messageInfo_Login.Size(m)
}
func (m *Login) XXX_DiscardUnknown() {
	xxx_messageInfo_Login.DiscardUnknown(m)
}

var xxx_messageInfo_Login proto.InternalMessageInfo

func (m *Login) GetPid() string {
	if m != nil {
		return m.Pid
	}
	return ""
}

func (m *Login) GetPasswd() string {
	if m != nil {
		return m.Passwd
	}
	return ""
}

type LoginResult struct {
	Success              bool     `protobuf:"varint,1,opt,name=success,proto3" json:"success,omitempty"`
	Player               *Player  `protobuf:"bytes,2,opt,name=player,proto3" json:"player,omitempty"`
	XXX_NoUnkeyedLiteral struct{} `json:"-"`
	XXX_unrecognized     []byte   `json:"-"`
	XXX_sizecache        int32    `json:"-"`
}

func (m *LoginResult) Reset()         { *m = LoginResult{} }
func (m *LoginResult) String() string { return proto.CompactTextString(m) }
func (*LoginResult) ProtoMessage()    {}
func (*LoginResult) Descriptor() ([]byte, []int) {
	return fileDescriptor_c06e4cca6c2cc899, []int{2}
}

func (m *LoginResult) XXX_Unmarshal(b []byte) error {
	return xxx_messageInfo_LoginResult.Unmarshal(m, b)
}
func (m *LoginResult) XXX_Marshal(b []byte, deterministic bool) ([]byte, error) {
	return xxx_messageInfo_LoginResult.Marshal(b, m, deterministic)
}
func (m *LoginResult) XXX_Merge(src proto.Message) {
	xxx_messageInfo_LoginResult.Merge(m, src)
}
func (m *LoginResult) XXX_Size() int {
	return xxx_messageInfo_LoginResult.Size(m)
}
func (m *LoginResult) XXX_DiscardUnknown() {
	xxx_messageInfo_LoginResult.DiscardUnknown(m)
}

var xxx_messageInfo_LoginResult proto.InternalMessageInfo

func (m *LoginResult) GetSuccess() bool {
	if m != nil {
		return m.Success
	}
	return false
}

func (m *LoginResult) GetPlayer() *Player {
	if m != nil {
		return m.Player
	}
	return nil
}

type Logout struct {
	XXX_NoUnkeyedLiteral struct{} `json:"-"`
	XXX_unrecognized     []byte   `json:"-"`
	XXX_sizecache        int32    `json:"-"`
}

func (m *Logout) Reset()         { *m = Logout{} }
func (m *Logout) String() string { return proto.CompactTextString(m) }
func (*Logout) ProtoMessage()    {}
func (*Logout) Descriptor() ([]byte, []int) {
	return fileDescriptor_c06e4cca6c2cc899, []int{3}
}

func (m *Logout) XXX_Unmarshal(b []byte) error {
	return xxx_messageInfo_Logout.Unmarshal(m, b)
}
func (m *Logout) XXX_Marshal(b []byte, deterministic bool) ([]byte, error) {
	return xxx_messageInfo_Logout.Marshal(b, m, deterministic)
}
func (m *Logout) XXX_Merge(src proto.Message) {
	xxx_messageInfo_Logout.Merge(m, src)
}
func (m *Logout) XXX_Size() int {
	return xxx_messageInfo_Logout.Size(m)
}
func (m *Logout) XXX_DiscardUnknown() {
	xxx_messageInfo_Logout.DiscardUnknown(m)
}

var xxx_messageInfo_Logout proto.InternalMessageInfo

type Player struct {
	Pid                  string   `protobuf:"bytes,1,opt,name=pid,proto3" json:"pid,omitempty"`
	Passwd               string   `protobuf:"bytes,2,opt,name=passwd,proto3" json:"passwd,omitempty"`
	Age                  int32    `protobuf:"varint,3,opt,name=age,proto3" json:"age,omitempty"`
	XXX_NoUnkeyedLiteral struct{} `json:"-"`
	XXX_unrecognized     []byte   `json:"-"`
	XXX_sizecache        int32    `json:"-"`
}

func (m *Player) Reset()         { *m = Player{} }
func (m *Player) String() string { return proto.CompactTextString(m) }
func (*Player) ProtoMessage()    {}
func (*Player) Descriptor() ([]byte, []int) {
	return fileDescriptor_c06e4cca6c2cc899, []int{4}
}

func (m *Player) XXX_Unmarshal(b []byte) error {
	return xxx_messageInfo_Player.Unmarshal(m, b)
}
func (m *Player) XXX_Marshal(b []byte, deterministic bool) ([]byte, error) {
	return xxx_messageInfo_Player.Marshal(b, m, deterministic)
}
func (m *Player) XXX_Merge(src proto.Message) {
	xxx_messageInfo_Player.Merge(m, src)
}
func (m *Player) XXX_Size() int {
	return xxx_messageInfo_Player.Size(m)
}
func (m *Player) XXX_DiscardUnknown() {
	xxx_messageInfo_Player.DiscardUnknown(m)
}

var xxx_messageInfo_Player proto.InternalMessageInfo

func (m *Player) GetPid() string {
	if m != nil {
		return m.Pid
	}
	return ""
}

func (m *Player) GetPasswd() string {
	if m != nil {
		return m.Passwd
	}
	return ""
}

func (m *Player) GetAge() int32 {
	if m != nil {
		return m.Age
	}
	return 0
}

func init() {
	proto.RegisterEnum("pb.Type", Type_name, Type_value)
	proto.RegisterType((*Msg)(nil), "pb.Msg")
	proto.RegisterType((*Login)(nil), "pb.Login")
	proto.RegisterType((*LoginResult)(nil), "pb.LoginResult")
	proto.RegisterType((*Logout)(nil), "pb.Logout")
	proto.RegisterType((*Player)(nil), "pb.Player")
}

func init() { proto.RegisterFile("msg.proto", fileDescriptor_c06e4cca6c2cc899) }

var fileDescriptor_c06e4cca6c2cc899 = []byte{
	// 325 bytes of a gzipped FileDescriptorProto
	0x1f, 0x8b, 0x08, 0x00, 0x00, 0x00, 0x00, 0x00, 0x02, 0xff, 0x8c, 0x91, 0x41, 0x4f, 0xfa, 0x40,
	0x10, 0xc5, 0x5b, 0x4a, 0x97, 0x76, 0xf8, 0xe7, 0x6f, 0x33, 0x07, 0xd3, 0x83, 0x07, 0x6c, 0x3c,
	0x10, 0x0f, 0x24, 0xc2, 0xd1, 0x93, 0x04, 0x02, 0xc6, 0x2a, 0x64, 0x85, 0x83, 0x27, 0x53, 0x60,
	0xd3, 0x34, 0x29, 0xdd, 0x0d, 0xdb, 0xc6, 0xf4, 0x5b, 0xfa, 0x91, 0xcc, 0x4e, 0x41, 0xb9, 0x98,
	0x78, 0x9b, 0x79, 0xef, 0xb7, 0x2f, 0x3b, 0x33, 0xe0, 0xef, 0x75, 0x3a, 0x50, 0x07, 0x59, 0x4a,
	0x6c, 0xa9, 0x4d, 0xf4, 0x69, 0x83, 0xf3, 0xac, 0x53, 0xbc, 0x82, 0x76, 0x59, 0x2b, 0x11, 0xda,
	0x3d, 0xbb, 0xff, 0x7f, 0xe8, 0x0d, 0xd4, 0x66, 0xb0, 0xaa, 0x95, 0xe0, 0xa4, 0xe2, 0x0d, 0x30,
	0x95, 0x27, 0xb5, 0x38, 0x84, 0xad, 0x9e, 0xdd, 0xef, 0x0e, 0xc1, 0xf8, 0x4b, 0x52, 0xe6, 0x16,
	0x3f, 0x7a, 0x78, 0x0d, 0x6e, 0x2e, 0xd3, 0xac, 0x08, 0x1d, 0x82, 0x7c, 0x03, 0xc5, 0x46, 0x98,
	0x5b, 0xbc, 0x71, 0x4c, 0x50, 0x2e, 0x53, 0x59, 0x95, 0x61, 0xfb, 0x27, 0x28, 0x26, 0xc5, 0x04,
	0x35, 0x1e, 0x8e, 0xa0, 0x4b, 0x38, 0x17, 0xba, 0xca, 0xcb, 0xd0, 0x25, 0xf4, 0xe2, 0x3b, 0xae,
	0x91, 0xe7, 0x16, 0x3f, 0xa7, 0xc6, 0x1d, 0x70, 0xab, 0x22, 0x93, 0x45, 0x74, 0x07, 0x2e, 0x61,
	0x18, 0x80, 0xa3, 0xb2, 0x1d, 0x8d, 0xe4, 0x73, 0x53, 0xe2, 0x25, 0x30, 0x95, 0x68, 0xfd, 0xb1,
	0xa3, 0x39, 0x7c, 0x7e, 0xec, 0xa2, 0x27, 0xe8, 0x9e, 0x25, 0x63, 0x08, 0x1d, 0x5d, 0x6d, 0xb7,
	0x42, 0x6b, 0x7a, 0xec, 0xf1, 0x53, 0x8b, 0xd1, 0xef, 0x8b, 0x38, 0xad, 0x21, 0xf2, 0x80, 0x35,
	0x13, 0x45, 0x13, 0x60, 0x8d, 0xf7, 0xf7, 0xaf, 0x18, 0x32, 0x49, 0x05, 0xad, 0xd0, 0xe5, 0xa6,
	0xbc, 0xbd, 0x87, 0xb6, 0x39, 0x05, 0xfa, 0xe0, 0xc6, 0x8b, 0xd9, 0xe3, 0x4b, 0x60, 0x21, 0x00,
	0x8b, 0x17, 0xb3, 0xc5, 0x7a, 0x15, 0xd8, 0xa6, 0x5e, 0xc6, 0x0f, 0x6f, 0x53, 0x1e, 0xb4, 0x30,
	0x80, 0x7f, 0x84, 0xbc, 0xf3, 0xe9, 0xeb, 0x3a, 0x5e, 0x05, 0xce, 0xd8, 0x1b, 0xb3, 0x89, 0xdc,
	0x27, 0x59, 0xb1, 0x61, 0x74, 0xf4, 0xd1, 0x57, 0x00, 0x00, 0x00, 0xff, 0xff, 0x26, 0xe5, 0x4e,
	0x28, 0x01, 0x02, 0x00, 0x00,
}
