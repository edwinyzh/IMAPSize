{==============================================================================|
| Project : Ararat Synapse                                       | 004.003.000 |
|==============================================================================|
| Content: Socket Independent Platform Layer                                   |
|==============================================================================|
| Copyright (c)1999-2003, Lukas Gebauer                                        |
| All rights reserved.                                                         |
|                                                                              |
| Redistribution and use in source and binary forms, with or without           |
| modification, are permitted provided that the following conditions are met:  |
|                                                                              |
| Redistributions of source code must retain the above copyright notice, this  |
| list of conditions and the following disclaimer.                             |
|                                                                              |
| Redistributions in binary form must reproduce the above copyright notice,    |
| this list of conditions and the following disclaimer in the documentation    |
| and/or other materials provided with the distribution.                       |
|                                                                              |
| Neither the name of Lukas Gebauer nor the names of its contributors may      |
| be used to endorse or promote products derived from this software without    |
| specific prior written permission.                                           |
|                                                                              |
| THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"  |
| AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE    |
| IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE   |
| ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR  |
| ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL       |
| DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR   |
| SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER   |
| CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT           |
| LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY    |
| OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH  |
| DAMAGE.                                                                      |
|==============================================================================|
| The Initial Developer of the Original Code is Lukas Gebauer (Czech Republic).|
| Portions created by Lukas Gebauer are Copyright (c)2001-2003.                |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

{$IFNDEF LINUX}
//{$DEFINE WINSOCK1}
{Note about define WINSOCK1:
If you activate this compiler directive, then socket interface level 1.1 is
used instead default level 2.2. Level 2.2 is not available on old W95, however
you can install update.

On Linux is level 2.2 always used!
}
{$ENDIF}

//{$DEFINE FORCEOLDAPI}
{Note about define FORCEOLDAPI:
If you activate this compiler directive, then is allways used old socket API
for name resolution. If you leave this directive inactive, then the new API
is used, when running system allows it.

For IPv6 support you must have new API!
}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}
{$H+}
{$IFDEF VER125}
  {$DEFINE BCB}
{$ENDIF}
{$IFDEF BCB}
  {$ObjExportAll On}
  (*$HPPEMIT '/* EDE 2003-02-19 */' *)
  (*$HPPEMIT 'namespace Synsock { using System::Shortint; }' *)
  (*$HPPEMIT '#undef h_addr' *)
  (*$HPPEMIT '#undef IOCPARM_MASK' *)
  (*$HPPEMIT '#undef FD_SETSIZE' *)
  (*$HPPEMIT '#undef IOC_VOID' *)
  (*$HPPEMIT '#undef IOC_OUT' *)
  (*$HPPEMIT '#undef IOC_IN' *)
  (*$HPPEMIT '#undef IOC_INOUT' *)
  (*$HPPEMIT '#undef FIONREAD' *)
  (*$HPPEMIT '#undef FIONBIO' *)
  (*$HPPEMIT '#undef FIOASYNC' *)
  (*$HPPEMIT '#undef IPPROTO_IP' *)
  (*$HPPEMIT '#undef IPPROTO_ICMP' *)
  (*$HPPEMIT '#undef IPPROTO_IGMP' *)
  (*$HPPEMIT '#undef IPPROTO_TCP' *)
  (*$HPPEMIT '#undef IPPROTO_UDP' *)
  (*$HPPEMIT '#undef IPPROTO_RAW' *)
  (*$HPPEMIT '#undef IPPROTO_MAX' *)
  (*$HPPEMIT '#undef INADDR_ANY' *)
  (*$HPPEMIT '#undef INADDR_LOOPBACK' *)
  (*$HPPEMIT '#undef INADDR_BROADCAST' *)
  (*$HPPEMIT '#undef INADDR_NONE' *)
  (*$HPPEMIT '#undef INVALID_SOCKET' *)
  (*$HPPEMIT '#undef SOCKET_ERROR' *)
  (*$HPPEMIT '#undef WSADESCRIPTION_LEN' *)
  (*$HPPEMIT '#undef WSASYS_STATUS_LEN' *)
  (*$HPPEMIT '#undef IP_OPTIONS' *)
  (*$HPPEMIT '#undef IP_TOS' *)
  (*$HPPEMIT '#undef IP_TTL' *)
  (*$HPPEMIT '#undef IP_MULTICAST_IF' *)
  (*$HPPEMIT '#undef IP_MULTICAST_TTL' *)
  (*$HPPEMIT '#undef IP_MULTICAST_LOOP' *)
  (*$HPPEMIT '#undef IP_ADD_MEMBERSHIP' *)
  (*$HPPEMIT '#undef IP_DROP_MEMBERSHIP' *)
  (*$HPPEMIT '#undef IP_DONTFRAGMENT' *)
  (*$HPPEMIT '#undef IP_DEFAULT_MULTICAST_TTL' *)
  (*$HPPEMIT '#undef IP_DEFAULT_MULTICAST_LOOP' *)
  (*$HPPEMIT '#undef IP_MAX_MEMBERSHIPS' *)
  (*$HPPEMIT '#undef SOL_SOCKET' *)
  (*$HPPEMIT '#undef SO_DEBUG' *)
  (*$HPPEMIT '#undef SO_ACCEPTCONN' *)
  (*$HPPEMIT '#undef SO_REUSEADDR' *)
  (*$HPPEMIT '#undef SO_KEEPALIVE' *)
  (*$HPPEMIT '#undef SO_DONTROUTE' *)
  (*$HPPEMIT '#undef SO_BROADCAST' *)
  (*$HPPEMIT '#undef SO_USELOOPBACK' *)
  (*$HPPEMIT '#undef SO_LINGER' *)
  (*$HPPEMIT '#undef SO_OOBINLINE' *)
  (*$HPPEMIT '#undef SO_DONTLINGER' *)
  (*$HPPEMIT '#undef SO_SNDBUF' *)
  (*$HPPEMIT '#undef SO_RCVBUF' *)
  (*$HPPEMIT '#undef SO_SNDLOWAT' *)
  (*$HPPEMIT '#undef SO_RCVLOWAT' *)
  (*$HPPEMIT '#undef SO_SNDTIMEO' *)
  (*$HPPEMIT '#undef SO_RCVTIMEO' *)
  (*$HPPEMIT '#undef SO_ERROR' *)
  (*$HPPEMIT '#undef SO_OPENTYPE' *)
  (*$HPPEMIT '#undef SO_SYNCHRONOUS_ALERT' *)
  (*$HPPEMIT '#undef SO_SYNCHRONOUS_NONALERT' *)
  (*$HPPEMIT '#undef SO_MAXDG' *)
  (*$HPPEMIT '#undef SO_MAXPATHDG' *)
  (*$HPPEMIT '#undef SO_UPDATE_ACCEPT_CONTEXT' *)
  (*$HPPEMIT '#undef SO_CONNECT_TIME' *)
  (*$HPPEMIT '#undef SO_TYPE' *)
  (*$HPPEMIT '#undef SOCK_STREAM' *)
  (*$HPPEMIT '#undef SOCK_DGRAM' *)
  (*$HPPEMIT '#undef SOCK_RAW' *)
  (*$HPPEMIT '#undef SOCK_RDM' *)
  (*$HPPEMIT '#undef SOCK_SEQPACKET' *)
  (*$HPPEMIT '#undef TCP_NODELAY' *)
  (*$HPPEMIT '#undef AF_UNSPEC' *)
  (*$HPPEMIT '#undef SOMAXCONN' *)
  (*$HPPEMIT '#undef AF_INET' *)
  (*$HPPEMIT '#undef AF_MAX' *)
  (*$HPPEMIT '#undef PF_UNSPEC' *)
  (*$HPPEMIT '#undef PF_INET' *)
  (*$HPPEMIT '#undef PF_MAX' *)
  (*$HPPEMIT '#undef MSG_OOB' *)
  (*$HPPEMIT '#undef MSG_PEEK' *)
  (*$HPPEMIT '#undef WSABASEERR' *)
  (*$HPPEMIT '#undef WSAEINTR' *)
  (*$HPPEMIT '#undef WSAEBADF' *)
  (*$HPPEMIT '#undef WSAEACCES' *)
  (*$HPPEMIT '#undef WSAEFAULT' *)
  (*$HPPEMIT '#undef WSAEINVAL' *)
  (*$HPPEMIT '#undef WSAEMFILE' *)
  (*$HPPEMIT '#undef WSAEWOULDBLOCK' *)
  (*$HPPEMIT '#undef WSAEINPROGRESS' *)
  (*$HPPEMIT '#undef WSAEALREADY' *)
  (*$HPPEMIT '#undef WSAENOTSOCK' *)
  (*$HPPEMIT '#undef WSAEDESTADDRREQ' *)
  (*$HPPEMIT '#undef WSAEMSGSIZE' *)
  (*$HPPEMIT '#undef WSAEPROTOTYPE' *)
  (*$HPPEMIT '#undef WSAENOPROTOOPT' *)
  (*$HPPEMIT '#undef WSAEPROTONOSUPPORT' *)
  (*$HPPEMIT '#undef WSAESOCKTNOSUPPORT' *)
  (*$HPPEMIT '#undef WSAEOPNOTSUPP' *)
  (*$HPPEMIT '#undef WSAEPFNOSUPPORT' *)
  (*$HPPEMIT '#undef WSAEAFNOSUPPORT' *)
  (*$HPPEMIT '#undef WSAEADDRINUSE' *)
  (*$HPPEMIT '#undef WSAEADDRNOTAVAIL' *)
  (*$HPPEMIT '#undef WSAENETDOWN' *)
  (*$HPPEMIT '#undef WSAENETUNREACH' *)
  (*$HPPEMIT '#undef WSAENETRESET' *)
  (*$HPPEMIT '#undef WSAECONNABORTED' *)
  (*$HPPEMIT '#undef WSAECONNRESET' *)
  (*$HPPEMIT '#undef WSAENOBUFS' *)
  (*$HPPEMIT '#undef WSAEISCONN' *)
  (*$HPPEMIT '#undef WSAENOTCONN' *)
  (*$HPPEMIT '#undef WSAESHUTDOWN' *)
  (*$HPPEMIT '#undef WSAETOOMANYREFS' *)
  (*$HPPEMIT '#undef WSAETIMEDOUT' *)
  (*$HPPEMIT '#undef WSAECONNREFUSED' *)
  (*$HPPEMIT '#undef WSAELOOP' *)
  (*$HPPEMIT '#undef WSAENAMETOOLONG' *)
  (*$HPPEMIT '#undef WSAEHOSTDOWN' *)
  (*$HPPEMIT '#undef WSAEHOSTUNREACH' *)
  (*$HPPEMIT '#undef WSAENOTEMPTY' *)
  (*$HPPEMIT '#undef WSAEPROCLIM' *)
  (*$HPPEMIT '#undef WSAEUSERS' *)
  (*$HPPEMIT '#undef WSAEDQUOT' *)
  (*$HPPEMIT '#undef WSAESTALE' *)
  (*$HPPEMIT '#undef WSAEREMOTE' *)
  (*$HPPEMIT '#undef WSASYSNOTREADY' *)
  (*$HPPEMIT '#undef WSAVERNOTSUPPORTED' *)
  (*$HPPEMIT '#undef WSANOTINITIALISED' *)
  (*$HPPEMIT '#undef WSAEDISCON' *)
  (*$HPPEMIT '#undef WSAENOMORE' *)
  (*$HPPEMIT '#undef WSAECANCELLED' *)
  (*$HPPEMIT '#undef WSAEEINVALIDPROCTABLE' *)
  (*$HPPEMIT '#undef WSAEINVALIDPROVIDER' *)
  (*$HPPEMIT '#undef WSAEPROVIDERFAILEDINIT' *)
  (*$HPPEMIT '#undef WSASYSCALLFAILURE' *)
  (*$HPPEMIT '#undef WSASERVICE_NOT_FOUND' *)
  (*$HPPEMIT '#undef WSATYPE_NOT_FOUND' *)
  (*$HPPEMIT '#undef WSA_E_NO_MORE' *)
  (*$HPPEMIT '#undef WSA_E_CANCELLED' *)
  (*$HPPEMIT '#undef WSAEREFUSED' *)
  (*$HPPEMIT '#undef WSAHOST_NOT_FOUND' *)
  (*$HPPEMIT '#undef HOST_NOT_FOUND' *)
  (*$HPPEMIT '#undef WSATRY_AGAIN' *)
  (*$HPPEMIT '#undef TRY_AGAIN' *)
  (*$HPPEMIT '#undef WSANO_RECOVERY' *)
  (*$HPPEMIT '#undef NO_RECOVERY' *)
  (*$HPPEMIT '#undef WSANO_DATA' *)
  (*$HPPEMIT '#undef NO_DATA' *)
  (*$HPPEMIT '#undef WSANO_ADDRESS' *)
  (*$HPPEMIT '#undef ENAMETOOLONG' *)
  (*$HPPEMIT '#undef ENOTEMPTY' *)
  (*$HPPEMIT '#undef FD_CLR' *)
  (*$HPPEMIT '#undef FD_ISSET' *)
  (*$HPPEMIT '#undef FD_SET' *)
  (*$HPPEMIT '#undef FD_ZERO' *)
  (*$HPPEMIT '#undef NO_ADDRESS' *)
  (*$HPPEMIT '#undef ADDR_ANY' *)
  (*$HPPEMIT '#undef SO_GROUP_ID' *)
  (*$HPPEMIT '#undef SO_GROUP_PRIORITY' *)
  (*$HPPEMIT '#undef SO_MAX_MSG_SIZE' *)
  (*$HPPEMIT '#undef SO_PROTOCOL_INFOA' *)
  (*$HPPEMIT '#undef SO_PROTOCOL_INFOW' *)
  (*$HPPEMIT '#undef SO_PROTOCOL_INFO' *)
  (*$HPPEMIT '#undef PVD_CONFIG' *)
  (*$HPPEMIT '#undef AF_INET6' *)
  (*$HPPEMIT '#undef PF_INET6' *)
{$ENDIF}


unit synsock;

{$MINENUMSIZE 4}

interface

uses
  SyncObjs, SysUtils,
{$IFDEF LINUX}
  {$IFDEF FPC}
  synafpc,
  {$ENDIF}
  Libc;
{$ELSE}
  Windows;
{$ENDIF}

function InitSocketInterface(stack: string): Boolean;
function DestroySocketInterface: Boolean;

const
{$IFDEF WINSOCK1}
  WinsockLevel = $0101;
{$ELSE}
  WinsockLevel = $0202;
{$ENDIF}

type
  u_char = Char;
  u_short = Word;
  u_int = Integer;
  u_long = Longint;
  pu_long = ^u_long;
  pu_short = ^u_short;
  TSocket = u_int;

{$IFDEF LINUX}
  {$I sslinux.pas}
{$ELSE}
  {$I sswin32.pas}
{$ENDIF}

const
  WSADESCRIPTION_LEN     =   256;
  WSASYS_STATUS_LEN      =   128;
type
  PWSAData = ^TWSAData;
  TWSAData = packed record
    wVersion: Word;
    wHighVersion: Word;
    szDescription: array[0..WSADESCRIPTION_LEN] of Char;
    szSystemStatus: array[0..WSASYS_STATUS_LEN] of Char;
    iMaxSockets: Word;
    iMaxUdpDg: Word;
    lpVendorInfo: PChar;
  end;

  function IN6_IS_ADDR_UNSPECIFIED(const a: PInAddr6): boolean;
  function IN6_IS_ADDR_LOOPBACK(const a: PInAddr6): boolean;
  function IN6_IS_ADDR_LINKLOCAL(const a: PInAddr6): boolean;
  function IN6_IS_ADDR_SITELOCAL(const a: PInAddr6): boolean;
  function IN6_IS_ADDR_MULTICAST(const a: PInAddr6): boolean;
  function IN6_ADDR_EQUAL(const a: PInAddr6; const b: PInAddr6):boolean;
  procedure SET_IN6_IF_ADDR_ANY (const a: PInAddr6);
  procedure SET_LOOPBACK_ADDR6 (const a: PInAddr6);
var
  in6addr_any, in6addr_loopback : TInAddr6;

procedure FD_CLR(Socket: TSocket; var FDSet: TFDSet);
function FD_ISSET(Socket: TSocket; var FDSet: TFDSet): Boolean;
procedure FD_SET(Socket: TSocket; var FDSet: TFDSet);
procedure FD_ZERO(var FDSet: TFDSet);

{=============================================================================}

type
  TWSAStartup = function(wVersionRequired: Word; var WSData: TWSAData): Integer;
    {$IFDEF LINUX}cdecl{$ELSE}stdcall{$ENDIF};
  TWSACleanup = function: Integer;
    {$IFDEF LINUX}cdecl{$ELSE}stdcall{$ENDIF};
  TWSAGetLastError = function: Integer;
    {$IFDEF LINUX}cdecl{$ELSE}stdcall{$ENDIF};
  TGetServByName = function(name, proto: PChar): PServEnt;
    {$IFDEF LINUX}cdecl{$ELSE}stdcall{$ENDIF};
  TGetServByPort = function(port: Integer; proto: PChar): PServEnt;
    {$IFDEF LINUX}cdecl{$ELSE}stdcall{$ENDIF};
  TGetProtoByName = function(name: PChar): PProtoEnt;
    {$IFDEF LINUX}cdecl{$ELSE}stdcall{$ENDIF};
  TGetProtoByNumber = function(proto: Integer): PProtoEnt;
    {$IFDEF LINUX}cdecl{$ELSE}stdcall{$ENDIF};
  TGetHostByName = function(name: PChar): PHostEnt;
    {$IFDEF LINUX}cdecl{$ELSE}stdcall{$ENDIF};
  TGetHostByAddr = function(addr: Pointer; len, Struc: Integer): PHostEnt;
    {$IFDEF LINUX}cdecl{$ELSE}stdcall{$ENDIF};
  TGetHostName = function(name: PChar; len: Integer): Integer;
    {$IFDEF LINUX}cdecl{$ELSE}stdcall{$ENDIF};
  TShutdown = function(s: TSocket; how: Integer): Integer;
    {$IFDEF LINUX}cdecl{$ELSE}stdcall{$ENDIF};
  TSetSockOpt = function(s: TSocket; level, optname: Integer; optval: PChar;
    optlen: Integer): Integer;
    {$IFDEF LINUX}cdecl{$ELSE}stdcall{$ENDIF};
  TGetSockOpt = function(s: TSocket; level, optname: Integer; optval: PChar;
    var optlen: Integer): Integer;
    {$IFDEF LINUX}cdecl{$ELSE}stdcall{$ENDIF};
  TSendTo = function(s: TSocket; const Buf; len, flags: Integer; addrto: PSockAddr;
    tolen: Integer): Integer;
    {$IFDEF LINUX}cdecl{$ELSE}stdcall{$ENDIF};
  TSend = function(s: TSocket; const Buf; len, flags: Integer): Integer;
    {$IFDEF LINUX}cdecl{$ELSE}stdcall{$ENDIF};
  TRecv = function(s: TSocket; var Buf; len, flags: Integer): Integer;
    {$IFDEF LINUX}cdecl{$ELSE}stdcall{$ENDIF};
  TRecvFrom = function(s: TSocket; var Buf; len, flags: Integer; from: PSockAddr;
    var fromlen: Integer): Integer;
    {$IFDEF LINUX}cdecl{$ELSE}stdcall{$ENDIF};
  Tntohs = function(netshort: u_short): u_short;
    {$IFDEF LINUX}cdecl{$ELSE}stdcall{$ENDIF};
  Tntohl = function(netlong: u_long): u_long;
    {$IFDEF LINUX}cdecl{$ELSE}stdcall{$ENDIF};
  TListen = function(s: TSocket; backlog: Integer): Integer;
    {$IFDEF LINUX}cdecl{$ELSE}stdcall{$ENDIF};
  TIoctlSocket = function(s: TSocket; cmd: DWORD; var arg: u_long): Integer;
    {$IFDEF LINUX}cdecl{$ELSE}stdcall{$ENDIF};
  TInet_ntoa = function(inaddr: TInAddr): PChar;
    {$IFDEF LINUX}cdecl{$ELSE}stdcall{$ENDIF};
  TInet_addr = function(cp: PChar): u_long;
    {$IFDEF LINUX}cdecl{$ELSE}stdcall{$ENDIF};
  Thtons = function(hostshort: u_short): u_short;
    {$IFDEF LINUX}cdecl{$ELSE}stdcall{$ENDIF};
  Thtonl = function(hostlong: u_long): u_long;
    {$IFDEF LINUX}cdecl{$ELSE}stdcall{$ENDIF};
  TGetSockName = function(s: TSocket; name: PSockAddr; var namelen: Integer): Integer;
    {$IFDEF LINUX}cdecl{$ELSE}stdcall{$ENDIF};
  TGetPeerName = function(s: TSocket; name: PSockAddr; var namelen: Integer): Integer;
    {$IFDEF LINUX}cdecl{$ELSE}stdcall{$ENDIF};
  TConnect = function(s: TSocket; name: PSockAddr; namelen: Integer): Integer;
    {$IFDEF LINUX}cdecl{$ELSE}stdcall{$ENDIF};
  TCloseSocket = function(s: TSocket): Integer;
    {$IFDEF LINUX}cdecl{$ELSE}stdcall{$ENDIF};
  TBind = function(s: TSocket; addr: PSockAddr; namelen: Integer): Integer;
    {$IFDEF LINUX}cdecl{$ELSE}stdcall{$ENDIF};
  TAccept = function(s: TSocket; addr: PSockAddr; var addrlen: Integer): TSocket;
    {$IFDEF LINUX}cdecl{$ELSE}stdcall{$ENDIF};
  TTSocket = function(af, Struc, Protocol: Integer): TSocket;
    {$IFDEF LINUX}cdecl{$ELSE}stdcall{$ENDIF};
  TSelect = function(nfds: Integer; readfds, writefds, exceptfds: PFDSet;
    timeout: PTimeVal): Longint;
    {$IFDEF LINUX}cdecl{$ELSE}stdcall{$ENDIF};

  TGetAddrInfo = function(NodeName: PChar; ServName: PChar; Hints: PAddrInfo;
    var Addrinfo: PAddrInfo): integer;
    {$IFDEF LINUX}cdecl{$ELSE}stdcall{$ENDIF};
  TFreeAddrInfo = procedure(ai: PAddrInfo);
    {$IFDEF LINUX}cdecl{$ELSE}stdcall{$ENDIF};
  TGetNameInfo = function( addr: PSockAddr; namelen: Integer; host: PChar;
    hostlen: DWORD; serv: PChar; servlen: DWORD; flags: integer): integer;
    {$IFDEF LINUX}cdecl{$ELSE}stdcall{$ENDIF};

{$IFNDEF LINUX}
  T__WSAFDIsSet = function (s: TSocket; var FDSet: TFDSet): Bool;
    stdcall;

  TWSAIoctl = function (s: TSocket; dwIoControlCode: DWORD; lpvInBuffer: Pointer;
    cbInBuffer: DWORD; lpvOutBuffer: Pointer; cbOutBuffer: DWORD;
    lpcbBytesReturned: PDWORD; lpOverlapped: Pointer;
    lpCompletionRoutine: pointer): u_int;
    stdcall;
{$ENDIF}


var
  WSAStartup: TWSAStartup = nil;
  WSACleanup: TWSACleanup = nil;
  WSAGetLastError: TWSAGetLastError = nil;
  GetServByName: TGetServByName = nil;
  GetServByPort: TGetServByPort = nil;
  GetProtoByName: TGetProtoByName = nil;
  GetProtoByNumber: TGetProtoByNumber = nil;
  GetHostByName: TGetHostByName = nil;
  GetHostByAddr: TGetHostByAddr = nil;
  GetHostName: TGetHostName = nil;
  Shutdown: TShutdown = nil;
  SetSockOpt: TSetSockOpt = nil;
  GetSockOpt: TGetSockOpt = nil;
  SendTo: TSendTo = nil;
  Send: TSend = nil;
  Recv: TRecv = nil;
  RecvFrom: TRecvFrom = nil;
  ntohs: Tntohs = nil;
  ntohl: Tntohl = nil;
  Listen: TListen = nil;
  IoctlSocket: TIoctlSocket = nil;
  Inet_ntoa: TInet_ntoa = nil;
  Inet_addr: TInet_addr = nil;
  htons: Thtons = nil;
  htonl: Thtonl = nil;
  GetSockName: TGetSockName = nil;
  GetPeerName: TGetPeerName = nil;
  Connect: TConnect = nil;
  CloseSocket: TCloseSocket = nil;
  Bind: TBind = nil;
  Accept: TAccept = nil;
  Socket: TTSocket = nil;
  Select: TSelect = nil;

  GetAddrInfo: TGetAddrInfo = nil;
  FreeAddrInfo: TFreeAddrInfo = nil;
  GetNameInfo: TGetNameInfo = nil;

{$IFNDEF LINUX}
  __WSAFDIsSet: T__WSAFDIsSet = nil;

  WSAIoctl: TWSAIoctl = nil;
{$ENDIF}

{$IFDEF LINUX}
function LSWSAStartup(wVersionRequired: Word; var WSData: TWSAData): Integer; cdecl;
function LSWSACleanup: Integer; cdecl;
function LSWSAGetLastError: Integer; cdecl;
{$ENDIF}

var
  SynSockCS: SyncObjs.TCriticalSection;
  SockEnhancedApi: Boolean;
  SockWship6Api: Boolean;

type
  TVarSin = packed record
    case sin_family: u_short of
      AF_INET: (sin_port: u_short;
                sin_addr: TInAddr;
                sin_zero: array[0..7] of Char);
      AF_INET6: (sin6_port:     u_short;
            		sin6_flowinfo: u_long;
      		      sin6_addr:     TInAddr6;
      		      sin6_scope_id: u_long);
  end;

function SizeOfVarSin(sin: TVarSin): integer;

{==============================================================================}
implementation

var
  SynSockCount: Integer = 0;
  LibHandle: THandle = 0;
  Libwship6Handle: THandle = 0;

function IN6_IS_ADDR_UNSPECIFIED(const a: PInAddr6): boolean;
begin
  Result := ((a^.s_un_dw.s_dw1 = 0) and (a^.s_un_dw.s_dw2 = 0) and
             (a^.s_un_dw.s_dw3 = 0) and (a^.s_un_dw.s_dw4 = 0));
end;

function IN6_IS_ADDR_LOOPBACK(const a: PInAddr6): boolean;
begin
  Result := ((a^.s_un_dw.s_dw1 = 0) and (a^.s_un_dw.s_dw2 = 0) and
             (a^.s_un_dw.s_dw3 = 0) and
             (a^.s_un_b.s_b13 = char(0)) and (a^.s_un_b.s_b14 = char(0)) and
             (a^.s_un_b.s_b15 = char(0)) and (a^.s_un_b.s_b16 = char(1)));
end;

function IN6_IS_ADDR_LINKLOCAL(const a: PInAddr6): boolean;
begin
  Result := ((a^.s_un_b.s_b1 = u_char($FE)) and (a^.s_un_b.s_b2 = u_char($80)));
end;

function IN6_IS_ADDR_SITELOCAL(const a: PInAddr6): boolean;
begin
  Result := ((a^.s_un_b.s_b1 = u_char($FE)) and (a^.s_un_b.s_b2 = u_char($C0)));
end;

function IN6_IS_ADDR_MULTICAST(const a: PInAddr6): boolean;
begin
  Result := (a^.s_un_b.s_b1 = char($FF));
end;

function IN6_ADDR_EQUAL(const a: PInAddr6; const b: PInAddr6): boolean;
begin
  Result := (CompareMem( a, b, sizeof(TInAddr6)));
end;

procedure SET_IN6_IF_ADDR_ANY (const a: PInAddr6);
begin
  FillChar(a^, sizeof(TInAddr6), 0);
end;

procedure SET_LOOPBACK_ADDR6 (const a: PInAddr6);
begin
  FillChar(a^, sizeof(TInAddr6), 0);
  a^.s_un_b.s_b16 := char(1);
end;

{=============================================================================}
{$IFDEF LINUX}
var
{$IFNDEF FPC}
  errno_loc: function: PInteger cdecl = nil;
{$ELSE}
  errno_loc: function: PInteger = nil; cdecl;
{$ENDIF}

function LSWSAStartup(wVersionRequired: Word; var WSData: TWSAData): Integer;
begin
  with WSData do
  begin
    wVersion := wVersionRequired;
    wHighVersion := $202;
    szDescription := 'Synsock - Synapse Platform Independent Socket Layer';
    szSystemStatus := 'Running on Linux';
    iMaxSockets := 32768;
    iMaxUdpDg := 8192;
  end;
  Result := 0;
end;

function LSWSACleanup: Integer;
begin
  Result := 0;
end;

function LSWSAGetLastError: Integer;
var
  p: PInteger;
begin
  p := errno_loc;
  Result := p^;
end;

function __FDELT(Socket: TSocket): Integer;
begin
  Result := Socket div __NFDBITS;
end;

function __FDMASK(Socket: TSocket): __fd_mask;
begin
  Result := 1 shl (Socket mod __NFDBITS);
end;

function FD_ISSET(Socket: TSocket; var fdset: TFDSet): Boolean;
begin
  Result := (fdset.fds_bits[__FDELT(Socket)] and __FDMASK(Socket)) <> 0;
end;

procedure FD_SET(Socket: TSocket; var fdset: TFDSet);
begin
  fdset.fds_bits[__FDELT(Socket)] := fdset.fds_bits[__FDELT(Socket)] or __FDMASK(Socket);
end;

procedure FD_CLR(Socket: TSocket; var fdset: TFDSet);
begin
  fdset.fds_bits[__FDELT(Socket)] := fdset.fds_bits[__FDELT(Socket)] and (not __FDMASK(Socket));
end;

procedure FD_ZERO(var fdset: TFDSet);
var
  I: Integer;
begin
  with fdset do
    for I := Low(fds_bits) to High(fds_bits) do
      fds_bits[I] := 0;
end;

{=============================================================================}
{$ELSE}
procedure FD_CLR(Socket: TSocket; var FDSet: TFDSet);
var
  I: Integer;
begin
  I := 0;
  while I < FDSet.fd_count do
  begin
    if FDSet.fd_array[I] = Socket then
    begin
      while I < FDSet.fd_count - 1 do
      begin
        FDSet.fd_array[I] := FDSet.fd_array[I + 1];
        Inc(I);
      end;
      Dec(FDSet.fd_count);
      Break;
    end;
    Inc(I);
  end;
end;

function FD_ISSET(Socket: TSocket; var FDSet: TFDSet): Boolean;
begin
  Result := __WSAFDIsSet(Socket, FDSet);
end;

procedure FD_SET(Socket: TSocket; var FDSet: TFDSet);
begin
  if FDSet.fd_count < FD_SETSIZE then
  begin
    FDSet.fd_array[FDSet.fd_count] := Socket;
    Inc(FDSet.fd_count);
  end;
end;

procedure FD_ZERO(var FDSet: TFDSet);
begin
  FDSet.fd_count := 0;
end;
{$ENDIF}

{=============================================================================}

function SizeOfVarSin(sin: TVarSin): integer;
begin
  case sin.sin_family of
    AF_INET:
            Result := SizeOf(TSockAddrIn);
    AF_INET6:
            Result := SizeOf(TSockAddrIn6);
  else
    Result := 0;
  end;
end;
{=============================================================================}

function InitSocketInterface(stack: string): Boolean;
begin
  Result := False;
  SockEnhancedApi := False;
  if stack = '' then
    stack := DLLStackName;
  SynSockCS.Enter;
  try
    if SynSockCount = 0 then
    begin
      SockEnhancedApi := False;
      SockWship6Api := False;
{$IFDEF LINUX}
      Libc.Signal(Libc.SIGPIPE, TSignalHandler(Libc.SIG_IGN));
{$ENDIF}
      LibHandle := LoadLibrary(PChar(Stack));
      if LibHandle <> 0 then
      begin
{$IFDEF LINUX}
        errno_loc := GetProcAddress(LibHandle, PChar('__errno_location'));
        CloseSocket := GetProcAddress(LibHandle, PChar('close'));
        IoctlSocket := GetProcAddress(LibHandle, PChar('ioctl'));
        WSAGetLastError := LSWSAGetLastError;
        WSAStartup := LSWSAStartup;
        WSACleanup := LSWSACleanup;
{$ELSE}
        WSAIoctl := GetProcAddress(LibHandle, PChar('WSAIoctl'));
        __WSAFDIsSet := GetProcAddress(LibHandle, PChar('__WSAFDIsSet'));
        CloseSocket := GetProcAddress(LibHandle, PChar('closesocket'));
        IoctlSocket := GetProcAddress(LibHandle, PChar('ioctlsocket'));
        WSAGetLastError := GetProcAddress(LibHandle, PChar('WSAGetLastError'));
        WSAStartup := GetProcAddress(LibHandle, PChar('WSAStartup'));
        WSACleanup := GetProcAddress(LibHandle, PChar('WSACleanup'));
{$ENDIF}
        Accept := GetProcAddress(LibHandle, PChar('accept'));
        Bind := GetProcAddress(LibHandle, PChar('bind'));
        Connect := GetProcAddress(LibHandle, PChar('connect'));
        GetPeerName := GetProcAddress(LibHandle, PChar('getpeername'));
        GetSockName := GetProcAddress(LibHandle, PChar('getsockname'));
        GetSockOpt := GetProcAddress(LibHandle, PChar('getsockopt'));
        Htonl := GetProcAddress(LibHandle, PChar('htonl'));
        Htons := GetProcAddress(LibHandle, PChar('htons'));
        Inet_Addr := GetProcAddress(LibHandle, PChar('inet_addr'));
        Inet_Ntoa := GetProcAddress(LibHandle, PChar('inet_ntoa'));
        Listen := GetProcAddress(LibHandle, PChar('listen'));
        Ntohl := GetProcAddress(LibHandle, PChar('ntohl'));
        Ntohs := GetProcAddress(LibHandle, PChar('ntohs'));
        Recv := GetProcAddress(LibHandle, PChar('recv'));
        RecvFrom := GetProcAddress(LibHandle, PChar('recvfrom'));
        Select := GetProcAddress(LibHandle, PChar('select'));
        Send := GetProcAddress(LibHandle, PChar('send'));
        SendTo := GetProcAddress(LibHandle, PChar('sendto'));
        SetSockOpt := GetProcAddress(LibHandle, PChar('setsockopt'));
        ShutDown := GetProcAddress(LibHandle, PChar('shutdown'));
        Socket := GetProcAddress(LibHandle, PChar('socket'));
        GetHostByAddr := GetProcAddress(LibHandle, PChar('gethostbyaddr'));
        GetHostByName := GetProcAddress(LibHandle, PChar('gethostbyname'));
        GetProtoByName := GetProcAddress(LibHandle, PChar('getprotobyname'));
        GetProtoByNumber := GetProcAddress(LibHandle, PChar('getprotobynumber'));
        GetServByName := GetProcAddress(LibHandle, PChar('getservbyname'));
        GetServByPort := GetProcAddress(LibHandle, PChar('getservbyport'));
        GetHostName := GetProcAddress(LibHandle, PChar('gethostname'));

{$IFNDEF FORCEOLDAPI}
        GetAddrInfo := GetProcAddress(LibHandle, PChar('getaddrinfo'));
        FreeAddrInfo := GetProcAddress(LibHandle, PChar('freeaddrinfo'));
        GetNameInfo := GetProcAddress(LibHandle, PChar('getnameinfo'));
        SockEnhancedApi := Assigned(GetAddrInfo) and Assigned(FreeAddrInfo)
          and Assigned(GetNameInfo);
  {$IFNDEF LINUX}
        if not SockEnhancedApi then
        begin
          LibWship6Handle := LoadLibrary(PChar(DLLWship6));
          if LibWship6Handle <> 0 then
          begin
            GetAddrInfo := GetProcAddress(LibWship6Handle, PChar('getaddrinfo'));
            FreeAddrInfo := GetProcAddress(LibWship6Handle, PChar('freeaddrinfo'));
            GetNameInfo := GetProcAddress(LibWship6Handle, PChar('getnameinfo'));
            SockWship6Api := Assigned(GetAddrInfo) and Assigned(FreeAddrInfo)
              and Assigned(GetNameInfo);
          end;
        end;
  {$ENDIF}
{$ENDIF}
        Result := True;
      end;
    end
    else Result := True;
    if Result then
      Inc(SynSockCount);
  finally
    SynSockCS.Leave;
  end;
end;

function DestroySocketInterface: Boolean;
begin
  SynSockCS.Enter;
  try
    Dec(SynSockCount);
    if SynSockCount < 0 then
      SynSockCount := 0;
    if SynSockCount = 0 then
    begin
      if LibHandle <> 0 then
      begin
        FreeLibrary(libHandle);
        LibHandle := 0;
      end;
      if LibWship6Handle <> 0 then
      begin
        FreeLibrary(LibWship6Handle);
        LibWship6Handle := 0;
      end;
    end;
  finally
    SynSockCS.Leave;
  end;
  Result := True;
end;

initialization
begin
  SynSockCS := SyncObjs.TCriticalSection.Create;
  SET_IN6_IF_ADDR_ANY (@in6addr_any);
  SET_LOOPBACK_ADDR6  (@in6addr_loopback);
end;

finalization
begin
  SynSockCS.Free;
end;

end.
