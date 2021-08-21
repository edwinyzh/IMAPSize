{==============================================================================|
| Project : IMAPSize                                                           |
|==============================================================================|
| $Id: IMAPConnection.pas,v 1.10 2004/03/31 23:27:37 Ivan Exp $
|==============================================================================|
| Copyright 2003-2004 Ivan Vecanski                                            |
| =============================================================================}

unit IMAPConnection;

interface

uses Accounts, imapsend, classes, dialogs, sysutils, VecLog, EncryptIt, Messages,
     windows, forms, Log, GlobalConstants, GetConnectionResponseInfo, PasswordDlg;

const
    NOOP_TIMEOUT = 1000;   // timeout used for very quick response (NOOP operation only)
    // SOCK_TIMEOUT = 5000;   // timeout used in normal operations (@deprecated, see mSocketTimeout)
    LONG_TIMEOUT = 60000;  // timeout used in lengthy operations

type

TLoginResult = (lrHostNotFound, lrWrongUserPass, lrSuccess, lrTerminated, lrDontDisplayResult, lrAlreadyLoggedIn, lrLoginOnly);
// lrDontDisplayResult is passed only when the client requested that a message not be sent.
// We need a message to be sent in order to let the main thread know of the termination of the login thread

PIMAPConnection = ^TIMAPConnection;

{ Class that stores connection information. Used for sharing
  connection information between TIMAPConnection and TLoginThread }
TConnInfo = class
private

public
    imap: TIMAPSend;
    account: TAccountInfo;
    isLoggedIn: Boolean;       // Internal flag that signals if we are logged in,
                               // since NOOP can (in certain situations) return OK
                               // in an unauthenticated state...
    isLoggingIn: Boolean;      // True only while logging in...
    isBusy: Boolean;           // true if the connection is busy and can't be used by other threads...

    mSendMessage: Boolean;    // Flag whether to send a login status message or not
    imapConnRef: PIMAPConnection;  // Reference to the holding IMAPConnection object
    constructor Create;
    destructor Destroy; override;
    procedure HandleLoginOver(success: TLoginResult);
end;

TLoginThread = class(TThread)
private
    mConnInfo: TConnInfo;
public
    constructor Create(CreateSuspended: Boolean);
    procedure SetConnInfo(var connInfo: TConnInfo);
    procedure Execute; override;
end;

TIMAPConnection = class
private
    mLoginThread: TLoginThread;  // Thread used for logging in
    mSocketTimeout: Integer;     // How long the client waits for servers replies on normal operations
    function amIConnected: Boolean;
    function Login(var account: TAccountInfo): THandle;
public
    id: Integer;                // ID of the connection
    conn: TConnInfo;            // Connection information
    // Pointer to the structure used for communicating login results (requestId, connId, success)
    // No need to (de)allocate - is only passed as a parameter...
    getConnResponseInfoPtr: PGetConnectionResponseInfo;
    constructor Create;
    procedure Logout;
    function Reconnect(var account: TAccountInfo; sendMessage: Boolean): THandle; overload;
    function Reconnect(sendMessage: Boolean): THandle; overload;
    procedure RecreateSocket;
    procedure TerminateLogin;
    destructor Destroy; override;
published
    property isConnected: Boolean read amIConnected;
end;


implementation

uses Main;


{ ----------- TConnInfo ----------- }

constructor TConnInfo.Create;
begin
    isLoggedIn := false;
    isLoggingIn := false;
    isBusy := false;
    imap := TIMAPSend.Create;
    account := TAccountInfo.Create;
    new(imapConnRef);
end;

destructor TConnInfo.Destroy;
begin
    account.Free;
    imap.Free;
    dispose(imapConnRef);
end;

{ Inform FMain of the login result.
  success = 0 when host not found, 1 if wrong user/pass, 2 if successfull, 3 if terminated by user }
procedure TConnInfo.HandleLoginOver(success: TLoginResult);
begin
    if not mSendMessage then success := lrDontDisplayResult;
    imapConnRef^.getConnResponseInfoPtr^.success := ord(success);
    PostMessage(requestMgr.Handle,WM_GOTCONNECTION,Integer(imapConnRef^.getConnResponseInfoPtr),0);
end;

{ ----------- TLoginThread ----------- }

constructor TLoginThread.Create(CreateSuspended: Boolean);
begin
    Inherited Create(CreateSuspended);
    FreeOnTerminate := true;
end;

{ Sets the pointer to the caller connection }
procedure TLoginThread.SetConnInfo(var connInfo: TConnInfo);
begin
    mConnInfo:=connInfo;
end;

procedure TLoginThread.Execute;
begin
    mConnInfo.isLoggingIn:=true;
    if mConnInfo.imap.Login then begin
        if not Terminated then begin
            mConnInfo.HandleLoginOver(lrSuccess);    // OK, logged in
            mConnInfo.isLoggedIn:=true;
        end
        else begin
            mConnInfo.HandleLoginOver(lrTerminated);   // was terminated
            mConnInfo.isLoggedIn:=false;
        end;
    end
    else begin
        mConnInfo.isLoggedIn:=false;
        // Didn't login, try to find the cause
        if not Terminated then begin
            devLog.Info('LastError for Login: '+inttostr( mConnInfo.imap.Sock.LastError));
            if mConnInfo.imap.Sock.LastError=0 then begin
                // Bad username/password
                mConnInfo.HandleLoginOver(lrWrongUserPass);
            end
            else begin
                // Host not found
                mConnInfo.HandleLoginOver(lrHostNotFound);
            end;
        end
        else mConnInfo.HandleLoginOver(lrTerminated);
    end;
    mConnInfo.isLoggingIn:=false;
    devLog.debug('Login thread done. Self terminating.');
end;


{ ----------- TIMAPConnection ----------- }

constructor TIMAPConnection.Create;
begin
    conn:=TConnInfo.Create;
    mSocketTimeout:=settings.IMAPOpSettings.ServerTimeout*1000;
end;

{ Connects to the server. Returns true if connected, false if not 
  @return The handle of the login thread. }
function TIMAPConnection.Login(var account: TAccountInfo): THandle;
var mLoginThread: TLoginThread; pass, memPass: String;
    PasswordDlg: TFPasswordDlg;
begin
    devLog.Info('Logging in to account: '+account.Name);
    conn.Imap.Username := conn.account.Username;

    if conn.account.SavePassword then begin
        // Decrypt saved password
        pass := Decrypt(conn.account.Password);
    end
    else begin
        // Ask for password and store in memory while the app is running
        memPass:=Accounts.GetPassFromMemory(conn.account.Name);
        if memPass='' then begin
            // The password was not previously obtained, ask for it
            // Ask for password
            PasswordDlg := TFPasswordDlg.Create(Application);
            PasswordDlg.SetAccountName(conn.account.Name);
            PasswordDlg.ShowModal;
            memPass:=PasswordDlg.GetPassword;
            memPass:=Encrypt(memPass);
            // devLog.Debug('Storing password '+memPass+' for account '+conn.account.Name);
            Accounts.StorePassInMemory(conn.account.Name,memPass); // Store encrypted pass in the hashtable
            conn.account.SavePassword:=PasswordDlg.SavePassword;
            if conn.account.SavePassword then
                // If the user has selected to store the password for further sessions, save it
                conn.account.Password:=memPass;
        end;
        // We have the password in memory, use it
        pass := Decrypt(memPass);
    end;

    // insert \ if necessary
    pass:=StringReplace(pass,'\','\\',[rfReplaceAll]);
    conn.Imap.Password:=StringReplace(pass,'"','\"',[rfReplaceAll]);

    conn.Imap.TargetHost := conn.account.IMAPServer;
    conn.Imap.TargetPort := IntToStr(conn.account.Port);
    conn.Imap.FullSSL := conn.account.SSL;
    // conn.Imap.Timeout := SOCK_TIMEOUT;
    conn.Imap.Timeout := mSocketTimeout;

    FMain.ShowMessageOnStatusBar('Connecting to account: '+conn.account.Name, STATUSBAR_MSGS);
    FMain.StatusBar1.Repaint;

    mLoginThread:=TLoginThread.Create(true);
    conn.imapConnRef^:=Self;
    mLoginThread.SetConnInfo(conn);
    mLoginThread.Priority:=tpLowest;
    FMain.VCLAdaptLoginThreadRunning;
    Result:=mLoginThread.Handle;
    mLoginThread.Resume;
end;

{ Always call this to perform a login.
  @param account account to login
  @param sendMessage True if a message should be sent to FMain about the status of the login
         Use false if you want to perform a silent (re)connection
  @param loginMutexIndex if this is >0 then someone is waiting for the login thread to finish.
         The login thread should release the mutex in this case!
}
function TIMAPConnection.Reconnect(var account: TAccountInfo; sendMessage: Boolean): THandle;
var shouldLogin, shouldRecreateSocket: Boolean; loginThreadHandle: THandle;
begin
    devLog.Trace('IMAPConnection - Reconnect invoked');
    conn.mSendMessage:=sendMessage;
    shouldRecreateSocket:=true;
    shouldLogin := true;
    loginThreadHandle:=0;

    if accountsEqual(conn.account,account) then begin
        // Account is equal to the previous, just check if we are still connected
        devLog.Debug('Accounts the same, checking connection');

        // Don't send noop if we were not previously connected
        if amIConnected then begin
            devLog.Trace('Connected');
            if conn.isLoggedIn then begin
                shouldRecreateSocket:=false;
                shouldLogin:=false;
                devLog.Debug('No need to login - already logged in...');
                conn.HandleLoginOver(lrAlreadyLoggedIn);  // no real thread -> no real id, will just signal to the main unit that it can procede
            end
            else begin
                // This covers the case where NOOP returns true even if we are
                // not logged in (not authenticated).
                devLog.Trace('Connected but not logged in. Possible loss of connection');
                // This can happen if the server has been restarted etc. Recreate the socket (there were problems with this)
                shouldRecreateSocket:=true;
            end;
        end
        else devLog.Trace('amIConnected - Not connected');
    end;

    CopyAccount(account,conn.account);

    if shouldRecreateSocket then RecreateSocket;

    if shouldLogin then begin
        Logout;
        loginThreadHandle := Login(account);
        // Copy the account in case there were changes during login
        CopyAccount(conn.account,account);
    end;

    Result:=loginThreadHandle;
end;

{ Reconnect with the current account }
function TIMAPConnection.Reconnect(sendMessage: Boolean): THandle;
begin
    Result := Reconnect(conn.account,sendMessage);
end;

procedure TIMAPConnection.Logout;
begin
    // If NoOp fails, we are not logged in (doesn't matter if mConnected is true).
    if amIConnected then begin
        devLog.Debug('Logging out');
        conn.Imap.Logout;
    end
    else devLog.Debug('Not logged in');
    conn.isLoggedIn:=false;
end;

{ Will mark the login thread for graceful termination }
procedure TIMAPConnection.TerminateLogin;
begin
    mLoginThread.Terminate;
end;

function TIMAPConnection.amIConnected: Boolean;
var res: Boolean;
begin
    conn.Imap.Timeout := NOOP_TIMEOUT;
    res:=conn.Imap.NoOp;
    // conn.Imap.Timeout := SOCK_TIMEOUT;
    conn.Imap.Timeout := mSocketTimeout;
    Result:=res;
end;

{ Recreates the underlying socket in case something went wrong with it (e.g. server reboot, etc) }
procedure TIMAPConnection.RecreateSocket;
begin
    devLog.Trace('Recreating Socket');
    conn.imap.Free;
    conn.imap := TIMAPSend.Create;
end;

destructor TIMAPConnection.Destroy;
begin
    Logout;
    conn.imapConnRef:=nil;
    conn.Free;
end;

end.