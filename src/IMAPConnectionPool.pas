{==============================================================================|
| Project : IMAPSize                                                           |
|==============================================================================|
| $Id: IMAPConnectionPool.pas,v 1.3 2004/03/31 23:27:37 Ivan Exp $
|==============================================================================|
| Copyright 2003-2004 Ivan Vecanski                                            |
| =============================================================================}

unit IMAPConnectionPool;

interface

uses sysutils, windows, IMAPConnection, Accounts, HashTables, GlobalConstants, Log, VecLog, Messages,
     GetConnectionResponseInfo;

type

{ Exception which is raised if any problems occur with the connection pool }
EConnectionPoolException = class(Exception);

TIMAPConnectionPool = class
  private
    mMaxAllowedConnections: Integer; //@todo currently not used
    mIMAPConnections: THashTable;
    mIDCounter: Integer; // counter for unique ids
    cs: TRTLCriticalSection;
    procedure FreeConnectionPool;
    procedure FreeConnections;
    function getNewId: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure GetConnection(var account: TAccountInfo; const getConnResponseInfoPtr: PGetConnectionResponseInfo); overload;
    function GetConnection(id, requestId: Integer): TIMAPConnection; overload;
    procedure ReturnConnection(id: Integer);
    function IsAccountActive(accountInfo: TAccountInfo): Boolean;
    procedure LogoutAccount(accountInfoPtr: PAccountInfo);
    procedure DumpContents(msg: String);
  published
    property MaxAllowedConnections: Integer read mMaxAllowedConnections write mMaxAllowedConnections;
end;
PIMAPConnectionPool = ^TIMAPConnectionPool;

implementation

uses Main, MyUtil;

constructor TIMAPConnectionPool.Create;
begin
    mIMAPConnections:=THashTable.Create;
    mIDCounter:=0;
    InitializeCriticalSection(cs);
end;

destructor TIMAPConnectionPool.Destroy;
begin
    DeleteCriticalSection(cs);
    FreeConnectionPool;
end;

{ Call this method to safely deallocate the conncetion pool }
procedure TIMAPConnectionPool.FreeConnectionPool;
var imapConnPtr: PIMAPConnection;
begin
    if mIMAPConnections<>nil then begin
        FreeConnections;
        mIMAPConnections.Free;
    end;
end;

{ Will free all connections (implicit disconnection on conn.free -
  see TIMAPConnection }
procedure TIMAPConnectionPool.FreeConnections;
var imapConnPtr: PIMAPConnection;
begin
    if mIMAPConnections<>nil then begin
        imapConnPtr:=mIMAPConnections.First;
        while (imapConnPtr<>nil) do begin
            imapConnPtr^.Free;
            Dispose(imapConnPtr);
            imapConnPtr:=mIMAPConnections.Next;
        end;
    end;
end;

function TIMAPConnectionPool.GetNewId: Integer;
begin
    Inc(mIDCounter);
    Result:=mIDCounter;
end;

{   Gets a IMAP connection from the pool. If there are no connections, will create a new one (login)
    Will send a message to RequestMgr that a connection has been made.
    @param account IMAP Account to which we are connecting
    @param threadId The ID of the thread for which we are getting the connection (not used,
           but the caller will need it to identify the thread
}
procedure TIMAPConnectionPool.GetConnection(var account: TAccountInfo; const getConnResponseInfoPtr: PGetConnectionResponseInfo);
var found: Boolean; connId: Integer;
    imapConnPtr: PIMAPConnection;
begin
    EnterCriticalSection(cs);
    try
        devLog.Trace('Getting connection');
        // Go through the list of connections and check if there is any available that we can use
        found:=false;
        getConnResponseInfoPtr^.connId:=-1;
        imapConnPtr:=mIMAPConnections.First;
        while (imapConnPtr<>nil) and (not found) do begin

            if accountsEqual(account,imapConnPtr^.conn.account) then begin
                // Connection found for this account. Check if the connection is free to use
                devLog.Trace('Connection for account found. Checking if busy...');
                if not imapConnPtr^.conn.isLoggedIn then begin
                    //@todo try to login, this happens if the server was down in the
                    // first attempt and is now up.
                    // Currently just ignore and search for another connection (new if not found)
                    devLog.Debug('Connection exists, but not logged in. Skipping this one (@todo)');
                end
                else if not imapConnPtr^.conn.isBusy then begin
                    // Found suitable connection.
                    found:=true;
                    getConnResponseInfoPtr^.connId := imapConnPtr^.id;
                    getConnResponseInfoPtr^.success := ord(lrAlreadyLoggedIn);
                    devLog.Info('Will use existing connection');
                end;
                if not found then devLog.Trace('Connection busy. Continuing search.');
            end;
            if not found then imapConnPtr:=mIMAPConnections.Next;
        end;
        // We didn't find an available connection. Create a new one...
        if not found then begin
            devLog.Info('Suitable connection not found. Creating a new IMAP connection');
            New(imapConnPtr);
            imapConnPtr^ := TIMAPConnection.Create;
            imapConnPtr^.ID := GetNewId;
            imapConnPtr^.getConnResponseInfoPtr:=getConnResponseInfoPtr;
            imapConnPtr^.getConnResponseInfoPtr^.connId := imapConnPtr^.ID;
            mIMAPConnections[imapConnPtr^.ID]:=imapConnPtr;
            imapConnPtr^.conn.IsBusy := true;
            imapConnPtr^.Reconnect(account, true);
            // A message will be sent back directly to the requestMgr stating the result of the login.
        end
        else begin
            // We found a suitable connection, mark it as busy
            devLog.Trace('Reusing free connection');
            imapConnPtr^.conn.IsBusy := true;
            // Send a notification to requestMgr that a connection is available
            // All the parameters are set in getConnResponseInfoPtr which was passed as a parameter...
            PostMessage(requestMgr.Handle,WM_GOTCONNECTION,Integer(getConnResponseInfoPtr),0);
        end;
    finally
        LeaveCriticalSection(cs);
    end;
end;

{ Gets the connection with the specified id.
  To be used with existing connections.
  @throws EConnectionPoolException if the connection is not found }
function TIMAPConnectionPool.GetConnection(id, requestId: Integer): TIMAPConnection;
var imapConnPtr: PIMAPConnection;
begin
    EnterCriticalSection(cs);
    try
        if mIMAPConnections<>nil then begin
            imapConnPtr:=mIMAPConnections.Items[id];
            if imapConnPtr <> nil then begin
                imapConnPtr^.getConnResponseInfoPtr^.requestId:=requestId;
                Result:=imapConnPtr^;
            end
            else raise EConnectionPoolException.Create('Can''t find connection with id='+IntToStr(id));
        end
        else raise EConnectionPoolException.Create('Connection pool doesn''t exist!');
    finally
        LeaveCriticalSection(cs);
    end;
end;

{ Tells the pool that the connection is no longer needed by the
  caller. This DOESN'T mean that we are closing the connection,
  it just means that the connection will be available for use
  by other callers... }
procedure TIMAPConnectionPool.ReturnConnection(id: Integer);
begin
    devLog.Trace('Returning connection id='+inttostr(id));
    EnterCriticalSection(cs);
    try
        PIMAPConnection(mIMAPConnections.Items[id])^.conn.isBusy := false;
    except
        // do nothing (shouldn't happen)
        devLog.Error('Failed to return IMAP connection!');
    end;
    LeaveCriticalSection(cs);
    devLog.Trace('Connection returned');
end;

{ Checks if any connections for the specified account are busy }
function TIMAPConnectionPool.IsAccountActive(accountInfo: TAccountInfo): Boolean;
var found: Boolean;
    imapConnPtr: PIMAPConnection;
begin
    // Go through the list of connections and check if there is any available that we can use
    EnterCriticalSection(cs);
    found:=false;
    try
        imapConnPtr:=mIMAPConnections.First;
        while (imapConnPtr<>nil) and (not found) do begin
            if accountsEqual(accountInfo,imapConnPtr^.conn.account) then begin
                if imapConnPtr^.conn.isBusy then found:=true
                else imapConnPtr:=mIMAPConnections.Next;
            end;
        end;
    finally
        LeaveCriticalSection(cs);
        Result:=found;
    end;
end;

{ Performs the logout operation on all connections for the specified account.
  If there are multiple connections, all but one will be removed }
procedure TIMAPConnectionPool.LogoutAccount(accountInfoPtr: PAccountInfo);
var imapConnPtr: PIMAPConnection;
begin
    EnterCriticalSection(cs);
    try
        imapConnPtr:=mIMAPConnections.First;
        while (imapConnPtr<>nil) do begin
            if accountsEqual(accountInfoPtr^,imapConnPtr^.conn.account) then begin
                if imapConnPtr^.conn.isBusy then imapConnPtr^.conn.isBusy := false;
                //imapConnPtr^.Free;
                //Dispose(imapConnPtr);
                imapConnPtr:=mIMAPConnections.Next;
            end;
        end;
    finally
        LeaveCriticalSection(cs);
    end;
end;

{ Dumps the contents of the connection pool }
procedure TIMAPConnectionPool.DumpContents(msg: String);
var imapConnPtr: PIMAPConnection;
begin
    devLog.Trace('IMAPConnectionPool contents: '+msg);
    devLog.Trace('Total connections in pool: '+inttostr(mIMAPConnections.Count));
    imapConnPtr:=mIMAPConnections.First;
    while (imapConnPtr<>nil) do begin
        devLog.Trace(Format('ID=%3d Account=%10s Active=%s',[imapConnPtr^.Id,imapConnPtr^.conn.account.name,MyUtil.BoolToStr(imapConnPtr^.conn.isBusy)]));
        imapConnPtr:=mIMAPConnections.Next;
    end;
end;

end.