{==============================================================================|
| Project : IMAPSize                                                           |
|==============================================================================|
| $Id: IMAPWorker.pas,v 1.31 2004/04/04 20:17:11 Ivan Exp $
|==============================================================================|
| Copyright 2003-2005 Ivan Vecanski                                            |
| =============================================================================}

unit IMAPWorker;

interface

uses forms, windows, classes, sysutils, filectrl, Nodes, IMAPConnection, dialogs, Envelope, Log, VecLog,
     MessageLists, SplitFns, SpamHandles, synautil, BodyStructure, CustomThread,
     MyUtil, GlobalConstants, Converter, RegExpr, HashTables, ICache, messages, FolderListProcessor, BackupFilenameFormat;

const
    ENABLE_PAGING = false;
    LOGIN_MUTEX_TIMEOUT = 60000;
    NO_CONNECTION_MSG='Connection to server lost!';
type

ERecoverableError = class(Exception);
EUnrecoverableError = class(Exception);

TIMAPOperation = (iopNone,
                  iopFetchHeader,
                  iopFetchWholeBody,
                  iopFetchBodyPart,
                  iopFetchWholeMessage,
                  iopDeleteMsg,
                  iopUndeleteMsg,
                  iopSeenMsg,
                  iopUnseenMsg,
                  iopFlagMsg,
                  iopUnflagMsg,
                  iopAnsweredMsg,
                  iopUnansweredMsg,
                  iopExpungeMbox,
                  iopDeleteAllInMbox,
                  iopUndeleteAllInMbox,
                  iopDeleteExpungeMsg,
                  iopDeleteAndExpungeAllInMbox,
                  iopGetMboxEnvelopes,
                  iopGetQuota,
                  iopGetMultipleMessages,
                  iopSearch,
                  iopSearchReverse,
                  iopSearchAdvanced,
                  iopSearchGlobal,
                  iopSearchAdvancedGlobal,
                  iopCopy,
                  iopCopyDelete,
                  iopCopyDelExpunge,
                  iopCreateMbox,
                  iopDeleteMbox,
                  iopRenameMbox,
                  iopAppendMessage,
                  iopUploadMbox,
                  iopUploadEMLs,
                  iopDownloadMbox,
                  iopDownloadEMLs,
                  iopSaveMessageHeaders,
                  iopGetBodyStructure,
                  iopGetBodyStructures,
                  iopDeleteAttachments,
                  iopGetFolderList,
                  iopGetSubscribedFolderList,
                  iopSubscribeFolders,
                  iopUploadForRemoteCopy,
                  iopDownloadForRemoteCopy,
                  iopDebugServer,
                  iopAccountBackup,
                  iopRestoreBackup,
                  iopSaveAttachments);

{ Specifies the details about a copy command }
TCopyOperation = (tcoCopyOnly, tcoCopyMarkDeleted, tcoCopyDeleteExpunge);

{ Subscription pair, used for (un)subscribing folders }
TSubscriptionPair = class
    public
        Name: String;       // full name of the folder
        Subscribe: Boolean; // true if the folder should be subscribed, false otherwise
        constructor Create(inName: String; inSubscribe: Boolean);
end;

{ Thread for retreiving individual messages }
TIMAPWorker = class(TCustomThread)
private
    mIMAPConn: TIMAPConnection;
    mFullMailboxName: String;
    mDestFullMailboxName: String;
    mMsgUID: String;
    mMsgUIDs: TStringList;
    mMsgIDs: TStringList;
    mOperation: TIMAPOperation;
    mSelectedMailboxIndex: Integer;
    mDestMailboxIndex: Integer;
    mSearchString: String;
    mMsgFlags: TMessageFlags;
    mBodyPart: String;
    mFilename: String;
    mFilenames: TStringList;
    mFolderSeparator: Char;
    mFolders: TStringList;
    mSaveToDir: String;  // Dir name used for saving stuff...
    mCommandLineMode: Boolean;  // true if the request is invoked in the command line mode (backup,etc)
    // For renaming:
    mNewFullDisplayedName: String;
    mNewFullMailboxName: String;
    mNewNodeName: String;
    mMessageContent: TStringList;

    // Sync method vars
    mStatusBarText: String;
    mStatusBarPanelIndex: Integer;
    mIndexFirst, mIndexLast: Integer;

    // Login stuff
    mReloginAndTryAgain: Boolean;   // To be set to true if an operation failed (might need relogin)
    mAttemptCount: Integer;         // Usually this will count to 1, if relogin is needed then 2, otherwise abandon the operation

    procedure getMboxMsgInfo;
    function getMsgInfo(var msgInfo: TMessageInfo; uid: String): Boolean;
    procedure getMultipleMsgInfos;
    function fetchMessageInfos(var mMsgInfos: TListOfMessageInfos; numMessages: Integer; notifyProgress, page: Boolean): Boolean;
    procedure ReformatReply(var formattedReply:TStringList; serverReply: TStringList);
    function ProcessMessage(replyLine: String;
                spamHandleInfo: TSpamHandleInfo; hasSpamHandle: Boolean): TMessageInfo;
    procedure DisplayQuota;
    function getQuota(var currentQuota, maxQuota: Integer): Integer;
    procedure PerformDelete(delete, updateMsgList: Boolean);
    procedure SetFlagOnSelectedMsgs(setFlag: Boolean; op: TIMAPOperation);
    procedure ExpungeMailbox;
    procedure DeleteAndExpungeMailbox;
    procedure PerformSearch(rawSearch: boolean; reverted: boolean);
    procedure PerformGlobalSearch(rawSearch: Boolean);
    procedure PerformCopy;
    procedure PerformFetch;
    procedure PopulateMessageList;
    procedure AddMessagesToMessageList;
    procedure UpdateMessageList;
    procedure ClearMessageList;
    procedure UpdateTreeView;
    procedure SetQuota(current, maximum: Integer);
    procedure UpdateQuota;
    procedure DeleteMbox;
    procedure CreateMbox;
    procedure RenameMbox;
    procedure RenameMailboxes;
    procedure InsertNewMailboxToNodeArray(mFullMailboxName: String);
    procedure RemoveDeletedMailboxFromNodeArray;
    procedure AppendMessage;
    procedure UploadMbox;
    procedure UploadEMLs;
    procedure DownloadMbox;
    procedure DownloadEMLs; overload;
    procedure DownloadEMLsBackupMode(backupFolderId: Integer; maxUID: String; var summaryLvl: TMessageLevel);
    procedure DownloadEMLs(var createdFiles: TStringList; progressStart, progressEnd: Integer;
                           var msgsDownloaded, totalMsgs: Integer; backupMode: Boolean; backupFolderId: Integer;
                           var msgUIDsToSkip: TStringList; maxUID: String; var summaryLvl: TMessageLevel); overload;
    procedure DownloadEMLsForRemoteCopy;
    procedure UploadEMLsToRemoteServer;
    procedure SaveHeadersOfSelectedMessages;
    procedure UpdateStatusBar(newText: String; panelIndex: Integer);
    procedure InternalUpdateStatusBar;
    procedure HandleErrorSituation(msg: String);
    function CheckResult: String;
    function GetResultMessage: String;
    procedure UploadWithErrorCatching(var msgContents: TStringList; refreshQuota,refreshTree,refreshList: Boolean; flags: String);
    procedure UploadSingleNewMessage(var msgContents: TStringList; refreshQuota,refreshTree,refreshList: Boolean; flags: String);
    procedure addLineToFile(var theFile: TFileStream; theLine: String);
    procedure SearchUIDs(criteria: String; var res: TStringList);
    procedure GetBodyStructure;
    procedure GetBodyStructures;
    procedure QSDeleteAttachments;
    function GetFlagsStringForAppend(msgFlags: TMessageFlags): String;
    procedure GetFolderList(subscribedOnly: Boolean; var personalNamespace: String);
    procedure InvokeMessageListAddition(mboxIndex, msgFrom, msgTo: Integer; clear: Boolean);
    procedure SubscribeFolders;
    procedure DebugServer;
    procedure AccountBackup;
    procedure RestoreBackup;
    procedure SaveAttachments;
    procedure ParseMess(Value:TStrings);
    function IsSpam(spamScore: String; var spamHandleInfo: TSpamHandleInfo): Boolean;
    function GetMessageId(const fullReply: TStringList): String;
public
    constructor Create(CreateSuspended: Boolean);
    destructor Destroy; override;
    procedure Execute; override;
    procedure SetIMAPConnection(imapConn: TIMAPConnection);
    procedure setMsgUIDs(msgUIDs: TStringList);
    procedure setMsgFlags(flags: TMessageFlags);
    procedure setMessageContents(msg: TStringList);
    procedure setFilenames(filenames: TStrings);
    procedure setFolders(folders: TStrings);
published
    property Operation: TIMAPOperation read mOperation write mOperation;
    property FullMailboxName: String read mFullMailboxName write mFullMailboxName;
    property FullDestMailboxName: String read mDestFullMailboxName write mDestFullMailboxName;
    property MsgUID: String read mMsgUID write mMsgUID;
    property SelectedMailboxIndex: Integer read mSelectedMailboxIndex write mSelectedMailboxIndex;
    property DestMailboxIndex: Integer read mDestMailboxIndex write mDestMailboxIndex;
    property SearchString: String read mSearchString write mSearchString;
    property NewFullDisplayedName: String read mNewFullDisplayedName write mNewFullDisplayedName;
    property NewFullMailboxName: String read mNewFullMailboxName write mNewFullMailboxName;
    property NewNodeName: String read mNewNodeName write mNewNodeName;
    property BodyPart: String read mBodyPart write mBodyPart;
    property Filename: String read mFilename write mFilename;
    property FolderSeparator: Char read mFolderSeparator write mFolderSeparator;
    property CommandLineMode: Boolean read mCommandLineMode write mCommandLineMode;
end;


implementation

uses Main, GlobalSearch, RequestManager, MsgPeeker, AttachmentStripper,
     FolderHierarchyReplicatorDlg, MailUtil, CommonTypes;

constructor TSubscriptionPair.Create(inName: String; inSubscribe: Boolean);
begin
    Name:=inName;
    Subscribe:=inSubscribe;
end;

constructor TIMAPWorker.Create(CreateSuspended: Boolean);
begin
    Inherited Create(CreateSuspended, thtIMAPWorker);
    mMsgUIDs := TStringList.Create;
    mMessageContent := TStringList.Create;
    mFilenames:= TStringList.Create;
    mFolders:=TStringList.Create;
end;

destructor TIMAPWorker.Destroy;
begin
    mMsgUIDs.Free;
    mMessageContent.Free;
    mFilenames.Free;
    mFolders.Free;
    Inherited Destroy;
end;

procedure TIMAPWorker.SetIMAPConnection(imapConn: TIMAPConnection);
begin
    mIMAPConn:=imapConn;
end;


procedure TIMAPWorker.setMsgUIDs(msgUIDs: TStringList);
begin
    CopyStringList(TStrings(mMsgUIDs), TStrings(msgUIDs));
end;

procedure TIMAPWorker.setMessageContents(msg: TStringList);
begin
    CopyStringList(TStrings(mMessageContent), TStrings(msg));
end;

procedure TIMAPWorker.setMsgFlags(flags: TMessageFlags);
begin
    mMsgFlags:=flags;
end;

procedure TIMAPWorker.setFilenames(filenames: TStrings);
begin
    CopyStringList(TStrings(mFilenames), filenames);
end;

procedure TIMAPWorker.setFolders(folders: TStrings);
begin
    CopyStringList(TStrings(mFolders), folders);
end;

procedure TIMAPWorker.Execute;
var msg: TMessage;
    loginThreadHandle: THandle;
    waitResult: Integer;
    varStr: String;
begin
    try
        varStr:='';
        mReloginAndTryAgain:=false;
        mAttemptCount:=0;
        repeat
            Inc(mAttemptCount);
            if (mAttemptCount=2) then begin
                devLog.Warn('First attempt failed. Possible loss of connection. Will relogin and try again');
                loginThreadHandle := mIMAPConn.Reconnect(false);  // invoke silent relogin
                if (loginThreadHandle<>0) then begin
                    devLog.Trace('Got the loginThreadHandle. Waiting for login. '+IntToStr(loginThreadHandle));
                    waitResult := WaitForSingleObject(loginThreadHandle, LOGIN_MUTEX_TIMEOUT);
                    if waitResult=WAIT_OBJECT_0 then devLog.Trace('IMAPWorker - mutex released')
                    else if waitResult=WAIT_TIMEOUT then devLog.Trace('IMAPWorker - timeout elapsed')
                    else devLog.Trace('IMAPWorker - WAIT_ABANDONED');
                end
                else devLog.Trace('loginThreadHandle is nil - no login going on. Continuing');
            end;
            case mOperation of
                iopFetchHeader,iopFetchWholeBody,iopFetchBodyPart, iopFetchWholeMessage: PerformFetch;
                iopDeleteMsg, iopSeenMsg, iopFlagMsg, iopAnsweredMsg: SetFlagOnSelectedMsgs(true, mOperation);
                iopUndeleteMsg, iopUnseenMsg, iopUnflagMsg, iopUnansweredMsg: SetFlagOnSelectedMsgs(false, mOperation);
                iopExpungeMbox: ExpungeMailbox;
                iopDeleteAllInMbox: PerformDelete(true,true);
                iopUndeleteAllInMbox: PerformDelete(false,true);
                iopDeleteExpungeMsg: begin
                    SetFlagOnSelectedMsgs(true, mOperation);
                    ExpungeMailbox;
                end;
                iopDeleteAndExpungeAllInMbox: DeleteAndExpungeMailbox;
                iopGetMboxEnvelopes: begin
                    UpdateStatusBar('Retreiving messages from: '+mFullMailboxName,STATUSBAR_MSGS);
                    getMboxMsgInfo;
                    if not ENABLE_PAGING and not terminated then Synchronize(PopulateMessageList);
                    if not terminated then UpdateStatusBar('',STATUSBAR_MSGS);
                end;
                iopGetMultipleMessages: getMultipleMsgInfos;
                iopGetQuota: DisplayQuota;
                iopSearch: PerformSearch(false,false);
                iopSearchReverse: PerformSearch(false,true);
                iopSearchAdvanced: PerformSearch(true,false);
                iopSearchGlobal: PerformGlobalSearch(false);
                iopSearchAdvancedGlobal: PerformGlobalSearch(true);
                iopCopy,iopCopyDelete,iopCopyDelExpunge: PerformCopy;
                iopCreateMbox: CreateMbox;
                iopRenameMbox: RenameMbox;
                iopDeleteMbox: DeleteMbox;
                iopAppendMessage: AppendMessage;
                iopUploadMbox: UploadMbox;
                iopUploadEMLs: UploadEMLs;
                iopDownloadMbox: DownloadMbox;
                iopDownloadEMLs: DownloadEMLs;
                iopSaveMessageHeaders: SaveHeadersOfSelectedMessages;
                iopGetBodyStructure: GetBodyStructure;
                iopGetBodyStructures: GetBodyStructures;
                iopDeleteAttachments: QSDeleteAttachments;
                iopGetFolderList: GetFolderList(false,varStr);
                iopGetSubscribedFolderList: GetFolderList(true,varStr);
                iopUploadForRemoteCopy: UploadEMLsToRemoteServer;
                iopDownloadForRemoteCopy: DownloadEMLsForRemoteCopy;
                iopSubscribeFolders: SubscribeFolders;
                iopDebugServer: DebugServer;
                iopAccountBackup: AccountBackup;
                iopRestoreBackup: RestoreBackup;
                iopSaveAttachments: SaveAttachments;
            else devLog.Info('NO OP');
            end;
        until ((not mReloginAndTryAgain) or (mAttemptCount>=2));
        // Send a message to the main form to display messages
        devLog.Trace('IMAPWorker done, sending message');
    finally
        vcLog.Flush;
        // Inform the request manager that the request is over
        PostMessage(requestMgr.Handle,WM_IMAPOPOVER,ThreadInfo.ID,0);
    end;
end;

{ Parses a response to a fetch command.
  Taken from IMAPSend (is private there) }
procedure TIMAPWorker.ParseMess(Value:TStrings);
var n: integer;
begin
    Value.Clear;
    for n := 0 to mIMAPConn.conn.imap.FullResult.Count - 2 do
        if mIMAPConn.conn.imap.FullResult[n][Length(mIMAPConn.conn.imap.FullResult[n])] = '}' then begin
            Value.Text := mIMAPConn.conn.imap.FullResult[n + 1];
            Break;
        end;
end;

{ Performs a FETCH of a SINGLE message (header, text (body part 1), both,
  specified body part, all depending on the op }
procedure TIMAPWorker.PerformFetch;
var command, bs, res: String; header, body, bp: TStrings; request: TRequest; performFetch: Boolean;
begin
    if mIMAPConn.conn.imap.SelectFolder(mFullMailboxName) then begin
        performFetch:=true;
        request := requestMgr.GetRequest(ThreadInfo.requestId);
        request.data.msgContent.Clear;
        if (mOperation = iopFetchBodyPart) then begin
            devLog.Trace('PerformFetch - iopFetchBodyPart. BodyPart: '+mBodyPart);
            command:='UID FETCH '+mMsgUID+' BODY['+mBodyPart+']';
        end
        else if (mOperation = iopFetchWholeBody) then begin
            if request.data.limitLength >= 0 then
                command:='UID FETCH ' + mMsgUID + ' BODY[TEXT]<0.'+IntToStr(request.data.limitLength)+'>'
            else
                command:='UID FETCH ' + mMsgUID + ' BODY[TEXT]';
        end
        else if (mOperation = iopFetchWholeMessage) then begin
            if request.data.limitLength >= 0 then
                // Get the specified number of characters
                command := 'UID FETCH ' + mMsgUID + ' BODY[]<0.'+IntToStr(request.data.limitLength)+'>'
            else
                // No limits, fetch whole message
                command := 'UID FETCH ' + mMsgUID + ' BODY[]';
        end
        else if mOperation = iopFetchHeader then begin
            command:='UID FETCH ' + mMsgUID + ' BODY[HEADER]';
        end
        else begin
            devLog.Error('Unknown operation');
            performFetch:=false;
        end;

        if performFetch then begin
            if not terminated then begin
                if mIMAPConn.conn.imap.IMAPCommand(command)='OK' then ParseMess(request.data.msgContent)
                else devLog.Warn('Server could not execute the command');
            end
            else UpdateStatusBar('Fetch aborted',STATUSBAR_MSGS);
        end;
    end
    else HandleErrorSituation('Fetch failure');
end;

{ Gets the messages body structure. Result stored in request.data.stringResult.
  Note that the body structure still has to be parsed... }
procedure TIMAPWorker.GetBodyStructure;
var request: TRequest; command, res: String;
begin
    if mIMAPConn.conn.imap.SelectFolder(mFullMailboxName) then begin
        request := requestMgr.GetRequest(ThreadInfo.requestId);
        command:='UID FETCH ' + mMsgUID + ' (BODY)';
        if (mIMAPConn.conn.imap.IMAPCommand(command)='OK') then begin
            request.data.stringResult:=mIMAPConn.conn.imap.FullResult[0];
        end
    end
    else HandleErrorSituation('BodyStructure fetch failure');
end;

{ Retrieves multiple body structures (from a single folder).
  UIDs of messages are passed in msgUIDs,
  body structure strings are stored in msgContent of the request data. }
procedure TIMAPWorker.GetBodyStructures;
var request: TRequest; command: String; i: Integer;
begin
    if mIMAPConn.conn.imap.SelectFolder(mFullMailboxName) then begin
        request := requestMgr.GetRequest(ThreadInfo.requestId);
        request.data.msgUIDs.Clear;  // Clear for storing the resulting uids
        for i:=0 to mMsgUIDs.Count-1 do begin
            command:='UID FETCH ' + mMsgUIDs[i] + ' (BODY)';
            if (mIMAPConn.conn.imap.IMAPCommand(command)='OK') then begin
                request.data.msgContent.Add(mIMAPConn.conn.imap.FullResult[0]);
                request.data.msgUIDs.Add(mMsgUIDs[i]);
            end;
        end;
    end
    else HandleErrorSituation('BodyStructure fetch failure');
end;

procedure TIMAPWorker.DisplayQuota;
var currentQuota, maxQuota, percent: Integer; getQuotaResult: Integer; msg: String;
begin
     getQuotaResult:=GetQuota(currentQuota,maxQuota);
     UpdateStatusBar('',STATUSBAR_MSGS);  // clear messages
     case getQuotaResult of
     0: begin // Quota retrieved
            percent:=Round(currentQuota/maxQuota*100);
            SetQuota(currentQuota,maxQuota);
            msg:='Storage Quota: '+IntToStr(currentQuota)+' of '+IntToStr(maxQuota)+' ('+IntToStr(percent)+'%)';
            UpdateStatusBar(msg,STATUSBAR_QUOTA);
        end;
     1: begin  // Server doesn't support quota
            msg:='Storage Quota: Not supported by server';
            UpdateStatusBar(msg,STATUSBAR_QUOTA);
            requestMgr.SetSummary(ThreadInfo.RequestID,'Storage Quota: Not supported by server',mlvInfo);
        end;
     2: begin  // Server supports quota, but couldn't retrieve it
            msg:='Couldn''t retrieve storage quota';
            UpdateStatusBar(msg,STATUSBAR_QUOTA);
            requestMgr.SetSummary(ThreadInfo.RequestID,'Couldn''t retrieve storage quota: '+GetResultMessage,mlvError);
        end;
     end;
end;

{ This one should be called after expunge operations.
  Will update the quota on the status bar }
procedure TIMAPWorker.UpdateQuota;
var currentQuota, maxQuota, percent: Integer; getQuotaResult: Integer; msg: String;
begin
    getQuotaResult:=GetQuota(currentQuota,maxQuota);
    if getQuotaResult = 0 then begin
        // Quota retrieved
        percent:=Round(currentQuota/maxQuota*100);
        msg:='Storage Quota: '+IntToStr(currentQuota)+' of '+IntToStr(maxQuota)+' ('+IntToStr(percent)+'%)';
        UpdateStatusBar(msg,STATUSBAR_QUOTA);
    end;
end;

{ Returns quota info through the var parameters.
  Function returns:
  0 - Quota retrieved
  1 - Server doesn't support quota
  2 - Server supports quota, but unable to retrieve it
}
function TIMAPWorker.getQuota(var currentQuota, maxQuota: Integer): Integer;
var fullReply: TStringList; found: Boolean;
    i, posOpen, posClose :Integer;  storageInfo: String; valueList: TStringList;
    currentQuotaStr, maxQuotaStr: String;
    code: Integer;
    quotaRetreived: Boolean;
    command: String;
begin
    Result:=1;
    quotaRetreived:=false;
    UpdateStatusBar('Getting storage quota',STATUSBAR_MSGS);

    // Synapse retreives capability on login
    if (mIMAPConn.conn.imap.FindCap('QUOTA') <> '') then begin
        Result:=2;
        command:='GETQUOTAROOT INBOX';
        mIMAPConn.conn.imap.IMAPCommand(command);
        fullReply := mIMAPConn.conn.imap.FullResult;

        found:=false;
        i:=0;
        while ((not found) and (i<fullReply.Count)) do begin
            posOpen:=Pos('(STORAGE',fullReply[i]);

            if posOpen>0 then begin
                posClose := Pos(')',fullReply[i]);
                if posClose>0 then begin
                    found:=true;
                    valueList:=TStringList.Create;
                    storageInfo:=Copy(fullReply[i],posOpen+1, posClose-posOpen-1);
                    Split(storageInfo,' ',valueList);
                    if (valueList.count = 3) then begin
                        currentQuotaStr:=valueList[1];
                        maxQuotaStr:=valueList[2];
                        Val(currentQuotaStr,currentQuota,code);
                        Val(maxQuotaStr,maxQuota,code);
                        quotaRetreived:=true;
                        if maxQuota>0 then Result:=0;
                    end
                    else begin
                        devLog.Warn('ValueList of the reply to GETQUOTAROOT INBOX doesn''t have length of 3');
                    end;
                    valueList.Free;
                end
            end
            else i:=i+1;
        end;
        if not quotaRetreived then Result:=2;
    end;
    // else leave Result:=1
end;

{ Performs the (un)delete operation on ALL messages in the mailbox
  @param delete TRUE if messages should be deleted, FALSE if they should be undeleted
  @param updateMsgList TRUE if the message list should be updated}
procedure TIMAPWorker.PerformDelete(delete, updateMsgList: Boolean);
var command, res: String; i: Integer; msg: TMessageInfo;
begin
    if mIMAPConn.conn.imap.SelectFolder(mFullMailboxName) then begin
        if delete then command:='STORE 1:* +FLAGS.SILENT (\Deleted)'
        else command:='STORE 1:* -FLAGS.SILENT (\Deleted)';
        mIMAPConn.conn.imap.IMAPCommand(command);

        // If this mailbox is displayed in the list view mark all messages as deleted
        if mSelectedMailboxIndex=mDisplayedNodeIndex then begin
            for i:=0 to mMessageLists.GetMailboxLength(mSelectedMailboxIndex)-1 do begin
                msg:=mMessageLists.GetMessageInfo(mSelectedMailboxIndex,i);
                msg.mFlags.mDeleted:=delete;
                mMessageLists.UpdateMessageInfo(mSelectedMailboxIndex,i,msg);
            end;
            if updateMsgList then begin
                // Display changes in the tree
                Synchronize(UpdateMessageList);
            end;
        end;
    end
    else HandleErrorSituation('Delete failure');
end;

{ Performs an operation on multiple messages (specified by mMsgUIDs)
  Operation is specified by the class field mOperation.
  @param setFlag TRUE if the flag should be set, FALSE if it should be removed }
procedure TIMAPWorker.SetFlagOnSelectedMsgs(setFlag: Boolean; op: TIMAPOperation);
var command, res, flagStr: String; i, msgIndex: Integer; msg: TMessageInfo;
begin
    if mIMAPConn.conn.imap.SelectFolder(mFullMailboxName) then begin
        case op of
          iopDeleteMsg, iopUndeleteMsg, iopDeleteExpungeMsg: flagStr:='\Deleted';
          iopSeenMsg, iopUnseenMsg: flagStr:='\Seen';
          iopFlagMsg, iopUnflagMsg: flagStr:='\Flagged';
          iopAnsweredMsg, iopUnansweredMsg: flagStr:='\Answered';
        else
            begin
                devLog.Error('Error - bad operation in SetFlagOnSelectedMsgs');
            end;
        end;

        for i:=0 to mMsgUIDs.Count-1 do begin
            if setFlag then command:='UID STORE ' + mMsgUIDs[i] + ' +FLAGS.SILENT ('+flagStr+')'
            else command:='UID STORE ' + mMsgUIDs[i] + ' -FLAGS.SILENT ('+flagStr+')';
            mIMAPConn.conn.imap.IMAPCommand(command);
            //fullReply := mIMAPConn.conn.imap.FullResult;

            // Update the message info in messages list
            msgIndex:=mMessageLists.GetMessageInfoByUID(msg,mDisplayedNodeIndex,mMsgUIDs[i]);
            if (msgIndex<>-1) then begin
                case op of
                  iopDeleteMsg, iopUndeleteMsg, iopDeleteExpungeMsg: msg.mFlags.mDeleted:=setFlag;
                  iopSeenMsg, iopUnseenMsg: msg.mFlags.mSeen:=setFlag;
                  iopFlagMsg, iopUnflagMsg: msg.mFlags.mFlagged:=setFlag;
                  iopAnsweredMsg, iopUnansweredMsg: msg.mFlags.mAnswered:=setFlag;
                end;
                mMessageLists.UpdateMessageInfo(mDisplayedNodeIndex,msgIndex,msg);
            end;
        end;
        Synchronize(UpdateMessageList);
    end
    else HandleErrorSituation('Flagging failure');
end;

procedure TIMAPWorker.ExpungeMailbox;
var newSize, newMsgCount, numMessages: Integer; mMsgInfos: TListOfMessageInfos; res: String;
begin
    devLog.Info('Expunging folder: '+mboxTree.Items[mSelectedMailboxIndex].mFullMailboxName);
    // devLog.Info('Displayed folder: '+mboxTree.Items[mDisplayedNodeIndex].mFullMailboxName);

    if mIMAPConn.conn.imap.SelectFolder(mFullMailboxName) then begin
        if mIMAPConn.conn.imap.ExpungeFolder then begin
            // Here you could select the folder again to get the new folder size,
            // but that wouldn't get us the folder size
            // mIMAPConn.conn.imap.SelectFolder(mFullMailboxName)
            if mMessageLists.HasDataForRecover then begin
                devLog.Info('Calculating new size and msg count based on simulated expunge');
                newSize:=mMessageLists.SimulateExpunge(mSelectedMailboxIndex);
                newMsgCount:=mMessageLists.GetMailboxLength(mSelectedMailboxIndex);
            end
            else begin
                // We don't have all the messages in the client
                // (happens on a GlobalSearch without previous data)
                // so we need to recalculate the size and msg count.
                // Simulation doesn't work here because non-searched messages
                // could have been expunged.

                devLog.Info('Calculating new size and msg count based real mailbox fetch');

                mMessageLists.RecoverOriginalSearched;

                // Zeroes are default values in case things go wrong...
                newSize:=0;
                newMsgCount:=0;
                if mIMAPConn.conn.imap.SelectFolder(mFullMailboxName) then begin

                    numMessages:=mIMAPConn.conn.imap.SelectedCount;
                    SetLength(mMsgInfos,numMessages);

                    try
                        if (fetchMessageInfos(mMsgInfos,numMessages,false,false)) then begin
                            mMessageLists.AddMessageList(mMsgInfos, mSelectedMailboxIndex);
                            newSize:=mMessageLists.GetMailboxSize(mSelectedMailboxIndex);
                            newMsgCount:=mMessageLists.GetMailboxLength(mSelectedMailboxIndex);
                        end
                        else begin
                            requestMgr.SetSummary(ThreadInfo.RequestID,'Mailbox expunged, but cannot determine new mailbox size (Error was: Cannot fetch messages). Please recheck size of the full account to obtain accurate data.',mlvWarn);
                        end;
                    finally
                        SetLength(mMsgInfos,0);
                    end;
                end
                else begin
                    requestMgr.SetSummary(ThreadInfo.RequestID,'Mailbox expunged, but cannot determine new mailbox size (Error was: Could not select mailbox). Please recheck size of the full account to obtain accurate data.',mlvWarn);
                end;
            end;
            mboxTree.EnterCS;
            mboxTree.Items[mSelectedMailboxIndex].mSize:=newSize;
            mboxTree.Items[mSelectedMailboxIndex].mNumMessages:=newMsgCount;
            mboxTree.LeaveCS;
            Synchronize(UpdateTreeView);
            if mSelectedMailboxIndex=mDisplayedNodeIndex then begin
                Synchronize(PopulateMessageList);
            end;
            UpdateQuota;
        end
        else HandleErrorSituation('Expunge failure');
    end
    else HandleErrorSituation('Expunge failure. Can''t select folder.');
end;

procedure TIMAPWorker.DeleteAndExpungeMailbox;
var res: String;
begin
    // Perform select to check if we are connected
    if mIMAPConn.conn.imap.SelectFolder(mFullMailboxName) then begin
        PerformDelete(true,false);
        mIMAPConn.conn.imap.ExpungeFolder;
        mboxTree.EnterCS;
        mboxTree.Items[mSelectedMailboxIndex].mSize:=0;
        mboxTree.Items[mSelectedMailboxIndex].mNumMessages:=0;
        mboxTree.LeaveCS;
        Synchronize(UpdateTreeView);
        if mSelectedMailboxIndex=mDisplayedNodeIndex then begin
            mMessageLists.ClearMailbox(mSelectedMailboxIndex);
            Synchronize(ClearMessageList);
        end;
        UpdateQuota;
    end
    else HandleErrorSituation('Delete/Expunge failure');
end;

{ Retrieves and processes information about all messages in a mailbox }
procedure TIMAPWorker.getMboxMsgInfo;
var numMessages: Integer;
    mMsgInfos: TListOfMessageInfos;
    res: String;
begin
    devLog.Info('Getting messages in mailbox: '+mFullMailboxName);
    if mIMAPConn.conn.imap.SelectFolder(mFullMailboxName) then begin
        numMessages:=mIMAPConn.conn.imap.SelectedCount;
        SetLength(mMsgInfos,numMessages);

        try
            if (fetchMessageInfos(mMsgInfos,numMessages,true,ENABLE_PAGING)) then begin
                // Messages information for this mailbox is now added to msgInfos
                devLog.Info('Messages (headers) retrieved: '+IntToStr(numMessages));
                // Add messages to message lists
                mMessageLists.AddMessageList(mMsgInfos, mSelectedMailboxIndex);
            end
            else if (numMessages=0) then
                requestMgr.SetSummary(ThreadInfo.RequestID,'There are no messages in the selected mailbox!',mlvInfo)
            else
                requestMgr.SetSummary(ThreadInfo.RequestID,'An error occurred while fetching messages from the server. If the problem persists please contact the author.',mlvWarn);
        finally
            SetLength(mMsgInfos,0);
        end;
    end
    else HandleErrorSituation('Fetch failure');
end;

{ Retreives messages from the mMsgUIDs list }
procedure TIMAPWorker.getMultipleMsgInfos;
var mMsgInfos: TListOfMessageInfos; success: Boolean; i: Integer; res: String;
begin
    success:=true;
    devLog.Info('retrieving msgs from global search for mbox: '+mFullMailboxName);
    if mIMAPConn.conn.imap.SelectFolder(mFullMailboxName) then begin
        SetLength(mMsgInfos,mMsgUIDs.Count);
        try
            for i:=0 to mMsgUIDs.Count-1 do begin
                if not getMsgInfo(mMsgInfos[i],mMsgUIDs[i]) then begin
                    success:=false;
                    devLog.Warn('Unable to retrieve msg with uid: '+mMsgUIDs[i]);
                end;
            end;
            mMessageLists.AddGlobalSearchResultsToMessageLists(mMsgInfos, mSelectedMailboxIndex);
            Synchronize(PopulateMessageList);
        finally
            SetLength(mMsgInfos,0);
            if not success then begin
                requestMgr.SetSummary(ThreadInfo.RequestID,'Some messages could not be retrieved!',mlvWarn);
            end;
        end;
    end
    else HandleErrorSituation('Fetch failure');
end;

{ Retrieves the msgInfo of the message with the specified uid from the current mailbox }
{ Returns true if the operation succeded }
{ Specify '*' if you want to get the message with the highest uid }
function TIMAPWorker.getMsgInfo(var msgInfo: TMessageInfo; uid: String): Boolean;
var hasSpamHandle: Boolean;
    spamHandleInfo: TSpamHandleInfo;
    spamScoreHeader, command: String;
    fullReply: TstringList;
    success: Boolean;
begin
    success:=false;
    if mIMAPConn.conn.imap.SelectFolder(mFullMailboxName) then begin

        spamScoreHeader:='';
        hasSpamHandle:=false;
        if (mIMAPConn.conn.account.SpamHandle>0) then begin
            hasSpamHandle:=true;
            spamHandleInfo:=settings.SpamHandles.GetSpamHandle(mIMAPConn.conn.account.SpamHandle-1);  // -1 for none
            spamScoreHeader:=spamHandleInfo.SpamHeader;
        end;

        // If caller specified '*' as the uid, fetch the last message in the mbox
        // check RFC 3501 / section 6.4.8 (page 60)
        // UID FETCH <a big number>:* will fetch the last one if none exist in the range
        // Also note that in rfc 3501 uid is an unsigned 32 bit (LongWord in Delphi),
        // however, some servers use signed 32 bit so we are using it too (LongInt in Delphi)
        if uid='*' then uid := IntToStr(High(LongInt)) + ':' + uid;

        command := 'UID FETCH '+uid+' (FLAGS UID RFC822.SIZE ENVELOPE BODY.PEEK[HEADER.FIELDS (CONTENT-TYPE)]';
        if hasSpamHandle then command:=command+' BODY.PEEK[HEADER.FIELDS ('+spamScoreHeader+')]';
        command:=command+')';

        // appLog.Info(command);
        mIMAPConn.conn.imap.IMAPCommand(command);
        fullReply := TStringList.Create;

        reformatReply(fullReply, mIMAPConn.conn.imap.FullResult);
        // Note: There is only one fetch reply here
        try
            try
                msgInfo:=ProcessMessage(fullReply[0], spamHandleInfo, hasSpamHandle);
                success:=true;
            except
                // This is in case something goes wrong (e.g. IMAP server not setting a proper reply)
                devLog.warn('Exception while processing message in getMsgInfo');
                success:=false;
            end;
        finally
            fullReply.Free;
        end;
    end;
    // else success will be false...
    Result:=success;
end;

{ Method that performs the actual fetch for information on messages,
  parses it and places the information into the msgInfos array.
  Will use cached info if available and enabled
  @param notifyProgress A notification will be sent to update the progress bar after each message has been processed
  @param page True if the fetching should be paged (a message sent for updating the message view after x fetches }
function TIMAPWorker.fetchMessageInfos(var mMsgInfos: TListOfMessageInfos; numMessages: Integer; notifyProgress, page: Boolean): Boolean;
var fullReply, serverReply: TStringList; i: Integer;
    mMsgInfosCnt: Integer; command: String;
    hasSpamHandle: Boolean;
    spamHandleInfo: TSpamHandleInfo;
    spamScoreHeader: String;
    searchedMsgUIDs: TStringList;
    searchedFlags: TStringList;
    criteria: String;
    msgInfo: TMessageInfo;
    cachedMsgs, flagsHash: THashTable;
    cachedMsgPtr: PCachedMessage; flagsPtr: PMessageFlags;
    strTable: TStringTable;
    dummyObj: TObject;
    uid: LongInt;
    allEmpty: Boolean;  // true if none of the cached message infos contains full information
    progressBarValue: Real;
    progressBarIncrement: Real;
    pageCount, lastMsgAdded: Integer;
    firstFill: Boolean;     // Used for determining when the gui list should be cleared (for paging)

    procedure PrepareProgressBar;  // Prepares the progress bar stuff for fetching
    begin
        PostMessage(requestMgr.Handle, WM_MAIN_PROGRESS_BAR_VISIBILITY,ThreadInfo.requestID, 1);
        progressBarValue:=0;
        if numMessages<>0 then progressBarIncrement := 100/numMessages
        else progressBarIncrement:=100;
        PostMessage(requestMgr.Handle, WM_PROGRESS_BAR_UPDATE, ThreadInfo.requestID, Trunc(progressBarValue));
    end;

    // Called whenever a message is added to mMsgInfos
    procedure ProcessMessageAddition;
    begin
        Inc(pageCount);
        if notifyProgress then begin
            progressBarValue:=progressBarValue+progressBarIncrement;
            PostMessage(requestMgr.Handle, WM_PROGRESS_BAR_UPDATE, ThreadInfo.requestID, Trunc(progressBarValue));
        end;
    end;

    procedure GetAllFromServer(updateCache: Boolean);  // Gets all the messages from the server
    var i: Integer;
    begin
        firstFill:=true;
        serverReply := TStringList.Create;
        try
            if notifyProgress then PrepareProgressBar;
            mMsgInfosCnt := 0;
            pageCount := 0; lastMsgAdded:=-1;
            for i:=1 to numMessages do begin
                if not terminated then begin
                    serverReply.Clear;
                    command := 'FETCH '+IntToStr(i)+' (FLAGS UID RFC822.SIZE ENVELOPE BODY.PEEK[HEADER.FIELDS (CONTENT-TYPE)]';
                    if hasSpamHandle then command:=command+' BODY.PEEK[HEADER.FIELDS ('+spamScoreHeader+')]';
                    command:=command+')';

                    if mIMAPConn.conn.imap.IMAPCommand(command)='OK' then begin
                        reformatReply(serverReply, mIMAPConn.conn.imap.FullResult);

                        mMsgInfos[mMsgInfosCnt]:=ProcessMessage(serverReply[0], spamHandleInfo, hasSpamHandle);
                        if updateCache then begin
                            new(cachedMsgPtr);
                            cachedMsgPtr^ := MessageInfoToCachedMessage(mMsgInfos[mMsgInfosCnt]);
                            cachedMsgs[StrToInt(mMsgInfos[mMsgInfosCnt].mUID)]:=cachedMsgPtr;
                        end;
                        Inc(mMsgInfosCnt);
                        ProcessMessageAddition;
                        if (page and (pageCount mod GlobalConstants.FETCH_PAGE_INCREMENT = 0)) then begin
                            // Page size reached - Need to update the message list in the gui
                            //dbg('***O '+Format('First %d, Last %d',[lastMsgAdded+1, pageCount]));
                            InvokeMessageListAddition(mSelectedMailboxIndex, lastMsgAdded+1, pageCount, firstFill);
                            lastMsgAdded:=pageCount;
                            firstFill:=false;
                        end;
                    end
                    else begin
                        devLog.Warn('Unable to fetch messages. Server replied: '+GetResultMessage); //@TODO
                        Result:=false;
                    end;
                end
                else UpdateStatusBar('Fetch aborted',STATUSBAR_MSGS);
            end;
            if not terminated then begin
                if page then
                    // Add any remaining messages to the list
                    InvokeMessageListAddition(mSelectedMailboxIndex, lastMsgAdded+1, pageCount,firstFill);
                if notifyProgress then
                    PostMessage(requestMgr.Handle, WM_PROGRESS_BAR_UPDATE, ThreadInfo.requestID, 100);
                if updateCache then mCache.SaveCachedFolderMsgs(mFullMailboxName, cachedMsgs);
            end;
        finally
            serverReply.Free;
            if notifyProgress then
                PostMessage(requestMgr.Handle, WM_MAIN_PROGRESS_BAR_VISIBILITY,ThreadInfo.requestID, 0);
        end;
    end;

begin
    Result:=true;
    mIMAPConn.conn.imap.UID := false;  //@todo gde ga vracas na true?
    spamScoreHeader:='';
    hasSpamHandle:=false;
    if (mIMAPConn.conn.account.SpamHandle>0) then begin
        hasSpamHandle:=true;
        spamHandleInfo:=settings.SpamHandles.GetSpamHandle(mIMAPConn.conn.account.SpamHandle-1);  // -1 for none
        spamScoreHeader:=spamHandleInfo.SpamHeader;
    end;

    if not settings.MiscSettings.EnableCaching then begin
        GetAllFromServer(false);
    end
    else begin
        // caching is enabled, use it and update it...
        searchedMsgUIDs:=TStringList.Create;
        searchedFlags:=TStringList.Create;
        cachedMsgs:=THashTable.Create;
        flagsHash:=THashTable.Create;
        mMsgInfosCnt := 0;
        try
            try
                // Get the uids of messages on server
                criteria := 'UID 1:*';
                SearchUIDs(criteria,searchedMsgUIDs);
                // Now get the cached messages
                mCache.LoadCachedFolderMsgs(mFullMailboxName,cachedMsgs);

                allEmpty:=true;
                cachedMsgPtr:=cachedMsgs.First;
                while cachedMsgPtr<>nil do begin
                    if cachedMsgPtr^.FullInfo then begin
                        allEmpty:=false;
                        break;
                    end
                    else cachedMsgPtr:=cachedMsgs.Next;
                end;

                if allEmpty then begin
                    GetAllFromServer(true);    // faster if there are no cached message infos
                end
                else begin
                    strTable:=TStringTable.Create;
                    dummyObj:=TObject.Create;
                    try
                        // Prepare flags lookup table. We will use this information for cached envelopes
                        // Set default values
                        for i:=0 to searchedMsgUIDs.Count-1 do begin
                            new (flagsPtr);
                            flagsPtr^.mSeen:=true;      // seen messages are more common
                            flagsPtr^.mDeleted:=false;
                            flagsPtr^.mFlagged:=false;
                            flagsPtr^.mAnswered:=false;
                            flagsPtr^.mDraft:=false;
                            flagsHash[StrToInt(searchedMsgUIDs[i])]:=flagsPtr;
                        end;
                        // Now get actual flags from the server
                        criteria:='NOT SEEN';
                        SearchUIDs(criteria,searchedFlags);
                        for i:=0 to searchedFlags.Count-1 do begin
                            PMessageFlags(flagsHash[StrToInt(searchedFlags[i])])^.mSeen:=false;
                        end;

                        criteria:='DELETED';
                        SearchUIDs(criteria,searchedFlags);
                        for i:=0 to searchedFlags.Count-1 do PMessageFlags(flagsHash[StrToInt(searchedFlags[i])])^.mDeleted:=true;

                        criteria:='FLAGGED';
                        SearchUIDs(criteria,searchedFlags);
                        for i:=0 to searchedFlags.Count-1 do PMessageFlags(flagsHash[StrToInt(searchedFlags[i])])^.mFlagged:=true;

                        criteria:='ANSWERED';
                        SearchUIDs(criteria,searchedFlags);
                        for i:=0 to searchedFlags.Count-1 do PMessageFlags(flagsHash[StrToInt(searchedFlags[i])])^.mAnswered:=true;

                        // Prepare lookup table (msgs existing on the server)
                        for i:=0 to searchedMsgUIDs.Count-1 do
                            strTable.Items[searchedMsgUIDs[i]]:=dummyObj;

                        if notifyProgress then PrepareProgressBar;
                        pageCount := 0; lastMsgAdded:=-1;

                        // Go through the cached messages
                        cachedMsgPtr:=cachedMsgs.First;
                        while cachedMsgPtr<>nil do begin
                            uid:=cachedMsgPtr^.UID;
                            if strTable.Items[IntToStr(uid)] = nil then begin
                                // this uid doesn't exist anymore in the mailbox. remove from cache
                                cachedMsgPtr:=cachedMsgs.Remove(uid);
                                Dispose(cachedMsgPtr);
                            end
                            else begin
                                // message exists on server. sync if needed
                                if not cachedMsgPtr^.FullInfo then begin
                                    // we have partial information cached. need to get the whole message
                                    getMsgInfo(msgInfo, IntToStr(uid));
                                    cachedMsgPtr^ := MessageInfoToCachedMessage(msgInfo);
                                    msgInfo.mIsSpam := isSpam(msgInfo.mSpamScore, spamHandleInfo);
                                    mMsgInfos[mMsgInfosCnt]:=msgInfo;
                                    ProcessMessageAddition;
                                end
                                else begin
                                    cachedMsgPtr^.Flags:=PMessageFlags(flagsHash[uid])^;
                                    mMsgInfos[mMsgInfosCnt]:=CachedMesageToMessageInfo(cachedMsgPtr^);
                                    mMsgInfos[mMsgInfosCnt].mIsSpam := isSpam(mMsgInfos[mMsgInfosCnt].mSpamScore, spamHandleInfo);
                                    ProcessMessageAddition;
                                end;
                                Inc(mMsgInfosCnt);
                            end;
                            cachedMsgPtr:=cachedMsgs.Next;
                        end;

                        // We were not paging cached information, if there is more messages than the
                        // paging size, add them to the gui list now
                        if page then
                            if pageCount >= GlobalConstants.FETCH_PAGE_INCREMENT then begin
                                InvokeMessageListAddition(mSelectedMailboxIndex, lastMsgAdded+1, pageCount, firstFill);
                                lastMsgAdded:=pageCount;
                                firstFill:=false;
                            end;

                        // So far we have populated the cache with full information,
                        // but only the messages that were already in the cache.
                        // Now go through messages on the server and download those that are not cached
                        for i:=0 to searchedMsgUIDs.Count-1 do begin
                            if not terminated then begin
                                if cachedMsgs[StrToInt(searchedMsgUIDs[i])] = nil then begin
                                    getMsgInfo(msgInfo, searchedMsgUIDs[i]);
                                    new(cachedMsgPtr);
                                    cachedMsgPtr^ := MessageInfoToCachedMessage(msgInfo);
                                    cachedMsgs[StrToInt(searchedMsgUIDs[i])]:=cachedMsgPtr;
                                    mMsgInfos[mMsgInfosCnt]:=msgInfo;
                                    Inc(mMsgInfosCnt);

                                    ProcessMessageAddition;
                                    if (page and (pageCount mod GlobalConstants.FETCH_PAGE_INCREMENT = 0)) then begin
                                        // Page size reached - Need to update the message list in the gui
                                        InvokeMessageListAddition(mSelectedMailboxIndex, lastMsgAdded+1, pageCount, firstFill);
                                        lastMsgAdded:=pageCount;
                                        firstFill:=false;
                                    end;
                                end;
                            end
                             else UpdateStatusBar('Fetch aborted',STATUSBAR_MSGS);
                        end;

                        if not terminated then begin
                            if page then
                                // Add any remaining messages to the list
                                InvokeMessageListAddition(mSelectedMailboxIndex, lastMsgAdded+1, pageCount, firstFill);

                            mCache.SaveCachedFolderMsgs(mFullMailboxName, cachedMsgs);
                        end;
                    finally
                        dummyObj.Free;
                        strTable.Free;  //@todo need to free individual items?
                        PostMessage(requestMgr.Handle, WM_MAIN_PROGRESS_BAR_VISIBILITY,ThreadInfo.requestID, 0);
                    end;
                end;
            except
                on E:ECacheException do begin
                    devLog.Warn('Error loading cache. Will clear cache and perform a full load from server',E);
                    mCache.ClearAccountCache(mIMAPConn.conn.account.name);
                    GetAllFromServer(true);
                end;
            end;
        finally
            searchedMsgUIDs.Free;
            searchedFlags.Free;
            if flagsHash<>nil then begin
                flagsPtr:=flagsHash.First;
                while (flagsPtr<>nil) do begin
                    Dispose(flagsPtr);
                    flagsPtr:=flagsHash.Next;
                end;
                flagsHash.Free;
            end;
            mCache.FreeCachedMessages(cachedMsgs);
        end;
    end
end;

function TIMAPWorker.IsSpam(spamScore: String; var spamHandleInfo: TSpamHandleInfo): Boolean;
begin
    Result:=false;
    if SpamHandleInfo<>nil then begin
        if (spamHandleInfo.SpamComparedBy = 0) then begin
            // Spam header has a value (e.g. SpamAssasins' X-Spam-Score: 4.6)
            try
                Result := (StrToFloat(spamScore) > StrToFloat(spamHandleInfo.CompareValue));
            except
                Result := false;
            end;
        end
        else begin
            // Spam header has a substring that indicates spam
            // Message is spam if it contains the specified spamValue
            //@todo regexp
            Result := Pos(LowerCase(spamHandleInfo.CompareValue),LowerCase(spamScore))>0;
        end;
    end;
end;

{ Performs a search on the currently selected mailbox for the specified criteria.
  res contains a list of found UIDs }
procedure TIMAPWorker.SearchUIDs(criteria: String; var res: TStringList);
begin
    mIMAPConn.conn.imap.UID := true;
    res.Clear;
    try
        if not (mIMAPConn.conn.imap.SearchMess(criteria, TStrings(res))) then begin
            raise ECacheException.Create('Unable to perform search');
        end;
    finally
        mIMAPConn.conn.imap.UID := false;
    end;
end;


{ formats the reply (one FETCH command per line) }
{ if uidFetch is true, should strip the first UID from the command }
procedure TIMAPWorker.ReformatReply(var formattedReply:TStringList; serverReply: TStringList);
var formattedLine: String; i: Integer; fetchFound: Boolean;
begin
    fetchFound := false;
    formattedLine := '';
    i:=0;
    while (i<serverReply.Count) do begin
        if (Pos('FETCH (',serverReply[i])>0) then begin
            fetchFound := true;

            // Save the previous formatted line
            if Length(formattedLine)>0 then begin
                formattedReply.add(formattedLine);
                formattedLine := '';
            end;
            // Add the line
            formattedLine := formattedLine + serverReply[i];
        end
        else begin
            // The fetchFound checks if we are within the FETCH response
            // Solves the issue with servers sending messages prior to the FETCH response
            // e.g. S: * OK [PARSE] Unexpected characters at end of address:
            //        S: * 322 FETCH (FLAGS ...
            if fetchFound then
                formattedLine:=formattedLine + serverReply[i];
        end;
        i:=i+1;
    end;
    // Add last line
    if (fetchFound and (Length(formattedLine)>0)) then begin
        formattedReply.add(formattedLine);
    end;
end;

{ Processes a single fetch response (for one message)
  @param replyLine - the line containing the fetch reply for one message}
function TIMAPWorker.ProcessMessage(replyLine: String;
            spamHandleInfo: TSpamHandleInfo; hasSpamHandle: Boolean): TMessageInfo;
var msgInfo: TMessageInfo;

    { Extracts the UID from a FETCH response (one line) }
    function getUID(line: String): String;
    var UID: String; p: Integer;
    begin
        p:=pos('UID ',line);   // Find beggining of UID
        p:=p+4;                // Find first character of UID value
        UID:='';
        while line[p]<>' ' do begin  // Add all strings until the next blank char
            UID:=UID+line[p];
            Inc(p);
        end;
        Result:=UID;
    end;

    { Extracts the size from a FETCH response }
    function getSize(line: String): Integer;
    var sizeStr: String; p: Integer;
    begin
        p:=pos('RFC822.SIZE ',line);   // Find beggining of RFC Size
        p:=p+12;                       // Find first character of RFC Size value
        sizeStr:='';
        while ((line[p]<>' ') and (line[p]<>')')) do begin  // Add all strings until the next blank char or ) //@todo use regex
            sizeStr:=sizeStr+line[p];
            Inc(p);
        end;
        Result:=StrToInt(sizeStr);
    end;

    procedure fillFlagsDetails(var msgInfo: TMessageInfo; line: String);
    begin
        msgInfo.mFlags.mSeen := (pos('\Seen',line)>0);
        msgInfo.mFlags.mAnswered := (pos('\Answered',line)>0);
        msgInfo.mFlags.mDeleted := (pos('\Deleted',line)>0);
        msgInfo.mFlags.mFlagged := (pos('\Flagged',line)>0);
        msgInfo.mFlags.mDraft := (pos('\Draft',line)>0);
    end;

    { Extracts ENVELOPE data from the FETCH response and fills msgInfo }
    procedure fillEnvelopeDetails(var msgInfo: TMessageInfo; line: String);
    var wholeEnv: String; p: Integer; env: TEnvelope;
    begin
        p := Pos('ENVELOPE ',line);

        if p>0 then begin
            wholeEnv := Copy(line,p,Length(line)-p);  // Will start from openning brace after ENVELOPE
            env := TEnvelope.Create;
            try
                if env.ParseEnvelope(wholeEnv) then begin
                    try
                        msgInfo.mDate := DecodeRfcDateTime(env.mDate);
                    except
                        // On error, set to start of time
                        devLog.Warn('Could not parse date: '+env.mDate);
                        msgInfo.mDate := 0;
                    end;
                    msgInfo.mDateStr := DateTimeToStr(msgInfo.mDate);  // Ako bude trebalo formatirano, koristi DateTimeToString
                    msgInfo.mSubject := env.mSubject;
                    msgInfo.mFrom := env.GetFromField;
                    msgInfo.mTo := env.GetToField;
                end
                else begin
                    devLog.Error('Error while parsing envelope');
                end;
            finally
                env.Free;
            end;
        end;
    end;

    { Extracts spam information from the FETCH response and sets data in msgInfo }
    { @param spamHandleInfo Represents SpamHeaderInfo for current account }
    procedure getSpamInfo(var msgInfo: TMessageInfo; line: String; var spamHandleInfo: TSpamHandleInfo);
    var p: Integer; spamLineValue: String;
    begin
        msgInfo.mIsSpam := false;  // Default
        msgInfo.mSpamScore := '-';
        //@todo mozda treba da trazis malo sire (npr. HEADER.FIELDS etc)
        p := Pos(LowerCase(spamHandleInfo.SpamHeader)+': ',LowerCase(line));
        if (p>0) then begin
            p:=p+Length(spamHandleInfo.SpamHeader)+2;  // 2 from ': '
            spamLineValue := Copy(line,p,Length(line)-p);  //@todo ovde -1 ili -2 (da obrises zadnji ) )
            spamLineValue := Trim(spamLineValue);

            if (spamHandleInfo.SpamComparedBy = 0) then begin
                devLog.Trace('Compared by value');
                // Spam header has a value (e.g. SpamAssasins' X-Spam-Score: 4.6)
                msgInfo.mSpamScore := spamLineValue;
                try
                    msgInfo.mIsSpam := (StrToFloat(msgInfo.mSpamScore) > StrToFloat(spamHandleInfo.CompareValue));
                except
                    devLog.Error('Error with spam header: '+line);
                    msgInfo.mIsSpam := false;
                end;
            end
            else begin
                devLog.Trace('Compared by string');
                // Spam header has a substring that indicates spam
                if spamLineValue<>'' then
                    msgInfo.mSpamScore := spamLineValue
                else
                    msgInfo.mSpamScore := '-';
                // Message is spam if it contains the specified spamValue
                //@todo regexp
                msgInfo.mIsSpam := Pos(LowerCase(spamHandleInfo.CompareValue),LowerCase(spamLineValue))>0;
            end;
        end;
    end;

    { Inspects the Content-Type header and sets the attachment field in msgInfo }
    { We are assuming that the message doesn't have attachments if the content type
      is text/... or multipart/alternative. All other content types signal an
      attachment. If a message doesn't contain the content type header it is
      assumed to be text/plain (RFC 2045 and 2046) }
    procedure fillAttachmentInfo(var msgInfo: TMessageInfo; line: String);
    var p: Integer; ctLineValue: String;
    begin
        devLog.Trace('Filling attachment info');
        msgInfo.mHasAttachments:=false;  // default
        p := Pos('content-type'+': ',LowerCase(line));
        if (p>0) then begin
            p:=p+12+2;  // 2 from ': ', 12 = Length('content-type')
            ctLineValue := Copy(line,p,Length(line)-p);  //@todo ovde -1 ili -2 (da obrises zadnji ) )
            ctLineValue := Trim(ctLineValue);
            p:=Pos('text',LowerCase(ctLineValue));
            if p<>1 then begin
                p:=Pos('multipart/alternative',LowerCase(ctLineValue));
                if p<>1 then begin
                    msgInfo.mHasAttachments:=true;
                end;
                // else: it is multipart/alternative
            end;
            // else: it is text/ -> no attachments
        end;
        // else: no content-type -> text/plain
        devLog.Trace('MSG Has attachments: '+BoolToStr(msgInfo.mHasAttachments));
    end;

begin
    devLog.Trace('ReplyLine: '+replyLine);
   // reformatReply makes sure all lines contain a FETCH response
    msgInfo.mUID := getUID(replyLine);
    msgInfo.mSize := getSize(replyLine);
    msgInfo.mSizeDisplay := formatSizeDisplay(msgInfo.mSize);

    fillFlagsDetails(msgInfo,replyLine);
    fillEnvelopeDetails(msgInfo,replyLine);
    fillAttachmentInfo(msgInfo,replyLine);
    // Set image
    msgInfo.mImageIndex:=GetMessageImageIndex(msgInfo.mSize,msgInfo.mHasAttachments);
    if hasSpamHandle then begin
        getSpamInfo(msgInfo,replyLine,spamHandleInfo);
    end
    else begin
        msgInfo.mIsSpam:=false;
        msgInfo.mSpamScore:='-';
    end;
    Result:=msgInfo;
end;

{ @param rawSearch TRUE if mSearchString is the actual command,
  FALSE if mSearchString is the text to search for
  @param reverted TRUE if messages NOT satisfying the search should be displayed }
procedure TIMAPWorker.PerformSearch(rawSearch: Boolean; reverted: Boolean);
var searchedMsgUIDs: TStringList; searchReply: String;
    command, res: String; fullReply: TStringList; i,j,p,msgIndex: Integer; msg: TMessageInfo;
    searchedMsgs: TListOfMessageInfos;
    searchedCount: Integer;
    found: Boolean;
begin
    if mIMAPConn.conn.imap.SelectFolder(mFullMailboxName) then begin
        // if it is a "quick search" add the necessary command prefix
        if (rawSearch) then
            command := mSearchString
        else
            command := 'UID SEARCH TEXT "'+mSearchString+'"';
        mIMAPConn.conn.imap.IMAPCommand(command);
        fullReply := mIMAPConn.conn.imap.FullResult;
        if (fullReply.Count>0) then begin
            searchReply := fullReply[0];  // This is a one-liner

            // Parse reply
            searchedMsgUIDs := TStringList.Create;
            try
                p:=Pos('SEARCH ',searchReply);
                if (p > 0) then begin
                    p:=p+7;
                    searchReply := Copy(searchReply,p,Length(searchReply)-p+1);
                    devLog.Debug('SEARCH REPLY UIDS: '+searchReply);
                    Split(searchReply,' ',searchedMsgUIDs);

                    // Recover any search backups (this is basically to cover the case
                    // when a search is made second consecutive time on the same mailbox
                    mMessageLists.RecoverOriginalSearched;

                    // Create a list of MsgInfos that form search results
                    if (not reverted) then begin
                        // Display messages of retreived UIDs
                        SetLength(searchedMsgs,searchedMsgUIDs.Count);
                        for i:=0 to searchedMsgUIDs.Count-1 do begin
                            msgIndex:=mMessageLists.GetMessageInfoByUID(msg,mSelectedMailboxIndex,searchedMsgUIDs[i]);
                            if (msgIndex<>-1) then begin
                                searchedMsgs[i]:=msg;
                            end;
                        end;
                    end
                    else begin
                        // Display messages not in found UIDs
                        searchedCount:=0;
                        SetLength(searchedMsgs,mMessageLists.GetMailboxLength(mSelectedMailboxIndex)-searchedMsgUIDs.Count);
                        for i:=0 to mMessageLists.GetMailboxLength(mSelectedMailboxIndex)-1 do begin
                            msg:=mMessageLists.GetMessageInfo(mSelectedMailboxIndex,i);
                            found:=false; j:=0;
                            while (not found) and (j<searchedMsgUIDs.Count) do begin
                                if msg.mUID=searchedMsgUIDs[j] then found:=true
                                else Inc(j);
                            end;
                            if not found then begin
                                searchedMsgs[searchedCount]:=msg;
                                Inc(searchedCount);
                            end;
                        end;
                    end;
                    mMessageLists.AddSearchResultsToMessageLists(searchedMsgs, mSelectedMailboxIndex);

                    // Display changes in the tree
                    Synchronize(PopulateMessageList);
                end
                else begin
                    // No UIDs with the specified search. If reverted, leave messages as they are
                    // Just recover any originals...
                    if not reverted then requestMgr.SetSummary(ThreadInfo.RequestID,'No messages found with the specified search!',mlvInfo)
                    else begin
                        mMessageLists.RecoverOriginalSearched;
                        Synchronize(PopulateMessageList);
                    end;
                end;
            finally
                searchedMsgUIDs.Free;
                SetLength(searchedMsgs,0);
            end;
        end
        else begin
            requestMgr.SetSummary(ThreadInfo.RequestID,'There was no reply to the search command. Possible syntax error.'+chr(10)+chr(10)+
                'Command sent: '+command,mlvWarn);
        end;
    end
    else HandleErrorSituation('Search failure');
end;

procedure TIMAPWorker.PerformGlobalSearch(rawSearch: Boolean);
var command, fullMailboxName, searchReply: String;
    i,p, mboxIndex, failCount: Integer; fullReply, searchedMsgUIDs: TStringList;
begin

    devLog.Info('Perform global search starting');
    failCount:=0;
    if (rawSearch) then begin
        command := mSearchString;
    end
    else command := 'UID SEARCH TEXT "'+mSearchString+'"';

    // Try selecting the first folder to see if the connection is active
    // NOTE: This assumes the gui checks that there are folders selected for the global search...
    FMain.FGlobalSearch.SearchResult.ErrorOccurred:=false;
    i:=0;
    if (mIMAPConn.conn.imap.SelectFolder(mboxTree.Items[FMain.FGlobalSearch.SearchResult[i].MailboxIndex].mFullMailboxName)) then begin
        for i:=0 to FMain.FGlobalSearch.SearchResult.getLength-1 do begin
            devLog.Debug('Processing '+IntToStr(i));
            mboxIndex:=FMain.FGlobalSearch.SearchResult[i].MailboxIndex;
            fullMailboxName := mboxTree.Items[mboxIndex].mFullMailboxName;
            if (mIMAPConn.conn.imap.SelectFolder(fullMailboxName)) then begin
                devLog.Debug('Global search. Retreiving for mbox: '+fullMailboxName);
                mIMAPConn.conn.imap.IMAPCommand(command);
                fullReply := mIMAPConn.conn.imap.FullResult;
                if (fullReply.Count>0) then begin
                    searchReply := fullReply[0];  // This is a one-liner

                    // Parse reply
                    searchedMsgUIDs := TStringList.Create;
                    try
                        p:=Pos('SEARCH ',searchReply);
                        if (p > 0) then begin
                            p:=p+7;
                            searchReply := Copy(searchReply,p,Length(searchReply)-p+1);
                            devLog.Debug('SEARCH REPLY UIDS: '+searchReply);
                            Split(searchReply,' ',searchedMsgUIDs);

                            // Recover any search backups (this is basically to cover the case
                            // when a search is made second consecutive time on the same mailbox
                            //mMessageLists.RecoverOriginalSearched;

                            // Add to globalsearch results
                            FMain.FGlobalSearch.SearchResult.setUIDs(i,searchedMsgUIDs);
                            PostMessage(FMain.FGlobalSearch.Handle, WM_GLOBAL_SEARCH_UPDATE, i, 0);
                        end
                        else begin
                            // No UIDs with the specified search.
                        end;
                    finally
                        searchedMsgUIDs.Free;
                    end;
                end
                else begin
                    // No reply to the search command. Increment failure count
                    Inc(failCount);
                end;
            end
            else begin
                // Couldn't select folder, continuing...
                devLog.Warn('Couldn''t select folder '+ fullMailboxName);
            end;
        end;
        // If all searches have failed (no reply to any of them)
        if (failCount = FMain.FGlobalSearch.SearchResult.getLength) then begin
            requestMgr.SetSummary(ThreadInfo.RequestID,'There was no reply to the search command on any of the mailboxes. Possible syntax error.'+chr(10)+chr(10)+
                'Command sent: '+command,mlvWarn);
            FMain.FGlobalSearch.SearchResult.ErrorOccurred:=true;
        end;
    end
    else begin
        if (mAttemptCount = 1) then begin
            mReloginAndTryAgain := true;
        end
        else begin
            requestMgr.SetSummary(ThreadInfo.RequestID,'Global Search Failure: '+NO_CONNECTION_MSG,mlvError);
            FMain.FGlobalSearch.SearchResult.ErrorOccurred:=true;
        end;
    end;
end;

procedure TIMAPWorker.PerformCopy;
var i, msgIndex: Integer; command, res: String; msg: TMessageInfo; ok: Boolean;
begin
    ok:=true;
    if mIMAPConn.conn.imap.SelectFolder(mFullMailboxName) then begin
        for i:=0 to mMsgUIDs.Count-1 do begin
            command:='UID COPY ' + mMsgUIDs[i] + ' "' + mDestFullMailboxName + '"';
            if (mIMAPConn.conn.imap.IMAPCommand(command) = 'OK') then begin
                msgIndex:=mMessageLists.GetMessageInfoByUID(msg,mDisplayedNodeIndex,mMsgUIDs[i]);
                mboxTree.EnterCS;
                mboxTree.Items[mDestMailboxIndex].mSize:=mboxTree.Items[mDestMailboxIndex].mSize + msg.mSize;
                mboxTree.Items[mDestMailboxIndex].mNumMessages:=mboxTree.Items[mDestMailboxIndex].mNumMessages + 1;
                mboxTree.LeaveCS;

                if (mOperation=iopCopyDelete) or (mOperation=iopCopyDelExpunge) then begin
                    // Mark message deleted
                    command:='UID STORE ' + mMsgUIDs[i] + ' +FLAGS.SILENT (\Deleted)';
                    mIMAPConn.conn.imap.IMAPCommand(command);
                    msg.mFlags.mDeleted:=true;
                    mMessageLists.UpdateMessageInfo(mDisplayedNodeIndex,msgIndex,msg);
                end;
            end
            else begin
                devLog.Warn('Unable to copy message: '+mMsgUIDs[i]);
                requestMgr.LogError(ThreadInfo.RequestID,'ERROR copying message UID='+mMsgUIDs[i]);
                ok:=false;
            end;
        end;

        if (mOperation = iopCopyDelExpunge) then begin
            // Expunge will update both the tree and list view
            ExpungeMailbox;
        end
        else begin
            // Update both message list and tree view
            Synchronize(UpdateTreeView);
            Synchronize(UpdateMessageList);
            UpdateQuota;
        end;

        if not ok then requestMgr.SetSummary(ThreadInfo.RequestID,'Some messages could not be copied!',mlvWarn);
    end
    else HandleErrorSituation('Copy failure');
end;

procedure TIMAPWorker.DeleteMbox;
var res: String;
begin
    devLog.Info('Deleting folder '+mFullMailboxName);
    res := mIMAPConn.conn.imap.IMAPcommand('DELETE "' + mboxTree.Items[mSelectedMailboxIndex].mFullMailboxName + '"');
    if (res = 'OK') then begin
        RemoveDeletedMailboxFromNodeArray;
        mMessageLists.ClearMailbox(mSelectedMailboxIndex);
        mMessageLists.RemoveMailbox(mSelectedMailboxIndex);
        if (mSelectedMailboxIndex = mDisplayedNodeIndex) then begin
            Synchronize(ClearMessageList);
            mDisplayedNodeIndex:=-1;
        end
        else if mDisplayedNodeIndex>mSelectedMailboxIndex then Dec(mDisplayedNodeIndex);
        UpdateQuota;
    end
    else HandleErrorSituation('Delete failure');
end;

procedure TIMAPWorker.CreateMbox;
var res: String;
begin
    devLog.Info('Creating folder '+mFullMailboxName);
    res := mIMAPConn.conn.imap.IMAPcommand('CREATE "' + mFullMailboxName + '"');
    if (res = 'OK') then begin
        InsertNewMailboxToNodeArray(mFullMailboxName);
    end
    else if (res = 'NO') then begin
        requestMgr.SetSummary(ThreadInfo.RequestID,'Create failure: '+GetResultMessage+CRLF+CRLF+
            'If you are trying to create a subfolder of an existing folder note that '+
            'some IMAP servers do not allow this operation. ',mlvWarn);
    end
    else if (res = 'BAD') then begin
        requestMgr.SetSummary(ThreadInfo.RequestID,'Create failure: '+GetResultMessage,mlvError);
    end
    else begin
        if (mAttemptCount = 1) then begin
            mReloginAndTryAgain := true;
        end
        else begin
            requestMgr.SetSummary(ThreadInfo.RequestID,'Can''t create mailbox. '+NO_CONNECTION_MSG,mlvError);
        end;
    end;
end;

procedure TIMAPWorker.RenameMbox;
var res, command: String;
begin
    devLog.Info('Renaming full mailbox name from   '+mFullMailboxName+' to '+mNewFullMailboxName);
    devLog.Info('Renaming full displayed name from '+mboxTree.Items[mSelectedMailboxIndex].mFullDisplayedName+' to '+mNewFullDisplayedName);
    // Should rename this mailbox and all submailboxes
    command := 'RENAME "' + mFullMailboxName + '" "' + mNewFullMailboxName + '"';
    res := mIMAPConn.conn.imap.IMAPCommand(command);
    if (res = 'OK') then begin
        RenameMailboxes;
    end
    else HandleErrorSituation('Rename failure');
end;

{ Performs an append after for attachment deletion, invoke only on attachment deletion, header modification, etc }
procedure TIMAPWorker.AppendMessage;
var res: String; command, flags: String; foundFlags: Boolean; orgMsg, newMsgInfo: TMessageInfo;  msgIndex: Integer;
begin
    devLog.Info('sending append command to mbox '+mFullMailboxName);
    GetFlagsStringForAppend(mMsgFlags);
    res:=mIMAPConn.conn.imap.IMAPuploadCommand('APPEND "' + mFullMailboxName + '"' +flags, mMessageContent);

    if res = 'OK' then begin
        // Mark original message deleted
        devLog.Info('Marking original message as deleted');
        command:='UID STORE ' + mMsgUID + ' +FLAGS.SILENT (\Deleted)';

        if (mIMAPConn.conn.imap.IMAPCommand(command))='OK' then begin

            // Update the message info of the original message in messages list
            msgIndex:=mMessageLists.GetMessageInfoByUID(orgMsg,mSelectedMailboxIndex,mMsgUID);
            if (msgIndex<>-1) then begin
                orgMsg.mFlags.mDeleted:=true;
                mMessageLists.UpdateMessageInfo(mSelectedMailboxIndex,msgIndex,orgMsg);
            end;

            // Retrieve the new message from the server and add to the message list
            // '*' is the uid of the newest message in the folder
            if getMsgInfo(newMsgInfo, '*') then begin
                mMessageLists.AddMessageInfoToMessageList(mSelectedMailboxIndex,newMsgInfo);
            end
            else begin
                devLog.Warn('FAILED to retrieve new message');
            end;

            if mSelectedMailboxIndex = mDisplayedNodeIndex then
                    Synchronize(PopulateMessageList);

            UpdateQuota;

            requestMgr.SetSummary(ThreadInfo.RequestID,'A new message without attachments has been created. The original message has been marked ''Deleted''. Expunge mailbox to remove it completely.',mlvInfo);
        end
        else begin
            requestMgr.SetSummary(ThreadInfo.RequestID,'A new message without attachments has been created. FAILED to mark the original message ''Deleted''.',mlvWarn);
        end;
    end
    else HandleErrorSituation('Append failure');
end;

{ Uploads a mbox file.
  @todo This is a quick solution, since we have to handle both dos & unix files
        So, here we're using a TStringList to do the job, should use file streams instead }
procedure TIMAPWorker.UploadMbox;
const
    progBarLinesStep = 10;  // number of lines for updating the progress bar
var mboxFileStrList, oneMail: TStringList; i: Integer; line: String;
    foundFrom, refreshQuota,refreshTree,refreshList: boolean;
    progressBarValue: Real;
    progressBarIncrement: Real;
begin
    // Only refresh tree on each message
    refreshQuota:=false;
    refreshTree:=true;
    refreshList:=true;

    mboxFileStrList:=TStringList.Create;
    mboxFileStrList.LoadFromFile(mFilename);
    oneMail:=TStringList.Create;
    foundFrom:=false;
    try
        try
            progressBarValue:=0;
            progressBarIncrement := (100*progBarLinesStep)/mboxFileStrList.Count;
            PostMessage(requestMgr.Handle, WM_MAIN_PROGRESS_BAR_VISIBILITY,ThreadInfo.requestID, 1);
            PostMessage(requestMgr.Handle, WM_PROGRESS_BAR_UPDATE, ThreadInfo.requestID, Trunc(progressBarValue));
            for i:=0 to mboxFileStrList.Count-1 do begin
                if not terminated then begin
                    line:=mboxFileStrList[i];
                    if Copy(line,1,5)='From ' then begin
                        if foundFrom then begin
                            // Contents of oneMail represent a message (located before this from line). Send
                            UploadWithErrorCatching(oneMail,refreshQuota,refreshTree,refreshList,'');
                        end;
                        foundFrom:=true;
                        oneMail.Clear;
                    end
                    else begin
                        // Not a from line, just add to oneMail (if foundFrom - if a mail message has started)
                        // First fix if it's a fake From line
                        if Copy(line,1,6)='>From ' then line:=Copy(line,2,length(line)-1);
                        oneMail.add(line);
                    end;
                end
                else begin
                    UpdateStatusBar('Upload aborted',STATUSBAR_MSGS);
                    requestMgr.SetSummary(ThreadInfo.RequestID,'Upload aborted. Some messages may have been uploaded to the server.',mlvInfo);
                    break;
                end;
                if (i mod progBarLinesStep = 0) then begin
                    progressBarValue:=progressBarValue+progressBarIncrement;
                    PostMessage(requestMgr.Handle, WM_PROGRESS_BAR_UPDATE, ThreadInfo.requestID, Trunc(progressBarValue));
                end;
            end;

            if not terminated then
                if foundFrom then UploadWithErrorCatching(oneMail,refreshQuota,refreshTree,refreshList,'');

            PostMessage(requestMgr.Handle, WM_PROGRESS_BAR_UPDATE, ThreadInfo.requestID, 100);

            // Refresh if not being refreshed after each message
            if not refreshTree then Synchronize(UpdateTreeView);
            if not refreshQuota then UpdateQuota;
            //@todo need to define behaviour for refreshing the list. Need that?

            if not terminated then
                requestMgr.SetSummary(ThreadInfo.RequestID,'Messages successfully uploaded',mlvInfo);

        except
            //@todo show info about the error log, etc if needed
            requestMgr.SetSummary(ThreadInfo.RequestID,'Can''t continue uploading messages: '+GetResultMessage,mlvError);
        end;
    finally
        oneMail.Free;
        mboxFileStrList.Free;
        PostMessage(requestMgr.Handle, WM_MAIN_PROGRESS_BAR_VISIBILITY,ThreadInfo.requestID, 0);
    end;
end;


procedure TIMAPWorker.UploadEMLs;
var msgContents: TStringList; i: Integer; refreshQuota,refreshTree,refreshList: Boolean;
    progressBarValue: Real;
    progressBarIncrement: Real;
begin
    // Only refresh tree on each message
    refreshQuota:=false;
    refreshTree:=true;
    refreshList:=true;

    msgContents := TStringList.Create;
    try
        try
            PostMessage(requestMgr.Handle, WM_MAIN_PROGRESS_BAR_VISIBILITY,ThreadInfo.requestID, 1);
            progressBarValue:=0;
            progressBarIncrement := 100/mFilenames.Count;
            PostMessage(requestMgr.Handle, WM_PROGRESS_BAR_UPDATE, ThreadInfo.requestID, Trunc(progressBarValue));

            for i:=0 to mFilenames.Count-1 do begin
                if not terminated then begin
                    msgContents.Clear;
                    msgContents.LoadFromFile(mFilenames[i]);
                    UploadWithErrorCatching(msgContents,refreshQuota,refreshTree,refreshList,'');
                end
                else begin
                    UpdateStatusBar('Upload aborted',STATUSBAR_MSGS);
                    requestMgr.SetSummary(ThreadInfo.RequestID,'Upload aborted. Some messages may have been uploaded to the server.',mlvWarn);
                    break;
                end;
                progressBarValue:=progressBarValue+progressBarIncrement;
                PostMessage(requestMgr.Handle, WM_PROGRESS_BAR_UPDATE, ThreadInfo.requestID, Trunc(progressBarValue));
            end;
            PostMessage(requestMgr.Handle, WM_PROGRESS_BAR_UPDATE, ThreadInfo.requestID, 100);
            if not refreshTree then Synchronize(UpdateTreeView);
            if not refreshQuota then UpdateQuota;


            Synchronize(UpdateMessageList);
            //@todo need to define behaviour for refreshing the list. Need that?

            if not terminated then
                requestMgr.SetSummary(ThreadInfo.RequestID,'Messages successfully uploaded',mlvInfo);

        except
            //@todo show info about the error log, etc if needed
            requestMgr.SetSummary(ThreadInfo.RequestID,'Can''t continue uploading messages: '+GetResultMessage,mlvError);
        end;
    finally
        PostMessage(requestMgr.Handle, WM_MAIN_PROGRESS_BAR_VISIBILITY,ThreadInfo.requestID, 0);
        msgContents.Free;
    end;
end;

{ Just a wrapper around UploadSingleNewMessage }
procedure TIMAPWorker.UploadWithErrorCatching(var msgContents: TStringList; refreshQuota,refreshTree,refreshList: Boolean; flags: String);
var i: Integer;
begin
    try
        UploadSingleNewMessage(msgContents,refreshQuota,refreshTree,refreshList,flags);
    except
        on ERecEr: ERecoverableError do begin
            // We can continue processing other messages
            devLog.Warn(ERecEr.message);
            requestMgr.LogError(ThreadInfo.RequestID,ERecEr.Message);
        end;
        // propagate other errors (EUnrecoverableError)
    end;
end;

{ Private method that uploads a single message to the server
  @param mailbox: Full name of the mailbox on the server
  @param msgContents: Contents of the message
  Following params are mainly for cases when not batch uploading:
  @param refreshQuota: true if the method should refresh the qouta
  @param refreshTree: true if the method should refresh the tree
  @param refreshList: true if the method should refresh the list
  @param the flags of the new message, this has to be ready for inclusion in the append command. Pass '' if you don't care about flags
  @throws Recovarable error, calling method can proceed with the next message upload
  @throws Unrecoverable error, calling method should abort the operation}
procedure TIMAPWorker.UploadSingleNewMessage(var msgContents: TStringList; refreshQuota,refreshTree,refreshList: Boolean; flags: String);
var res: String; var newMsgInfo: TMessageInfo;
begin
    devLog.Info('Uploading message to mbox '+mboxTree.Items[mSelectedMailboxIndex].mFullMailboxName);
    res:=mIMAPConn.conn.imap.IMAPuploadCommand('APPEND "' + mboxTree.Items[mSelectedMailboxIndex].mFullMailboxName + '"' +flags, msgContents);
    if res = 'OK' then begin
        devLog.Info('Message uploaded');

        // Update mailbox info
        mboxTree.EnterCS;
        mboxTree.Items[mSelectedMailboxIndex].mSize:=mboxTree.Items[mSelectedMailboxIndex].mSize+Length(msgContents.Text);
        mboxTree.Items[mSelectedMailboxIndex].mNumMessages:=mboxTree.Items[mSelectedMailboxIndex].mNumMessages+1;
        mboxTree.LeaveCS;

        if refreshTree then Synchronize(UpdateTreeView);
        if refreshQuota then UpdateQuota;
        if refreshList then begin
            // Retrieve the new message from the server and add to the message list
            // '*' is the uid of the newest message in the folder
            if getMsgInfo(newMsgInfo, '*') then begin
                mMessageLists.AddMessageInfoToMessageList(mSelectedMailboxIndex,newMsgInfo);
            end
            else begin
                devLog.Warn('FAILED to retrieve new message');
            end;
            if mSelectedMailboxIndex = mDisplayedNodeIndex then begin
                mIndexFirst:=mMessageLists.GetMailboxLength(mSelectedMailboxIndex)-1;
                mIndexLast:=mMessageLists.GetMailboxLength(mSelectedMailboxIndex)-1;
                Synchronize(AddMessagesToMessageList);
            end;
        end;
    end
    else if res = 'NO' then begin
        raise ERecoverableError.Create('Message not appended due to: '+GetResultMessage);
    end
    else if res = 'BAD' then begin
        raise ERecoverableError.Create('Message not appended due to: '+GetResultMessage);
    end
    else begin
        if (mAttemptCount = 1) then begin
            mReloginAndTryAgain := true;
        end
        else begin
            raise EUnrecoverableError.Create('Upload error: '+NO_CONNECTION_MSG);
        end;
    end;
end;

{ Will download messages specified in mMsgUIDs (or all messages in the
  FullMailboxName folder if mMsgUIDs are not set) to a single file
  in the mbox format. Filename is defined in mFilename.
  Msgs are downloaded as temporary emls, then converted to a mbox }
procedure TIMAPWorker.DownloadMbox;
var theFile: TFileStream;
    mboxFilename: String;
    converter: TConverter;
    emlFileList, dummyList: TStringList;
    i: Integer;
    options: TConvertOptions;
    msgsDownloaded, totalMsgs: Integer;
    summaryLvl: TMessageLevel;
begin
    devLog.Trace('Saving messages to mbox');
    mboxFilename:=mFilename;

    converter:=TConverter.Create;
    emlFileList:=TStringList.Create;
    dummyList:=TStringList.Create;
    try
        mFilename:=dataDir+'\temp';  // folder for temporary emls (used by downloadEMLs)
        if not DirectoryExists(mFilename) then ForceDirectories(mFilename);
        PostMessage(requestMgr.Handle, WM_MAIN_PROGRESS_BAR_VISIBILITY,ThreadInfo.requestID, 1);
        summaryLvl:=mlvInfo;
        DownloadEMLs(emlFileList,0,90,msgsDownloaded, totalMsgs, false, 0, dummyList, '', summaryLvl);
        if not terminated then begin
            options:=GetDefaultConversionOptions;
            options.overwrite:=false;
            options.exportProgress:=true;
            options.progressStart:=90;
            options.progressEnd:=100;
            if converter.eml2mbox(emlFileList,mboxFilename,options,TCustomThread(self)) then
                requestMgr.SetSummary(ThreadInfo.RequestID,IntToStr(msgsDownloaded)+'/'+IntToStr(totalMsgs)+' messages saved to '+mboxFilename,mlvInfo)
            else
                requestMgr.SetSummary(ThreadInfo.RequestID,'Failed to save messages to '+mboxFilename,mlvError);
        end;
    finally
        for i:=0 to emlFileList.Count-1 do
            if FileExists(emlFileList[i]) then DeleteFile(emlFileList[i]);
        dummyList.Free;
        emlFileList.Free;
        converter.Free;
        PostMessage(requestMgr.Handle, WM_MAIN_PROGRESS_BAR_VISIBILITY,ThreadInfo.requestID, 0);
    end;
end;

{ Just a convenient class for callers who don't care about
  the list of created files }
procedure TIMAPWorker.DownloadEMLs;
var dummyList: TStringList; d1,d2: Integer; summaryLvl: TMessageLevel;
begin
    dummyList := TStringList.Create;
    summaryLvl:=mlvInfo;
    DownloadEMLs(dummyList,0,100,d1,d2,false,0,dummyList,'',summaryLvl);
    dummyList.Free;
    PostMessage(requestMgr.Handle,WM_MAIN_PROGRESS_BAR_VISIBILITY,ThreadInfo.requestID,0);
end;

{ Another wrapper, used in backup mode }
procedure TIMAPWorker.DownloadEMLsBackupMode(backupFolderId: Integer; maxUID: String; var summaryLvl: TMessageLevel);
var dummyList: TStringList; d1,d2: Integer;
begin
    dummyList := TStringList.Create;
    DownloadEMLs(dummyList,0,100,d1,d2,true,backupFolderId,dummyList,maxUID,summaryLvl);
    dummyList.Free;
    PostMessage(requestMgr.Handle,WM_MAIN_PROGRESS_BAR_VISIBILITY,ThreadInfo.requestID,0);
end;

{ Will download messages specified in mMsgUIDs (or all messages in the
  mSelectedMailboxIndex folder if mMsgUIDs are not set) to multiple files
  with a eml extension. Names will be formed of message header (cnt+subject)
  @param var createdFiles List of created files
  @param msgsDownloaded Number of messages that were actually downloaded (some will be skipped if they already exist)
  @param totalMsgs Total number of messages checked for download
  @param backupFolderId Will be used for populating the database entry (backup mode only) }

{ ProgressBar visibility should be set by the caller }
procedure TIMAPWorker.DownloadEMLs(var createdFiles: TStringList; progressStart, progressEnd: Integer;
    var msgsDownloaded, totalMsgs: Integer; backupMode: Boolean;
    backupFolderId: Integer; var msgUIDsToSkip: TStringList; maxUID: String; var summaryLvl: TMessageLevel);

var theFile: TFileStream; i,n,p,pCRLF,pLF: Integer;
    line, command, commandMsgUIDCheck, res, s, messageUID, messageId: String;
    append, getAllMsgs, checkMsgUIds, skipMessage: Boolean;
    oneMail: TStringList;
    emlFilename, nowStr, subjectStr, fromStr, dateStr, msgNumStr, sMsg: String;
    invCharRegEx: TRegExpr;
    tempStr, createdFilename: String;
    progressBarValue: Real;
    progressBarIncrement: Real;
    uidFetched: Boolean;
    sqlStmt: String;
    backupFilenameFormat: TBackupFilenameFormat;
    expandedFilename: String;

    { Checks the specified header and returns it's value if found }
    function CheckString(header: String; lowCase: Boolean): String;
    begin
        res:='';
        if lowCase then
            p:=Pos(header,LowerCase(line))
        else
            p:=Pos(header,line);
        if p>0 then begin
            tempStr:=Copy(line,p+Length(header),256);
            pCRLF:=Pos(CRLF,tempStr);
            pLF:=Pos(LF,tempStr);
            if pCRLF>0 then res:=Copy(tempStr,1,pCRLF-1)
            else if pLF>0 then res:=Copy(tempStr,1,pLF-1);
        end;
        Result:=res;
    end;

    { Extracts the UID from a FETCH response (one line) }
    function getUID(line: String): String;
    var UID: String; p: Integer;
    begin
        p:=pos('UID ',line);   // Find beggining of UID
        p:=p+4;                // Find first character of UID value
        UID:='';
        while ((line[p]<>')') and (line[p]<>' ')) do begin  // Add all strings until the next ")" char
            UID:=UID+line[p];
            Inc(p);
        end;
        Result:=UID;
    end;

begin
    invCharRegEx:=TRegExpr.Create;
    invCharRegEx.Expression:=invCharRE; // const in Converter.pas
    devLog.Trace('Saving messages to emls');
    createdFiles.Clear;
    checkMsgUIds:=msgUIDsToSkip.Count>0;  // true if some messages should be skipped (list contains UIDs to skip)
    msgsDownloaded:=0;  // Counts the messages downloaded
    totalMsgs:=0;       // Counts the total messages checked for download
    theFile := nil;
    backupFilenameFormat:=TBackupFilenameFormat.Create(settings.MiscSettings.BackupFilenameFormat);

    if backupMode then backupLog.Info('===== Backup starting');

    try
        try
            if mIMAPConn.conn.imap.SelectFolder(mFullMailboxName) then begin

                getAllMsgs:=(mMsgUIDs.Count=0) and (maxUID='');
                if getAllMsgs then begin
                    // Either some messages have been specified for skipping, or a max uid has been specified.
                    // Add all IDs (not UIDs) to the mMsgUIDs list
                    // This eases handling of both cases
                    for i:=1 to mIMAPConn.conn.imap.SelectedCount do mMsgUIDs.Add(IntToStr(i));
                end
                else if (maxUID<>'') then begin
                    // find all messages after maxUID
                    SearchUIDs('UID '+IntToStr(StrToInt(maxUID)+1)+':*',mMsgUIDs);
                    // Remove the first element if it equals maxUID (happens if there are no messages with UID>maxUID)
                    if mMsgUIDs.Count>0 then
                        if mMsgUIDs[0]=maxUID then mMsgUIDs.Delete(0);
                end;

                progressBarValue:=progressStart;
                progressBarIncrement:=0;
                if mMsgUIDs.Count>0 then
                    progressBarIncrement := (progressEnd-progressStart)/mMsgUIDs.Count;
                PostMessage(requestMgr.Handle, WM_PROGRESS_BAR_UPDATE, ThreadInfo.requestID, Trunc(progressBarValue));

                totalMsgs:=mMsgUIDs.Count;

                oneMail:=TStringList.Create;
                try
                    oneMail.Clear;
                    for i:=0 to mMsgUIDs.Count-1 do begin

                        skipMessage:=false;
                        messageId:='';

                        if not terminated then begin
                            if backupMode then begin
                                if getAllMsgs then commandMsgUIDCheck:=''
                                else commandMsgUIDCheck:='UID ';
                                commandMsgUIDCheck := commandMsgUIDCheck + 'FETCH ' + mMsgUIDs[i] + ' (UID)';
                                if mIMAPConn.conn.imap.IMAPCommand(commandMsgUIDCheck)='OK' then begin
                                    uidFetched:=true;
                                    // Extract the UID from the response
                                    messageUID:=getUID(mIMAPConn.conn.imap.FullResult[0]);
                                    if checkMsgUIds then begin
                                        if msgUIDsToSkip.IndexOf(messageUID)>-1 then
                                            skipMessage:=true;
                                    end;
                                end
                                else begin
                                    uidFetched:=false;
                                    summaryLvl:=mlvWarn;
                                    requestMgr.LogError(ThreadInfo.requestID,'Command failed: '+commandMsgUIDCheck);
                                    if mCommandLineMode then backupLog.Error('Command failed: '+commandMsgUIDCheck);
                                end;
                            end
                            else uidFetched:=true;

                            if getAllMsgs then command:='' else command:='UID ';
                            command := command + 'FETCH ' + mMsgUIDs[i] + ' BODY.PEEK[]';

                            if skipMessage then begin
                                devLog.Trace('Message with UID '+messageUID+' already exists. Skipping');
                            end
                            else if not uidFetched then begin
                                devLog.Warn('Failed to fetch message UID for '+mMsgUIDs[i]+'. Skipping');
                            end
                            else if mIMAPConn.conn.imap.IMAPCommand(command)='OK' then begin
                                try
                                    // Create file to store this email
                                    DateTimeToString(nowStr,'yyyymmdd_hhnnss',Now);
                                    subjectStr:='';
                                    emlFilename:=mFilename+'\'+nowStr+'_'+mMsgUIDs[i];

                                    // Create the file for writting
                                    theFile := TFileStream.Create(emlFilename, fmCreate);
                                    theFile.Seek(0, soFromBeginning);

                                    for n := 0 to mIMAPConn.conn.imap.FullResult.Count - 2 do
                                        if mIMAPConn.conn.imap.FullResult[n][Length(mIMAPConn.conn.imap.FullResult[n])] = '}' then begin
                                            // This line is actually the whole message
                                            line := mIMAPConn.conn.imap.FullResult[n + 1];

                                            // Try to find the first occurrence of Subject: and set subjectStr
                                            subjectStr:=CheckString('subject: ',true);
                                            fromStr:=CheckString('From: ',false);
                                            msgNumStr:=IntToStr(i);
                                            dateStr:=CheckString('date: ',true);
                                            try
                                                DateTimeToString(dateStr,'yyyymmdd_hhnnss',DecodeRfcDateTime(dateStr));
                                            except
                                                dateStr:='Invalid_Date';
                                            end;

                                            // Try to find the Message-ID
                                            // We could have done this by doing a header peek, but this is way faster
                                            // @todo !!! Is it really faster? what if you're checking a 10MB message?
                                            if messageId='' then begin
                                                messageId:=CheckString('message-id: ',true);
                                            end;

                                            addLineToFile(theFile,line);
                                            Break;
                                        end;
                                        createdFilename:=emlFilename;
                                finally
                                    try
                                        if (theFile <> nil) then theFile.Destroy();
                                    except
                                        on Exception do begin
                                            summaryLvl:=mlvError;
                                            devLog.Error('ERROR downloading message with original UID= '+mMsgUIDs[i]);
                                            requestMgr.LogError(ThreadInfo.RequestID,'ERROR downloading message with original UID= '+mMsgUIDs[i]);
                                        end;
                                    end;
                                end;
                                // Rename file
                                if FileExists(emlFilename) then begin
                                    if subjectStr<>'' then
                                        subjectStr:=invCharRegEx.Replace(subjectStr,'_');
                                    if fromStr<>'' then
                                        fromStr:=invCharRegEx.Replace(fromStr,'_');

                                    expandedFilename:=backupFilenameFormat.ExpandTemplate(fromStr, subjectStr, dateStr, nowStr, msgNumStr);
                                    expandedFilename:=invCharRegEx.Replace(expandedFilename,'_');
                                    if RenameFile(emlFilename,mFilename+'\'+expandedFilename) then begin
                                        createdFilename:=mFilename+'\'+expandedFilename;
                                    end
                                    else devLog.Warn('Cannot rename '+emlFilename+' to: '+mFilename+'\'+expandedFilename);
                                end;
                                createdFiles.add(createdFilename);
                                if backupMode then begin
                                    try
                                        messageId:=StringReplace(messageId,'"','""',[rfReplaceAll]);
                                        sqlStmt := 'INSERT INTO message VALUES ('+IntToStr(backupFolderId)+','+messageUID+',"'+messageId+'","'+ExtractFilename(createdFilename)+'")';
                                        FMain.backupDb.Insert(sqlStmt);
                                        backupLog.Info('Saved message with UID ['+messageUID+'] to file ['+ExtractFilename(createdFilename)+']');
                                    except
                                        on E: Exception do begin
                                            summaryLvl:=mlvError;
                                            devLog.Error('Error inserting message info into the backup database. Reason: '+E.Message);
                                            devLog.Error('Statement was: '+sqlStmt);
                                            requestMgr.LogError(ThreadInfo.RequestID,'ERROR inserting message info into the backup database');
                                            if mCommandLineMode then begin
                                                backupLog.Error('ERROR inserting message info into the backup database. Reason: ');
                                                backupLog.Error('Statement was: '+sqlStmt);
                                            end;
                                            break;
                                        end;
                                    end;
                                end;
                                Inc(msgsDownloaded);
                            end
                            else begin
                                devLog.Warn('Server could not execute the command');
                                if mCommandLineMode then backupLog.Warn('Server could not execute the command: '+command);
                            end;
                            // Update progress bar
                            progressBarValue:=progressBarValue+progressBarIncrement;
                            PostMessage(requestMgr.Handle, WM_PROGRESS_BAR_UPDATE, ThreadInfo.requestID, Trunc(progressBarValue));
                        end
                        else begin
                            UpdateStatusBar('Fetch aborted',STATUSBAR_MSGS);
                            break;
                        end;
                    end;
                    theFile:=nil;
                    PostMessage(requestMgr.Handle, WM_PROGRESS_BAR_UPDATE, ThreadInfo.requestID, progressEnd);
                finally
                    oneMail.Free; oneMail:=nil;
                end;
                if not terminated then begin
                    if backupMode then begin
                        FMain.backupDb.UpdateLastBackedUp(backupFolderId);

                        if totalMsgs=0 then
                            sMsg:='No new messages in '+mFullMailboxName
                        else
                            sMsg:=IntToStr(msgsDownloaded)+' of '+IntToStr(totalMsgs)+' new messages in '+mFullMailboxName+' backed up to directory: '+mFilename;

                        requestMgr.Log(ThreadInfo.RequestID,sMsg);
                        if mCommandLineMode then backupLog.Info(sMsg);
                    end
                    else
                        requestMgr.SetSummary(ThreadInfo.RequestID,IntToStr(msgsDownloaded)+'/'+IntToStr(totalMsgs)+' messages saved to directory: '+mFilename,mlvInfo);
                end;
            end
            else begin
                HandleErrorSituation('Message download failure');
            end;

        except
            on E:Exception do begin
                requestMgr.LogError(ThreadInfo.requestID,'Partial backup failure: '+E.Message);
                devLog.Error('Partial backup failure: '+E.Message);
                if mCommandLineMode then backupLog.Error('Partial backup failure: '+E.Message);
            end;
        end;
        
    finally
        // Make sure the file is closed
        try
            if (theFile <> nil) then theFile.Destroy();
        except
            devLog.Error('Failed to close file');
        end;
        theFile := nil;
        invCharRegEx.Free;
        mMsgUIDs.Clear;
        if backupMode then backupLog.Info('===== Backup ending');
    end;
end;

{ Downloads emls which will be uploaded to a remote server by another thread.
  Sends the filenames to the queue of the master request which is running
  TIMAPWorker.UploadEMLsToRemoteServer }
procedure TIMAPWorker.DownloadEMLsForRemoteCopy;
var theFile: TFileStream; i,n: Integer;
    oneMail: TStringList;
    emlFilename, nowStr, command, line: String;
    progressBarValue: Real;
    progressBarIncrement: Real;
    request: TRequest;
begin
    devLog.Trace('Downloading emls for remote copy');
    if not DirectoryExists(mFilename) then ForceDirectories(mFilename);
    request := requestMgr.GetRequest(ThreadInfo.requestId);
    try
        if mIMAPConn.conn.imap.SelectFolder(mFullMailboxName) then begin
            PostMessage(requestMgr.Handle, WM_MAIN_PROGRESS_BAR_VISIBILITY,ThreadInfo.requestID, 1);
            progressBarValue:=0;
            progressBarIncrement:=0;
            if mMsgUIDs.Count>0 then progressBarIncrement := 100/mMsgUIDs.Count;
            PostMessage(requestMgr.Handle, WM_PROGRESS_BAR_UPDATE, ThreadInfo.requestID, Trunc(progressBarValue));
            oneMail:=TStringList.Create;
            try
                oneMail.Clear;
                for i:=0 to mMsgUIDs.Count-1 do begin
                    command:='UID FETCH ' + mMsgUIDs[i] + ' BODY.PEEK[]';
                    if not terminated then begin
                        if mIMAPConn.conn.imap.IMAPCommand(command)='OK' then begin
                            try
                                // Create file to store this email
                                DateTimeToString(nowStr,'yyyymmddhhnnss',Now);
                                emlFilename:=mFilename+'\UID'+mMsgUIDs[i]+'_'+nowStr;

                                // Create the file for writting
                                theFile := TFileStream.Create(emlFilename, fmCreate);
                                theFile.Seek(0, soFromBeginning);

                                for n := 0 to mIMAPConn.conn.imap.FullResult.Count - 2 do begin
                                    if mIMAPConn.conn.imap.FullResult[n][Length(mIMAPConn.conn.imap.FullResult[n])] = '}' then begin
                                        // This line is actually the whole message
                                        line := mIMAPConn.conn.imap.FullResult[n + 1];
                                        addLineToFile(theFile,line);
                                        Break;
                                    end;
                                end;
                            finally
                                if (theFile <> nil) then theFile.Destroy();
                            end;
                        end
                        else devLog.Warn('Server could not execute the command');
                        // Add filename to queue for master request
                        requestMgr.AddToStringQueue(request.data.masterRequestId,emlFilename);
                        // Update progress bar
                        progressBarValue:=progressBarValue+progressBarIncrement;
                        PostMessage(requestMgr.Handle, WM_PROGRESS_BAR_UPDATE, ThreadInfo.requestID, Trunc(progressBarValue));
                    end
                    else begin
                        UpdateStatusBar('Fetch aborted',STATUSBAR_MSGS);
                        break;
                    end;
                end;
                theFile:=nil;

                // If copy operation is tcoCopyMarkDeleted then mark the messages as deleted
                // Once the messages are uploaded, if there are any messages that failed, they
                // will be undeleted.
                if request.data.copyOperation = tcoCopyMarkDeleted then begin
                    SetFlagOnSelectedMsgs(true,iopUndeleteMsg);
                end;

                PostMessage(requestMgr.Handle, WM_PROGRESS_BAR_UPDATE, ThreadInfo.requestID, 100);
            finally
                requestMgr.AddToStringQueue(request.data.masterRequestId,GlobalConstants.STRING_FIFO_QUEUE_DONE_SIGNAL);
                oneMail.Free; oneMail:=nil;
            end;
            if not terminated then begin
                requestMgr.SetSummary(request.data.masterRequestId,'All messages saved to directory: '+mFilename,mlvInfo);
            end;
        end
        else begin
            HandleErrorSituation('Message download failure');
        end;
    finally
        //PostMessage(requestMgr.Handle, WM_MAIN_PROGRESS_BAR_VISIBILITY,ThreadInfo.requestID, 0);
    end;
end;

{ Uploads messages to the remote server. Checks the string queue
  for filenames which are placed there by TIMAPWorker.DownloadEMLsForRemoteCopy
  (another thread). Once the GlobalConstants.STRING_FIFO_QUEUE_DONE_SIGNAL
  string is found the process is terminated }
procedure TIMAPWorker.UploadEMLsToRemoteServer;
var request: TRequest; done: Boolean; filename: String; failedUIDs, msgContents: TStringList;
    res: String; totalCount, successCount, p, i: Integer;
    progressBarValue: Real;
    progressBarIncrement: Real;
    currentUID: String;

    { Extracts the UID from the filename which is of format <path>/UID<UID>_<date_string> }
    function extractUIDFromFilename(fn: String): String;
    var p1,p2: Integer; temp: String;
    begin
        p1:=pos('UID',fn);
        temp:=Copy(fn,p1+3,Length(fn));
        p2:=pos('_',temp);
        if p2>0 then Result:=Copy(temp,0,p2-1)
        else Result:='Unknown UID';
    end;

begin
    request := requestMgr.GetRequest(ThreadInfo.requestId);
    msgContents:=TStringList.Create;
    failedUIDs:=TStringList.Create;
    done:=false;
    totalCount:=request.data.msgUIDs.Count;
    successCount:=0;
    progressBarValue:=0;
    progressBarIncrement := 100/request.data.msgUIDs.Count;
    PostMessage(requestMgr.Handle, WM_MAIN_PROGRESS_BAR_VISIBILITY,ThreadInfo.requestID, 1);
    PostMessage(requestMgr.Handle, WM_PROGRESS_BAR_UPDATE, ThreadInfo.requestID, Trunc(progressBarValue));
    try
        while not (done or terminated) do begin
            if request.data.strQueue.Count>0 then begin
                // There is data in the queue, process it
                filename:=request.data.strQueue[0];
                // Remove first string from the list
                request.data.strQueue.Delete(0);
                // Check if a termination string has been received
                if filename=GlobalConstants.STRING_FIFO_QUEUE_DONE_SIGNAL then begin
                    done:=true;
                end
                else begin
                    // Send contents of the specified file to the remote server
                    msgContents.Clear;
                    try
                        // Get the original UID of the message
                        currentUID:=extractUIDFromFilename(filename);
                        // Remove it from the list of uids
                        p:=request.data.msgUIDs.IndexOf(currentUID);
                        if p>-1 then request.data.msgUIDs.Delete(p);
                        // Upload the message
                        msgContents.LoadFromFile(filename);
                        res:=mIMAPConn.conn.imap.IMAPuploadCommand('APPEND "' + mDestFullMailboxName + '"', msgContents);
                        if res = 'OK' then begin
                            requestMgr.Log(request.requestId,'Successfully uploaded message with original UID= '+currentUID);
                            Inc(successCount);
                        end
                        else begin
                            failedUIDs.Add(currentUID);
                            requestMgr.LogError(request.requestId,'ERROR uploading message with original UID= '+currentUID+'. Server response: "'+GetResultMessage+'"');
                        end;
                    except
                        // Couldn't open the file
                        failedUIDs.Add(currentUID);
                        requestMgr.LogError(request.requestId,'ERROR uploading message with original UID= '+currentUID+'. Unable to open file '+filename);
                    end;
                    if not Sysutils.DeleteFile(filename) then requestMgr.LogError(request.requestId,'ERROR deleting temporary file '+filename);

                    // Update progress bar
                    progressBarValue:=progressBarValue+progressBarIncrement;
                    PostMessage(requestMgr.Handle, WM_PROGRESS_BAR_UPDATE, ThreadInfo.requestID, Trunc(progressBarValue));
                end;
            end
            // No data in the queue, pause the thread for a second and check again
            // @todo detection of new items could be handled better (event notifications)...
            else sleep(1000);
        end;
    finally

        //PostMessage(requestMgr.Handle, WM_PROGRESS_BAR_UPDATE, ThreadInfo.requestID, 100);
        PostMessage(requestMgr.Handle, WM_MAIN_PROGRESS_BAR_VISIBILITY,ThreadInfo.requestID, 0);
        msgContents.Free;
        if successCount=totalCount then begin
            requestMgr.SetSummary(request.requestId,
                Format('All messages (%d) successfully copied to %s @ %s',[totalCount,mDestFullMailboxName,request.accountInfoPtr^.Name]),mlvInfo);
        end
        else begin
            // Add the list of files that were not even downloaded (if any)
            if request.data.msgUIDs.Count>0 then begin
                requestMgr.LogError(request.requestId,'===> Could not retrieve messages with following UIDs from the origin mailbox:');
                for i:=0 to request.data.msgUIDs.Count-1 do
                    requestMgr.LogError(request.requestId,'     UID = '+request.data.msgUIDs.Strings[i]);
            end;
            requestMgr.SetSummary(request.requestId,
                Format('%d out of %d messages were copied to %s @ %s. There were some errors. Click "Details" for a list of errors',
                    [successCount,totalCount,mDestFullMailboxName,request.accountInfoPtr^.Name]),mlvWarn);
        end;

        try
            if request.data.copyOperation=tcoCopyMarkDeleted then begin
                // Have to undelete any failed UIDs
                for i:=0 to request.data.msgUIDs.Count-1 do failedUIDs.Add(request.data.msgUIDs[i]);
                devLog.Trace('Number of messages that failed uploading: '+inttostr(failedUIDs.Count));
                if failedUIDs.Count>0 then begin
                    FMain.PerformMessageIMAPOperation(iopDeleteMsg,request.data.mailboxIndex,failedUIDs);
                end;
            end;
        finally
            failedUIDs.Free;
        end;

    end;
end;

{ Private method used to add a line to a file.
  @NOTE: The file stream HAS to be open!!! }
procedure TIMAPWorker.addLineToFile(var theFile: TFileStream; theLine: String);
var buffer: PChar;
begin
    buffer := PChar(theLine);
    theFile.Write(buffer^, Length(theLine));
end;

{ Renames the mFullMailboxName to mDestFullMailboxName. It also changes names of any
  mailboxes that start with mFullMailboxName so that they start with mDestFullMailboxName instead.
  It also modifies mNodeName of the renaming node and mFullDisplayedName of all relevant mailboxes }
procedure TIMAPWorker.RenameMailboxes;
var oldFullMailboxName, oldFullDisplayedName, newCompleteFullMailboxName, newCompleteFullDisplayedName: String;
    i, p, oldPrefixLength, oldFullLength: Integer;
begin

    // Save original names
    oldFullMailboxName := mboxTree.Items[mSelectedMailboxIndex].mFullMailboxName;
    oldFullDisplayedName := mboxTree.Items[mSelectedMailboxIndex].mFullDisplayedName;

    // Then rename all descendants
    for i:=1 to mboxTree.Count-1 do begin
        // When looking for changing prefixes, should take into account the separator too
        // so we don't get into the situation of changing e.g. ivan->ivan01 then ivancuk->ivan01cuk
        p := Pos(oldFullMailboxName+FMain.GetSeparator, mboxTree.Items[i].mFullMailboxName);
        if  p > 0 then begin
            devLog.Debug('Mailbox found: '+mboxTree.Items[i].mFullMailboxName);
            oldPrefixLength := Length(oldFullMailboxName);
            oldFullLength := Length(mboxTree.Items[i].mFullMailboxName);
            newCompleteFullMailboxName := mNewFullMailboxName + Copy(mboxTree.Items[i].mFullMailboxName,oldPrefixLength+1,oldFullLength-oldPrefixLength);
            devLog.Debug('Renaming descendant from '+mboxTree.Items[i].mFullMailboxName+' to '+newCompleteFullMailboxName);
            mboxTree.Items[i].mFullMailboxName := newCompleteFullMailboxName;
        end;
        p := Pos(oldFullDisplayedName+FMain.GetSeparator, mboxTree.Items[i].mFullDisplayedName);
        if  p > 0 then begin
            devLog.Debug('Mailbox found: '+mboxTree.Items[i].mFullDisplayedName);
            oldPrefixLength := Length(oldFullDisplayedName);
            oldFullLength := Length(mboxTree.Items[i].mFullDisplayedName);
            newCompleteFullDisplayedName := mNewFullDisplayedName + Copy(mboxTree.Items[i].mFullDisplayedName,oldPrefixLength+1,oldFullLength-oldPrefixLength);
            devLog.Debug('Renaming descendant from '+mboxTree.Items[i].mFullDisplayedName+' to '+newCompleteFullDisplayedName);
            mboxTree.Items[i].mFullDisplayedName := newCompleteFullDisplayedName;
        end
    end;

    // Now rename self
    mboxTree.EnterCS;
    mboxTree.Items[mSelectedMailboxIndex].mNodeName := mNewNodeName;
    mboxTree.Items[mSelectedMailboxIndex].mFullMailboxName := mNewFullMailboxName;
    mboxTree.Items[mSelectedMailboxIndex].mFullDisplayedName := mNewFullDisplayedName;
    mboxTree.LeaveCS;

end;

procedure TIMAPWorker.PopulateMessageList;
var searched: Boolean;
begin
    if ((mOperation=iopSearchAdvanced) or (mOperation=iopSearch) or
        (mOperation=iopSearchReverse))
    then searched:=true
    else searched:=false;
    FMain.PopulateMessageList(mSelectedMailboxIndex,searched);
end;

{ Add messages from mIndexFirst to mIndexLast in mSelectedMailboxIndex (mMessageLists)
  to the message list (ListView1).
  This method should be called SYNCHRONIZED, and the parameters must be set }
procedure TIMAPWorker.AddMessagesToMessageList;
begin
    FMain.AddMessagesToMessageList(mSelectedMailboxIndex, mIndexFirst, mIndexLast, false);
end;

procedure TIMAPWorker.ClearMessageList;
begin
    FMain.ClearMessageList;
end;

procedure TIMAPWorker.UpdateMessageList;
begin
    FMain.UpdateMessageList;
end;

procedure TIMAPWorker.UpdateTreeView;
begin
    FMain.RecalculateSizesAndDisplay(true);
end;

{ Sets quotaInfo for this request }
procedure TIMAPWorker.SetQuota(current, maximum: Integer);
var request: TRequest;
begin
    request := requestMgr.GetRequest(ThreadInfo.requestId);
    request.data.quotaInfo.current:=current;
    request.data.quotaInfo.maximum:=maximum;
end;

{ Adds the specified new mailbox to mboxTree.Items.
  Finds its position (alphabetical order) and inserts it there }
procedure TIMAPWorker.InsertNewMailboxToNodeArray(mFullMailboxName: String);
var i,newIndex,nextNodeParentLevel: Integer; folderPath: TStringList; nodeName: String; found: Boolean;
begin

    // If the parent is root, and under root we have INBOX (namespace),
    // we want the new node to appear as child of INBOX, not root
    if (mSelectedMailboxIndex = 0) and (FMain.mNS.hasPersonalNS) and (not mIMAPConn.conn.account.SmartInbox) then
        mSelectedMailboxIndex := 1;  // Index of INBOX under root

    // Break down full path into components and get the node name
    folderPath := TStringList.Create;
    Split(FMain.EscapeString(mFullMailboxName,false),FMain.mSeparator,folderPath);
    nodeName:=folderPath[folderPath.Count-1];

    // Find the index where the new folder should be inserted
    // First find the limit for searching (next node on parent level)
    found:=false;
    i:=mSelectedMailboxIndex+1;
    while not found and (i<mboxTree.count) do begin
        if mboxTree.Items[i].mLevel<=mboxTree.Items[mSelectedMailboxIndex].mLevel then found:=true
        else Inc(i);
    end;
    nextNodeParentLevel:=i-1;
    devLog.Trace(Format('*** limit=%d',[nextNodeParentLevel]));

    // Now go through parents' children and find the right spot
    found:=false;
    i:=mSelectedMailboxIndex+1;   // parents index + 1
    while not found and (i<=nextNodeParentLevel) do begin
        if mboxTree.Items[i].mLevel = mboxTree.Items[mSelectedMailboxIndex].mLevel+1 then begin
            // Only compare if the comparing node is a child of the same parent
            if (CompareText(nodeName,mboxTree.Items[i].mNodeName)<0) then found:=true
            else Inc(i);
        end
        else Inc(i);
    end;
    newIndex:=i;
    devLog.Debug('Assigned index: '+IntToStr(newIndex));

    mboxTree.EnterCS;

    // Make sure the array is big enough
    if ((mboxTree.Count+1)=mboxTree.Size) then begin
        mboxTree.Size:=mboxTree.Size+MBOX_TREE_SIZE_INCREMENT;
        SetLength(mboxTree.Items,mboxTree.Size);
    end;

    // Move elements from newIndex 1 down
    for i:=mboxTree.Count-1 downto newIndex do begin
        CopyNode(mboxTree.Items[i],mboxTree.Items[i+1]);
        mboxTree.Items[i+1].mAbsoluteIndex:=i+1;
        if (mboxTree.Items[i+1].mParentIndex > newIndex) then Inc(mboxTree.Items[i+1].mParentIndex);
    end;
    mboxTree.Count := mboxTree.Count + 1;

    try
        // Set values for the new element
        mboxTree.Items[newIndex].mNodeName:=folderPath[folderPath.Count-1];
        mboxTree.Items[newIndex].mLevel:=mboxTree.Items[mSelectedMailboxIndex].mLevel+1;
        mboxTree.Items[newIndex].mAbsoluteIndex:=newIndex;
        mboxTree.Items[newIndex].mNumMessages:=0;
        mboxTree.Items[newIndex].mTotalNumMessages:=0;
        mboxTree.Items[newIndex].mSize:=0;
        mboxTree.Items[newIndex].mSizeDisplay:='0';
        mboxTree.Items[newIndex].mTotalSize:=0;
        mboxTree.Items[newIndex].mSizeDisplay:='0';
        mboxTree.Items[newIndex].mPercentOfParent:=0;

        // Set full displayed name
        if (FMain.mNS.hasPersonalNS) then begin
            if (Copy(mFullMailboxName,1,5)='INBOX') then
                mboxTree.Items[newIndex].mFullDisplayedName:=Copy(mFullMailboxName,7,Length(mFullMailboxName)-6)
            else
                mboxTree.Items[newIndex].mFullDisplayedName:=mFullMailboxName;
        end
        else mboxTree.Items[newIndex].mFullDisplayedName:=mFullMailboxName;
        mboxTree.Items[newIndex].mFullMailboxName:=mFullMailboxName;

        mboxTree.Items[newIndex].mIsLeaf:=true;
        mboxTree.Items[newIndex].mNumOfChildren:=0;
        mboxTree.Items[newIndex].mImageIndex:=0;
        mboxTree.Items[newIndex].mParentIndex:= mSelectedMailboxIndex;
        mboxTree.Items[newIndex].mExpanded:=false;
        mboxTree.Items[newIndex].mVirtual:=false;

        // Modify parents node
        mboxTree.Items[mSelectedMailboxIndex].mNumOfChildren := mboxTree.Items[mSelectedMailboxIndex].mNumOfChildren + 1;
        mboxTree.Items[mSelectedMailboxIndex].mIsLeaf:=false;
        mboxTree.Items[mSelectedMailboxIndex].mExpanded:=true;

        // Update the messagelist
        mMessageLists.InsertMailbox(newIndex);
        if mDisplayedNodeIndex>=newIndex then Inc(mDisplayedNodeIndex);

    finally
        if folderPath<>nil then folderPath.Free;
        mboxTree.LeaveCS;
    end;
end;

{ Removes a deleted mailbox from the tree model (mboxTree.Items[mSelectedMailboxIndex]) }
procedure TIMAPWorker.RemoveDeletedMailboxFromNodeArray;
var i: Integer; parentIndex: Integer;
begin
    mboxTree.EnterCS;
    try
        // Decrease parents number of children
        parentIndex := mboxTree.Items[mSelectedMailboxIndex].mParentIndex;
        mboxTree.Items[parentIndex].mNumOfChildren := mboxTree.Items[parentIndex].mNumOfChildren - 1;

        // Set parent to leaf if no more children
        if mboxTree.Items[parentIndex].mNumOfChildren = 0 then mboxTree.Items[parentIndex].mIsLeaf:=true;

        for i:=mSelectedMailboxIndex to mboxTree.Count-2 do begin
            CopyNode(mboxTree.Items[i+1],mboxTree.Items[i]);
            mboxTree.Items[i].mAbsoluteIndex:=i;
        end;
        mboxTree.Count := mboxTree.Count - 1;
    finally
        mboxTree.LeaveCS;
    end;
end;

procedure TIMAPWorker.SaveHeadersOfSelectedMessages;
var theFile: TFileStream; i,n: Integer; line, command, res, s: String; append: Boolean;
begin
    devLog.Trace('Saving message headers');
    try
        // Create the file if it doesn't exist
        append:=true;
        if not FileExists(mFilename) then begin
            devLog.Debug('Created file: '+mFilename);
            theFile := TFileStream.Create(mFilename, fmCreate);
            theFile.Destroy;
            append:=false;
        end
        else devLog.Debug('File already exists (will append): '+mFilename);

        // Try to open the file for writting (will append)
        theFile := TFileStream.Create(mFilename, fmOpenReadWrite);
        theFile.Seek(0, soFromEnd);

        if mIMAPConn.conn.imap.SelectFolder(mFullMailboxName) then begin
            for i:=0 to mMsgUIDs.Count-1 do begin
                devLog.Debug('Creating fetch header command for msg: '+mMsgUIDs[i]);
                command:='UID FETCH ' + mMsgUIDs[i] + ' BODY.PEEK[HEADER]';
                if not terminated then begin
                    if mIMAPConn.conn.imap.IMAPCommand(command)='OK' then begin
                        // Add the header to the file
                        addLineToFile(theFile, chr(13)+chr(10)+'=-=-= Mailbox: '+ mFullMailboxName +
                                      ' =-=-= Message UID: '+mMsgUIDs[i]+' =-=-='+
                                      chr(13)+chr(10)+chr(13)+chr(10));
                        for n := 0 to mIMAPConn.conn.imap.FullResult.Count - 2 do
                            if mIMAPConn.conn.imap.FullResult[n][Length(mIMAPConn.conn.imap.FullResult[n])] = '}' then begin
                                line := mIMAPConn.conn.imap.FullResult[n + 1];
                                addLineToFile(theFile, line);
                                Break;
                            end;
                    end
                    else devLog.Warn('Server could not execute the command');
                end
                else begin
                    UpdateStatusBar('Fetch aborted',STATUSBAR_MSGS);
                    break;
                end;
            end;
            if not terminated then begin
                if append then s:='appended' else s:='saved';
                requestMgr.SetSummary(ThreadInfo.RequestID,'Message headers '+s+' to '+mFilename,mlvInfo);
            end;
        end
        else begin
            HandleErrorSituation('Header retrieval failure');
        end;
    finally
        // Make sure the file is closed
        if (theFile <> nil) then theFile.Destroy();
        theFile := nil;
    end;
end;

{ Perorms a batch quick and smart attachment deletion }
procedure TIMAPWorker.QSDeleteAttachments;
var bsString, command, res: String; bsParsed: Boolean; request: TRequest; bs: TBodyStructure;
    strippedInfo: TStrippedMessageInfo; attStripper: TAttachmentStripper; strippedMsg: TStringList;
    header,mainTextPart,newMessage: TStringList;
    i,j, msgIndex, strippedCount: Integer; msg: TMessageInfo;
    summary: String;

    procedure ParseBodyStructure;
    var wholeBS: String; p: Integer;
    begin
        p := Pos('BODY (',bsString);
        if p>0 then begin
            wholeBS := Copy(bsString,p,Length(bsString)-p);
            bsParsed := bs.Parse(wholeBS);
        end;
    end;

    procedure ParseMess(Value:TStrings);
    var n: integer;
    begin
        Value.Clear;
        for n := 0 to mIMAPConn.conn.imap.FullResult.Count - 2 do
            if mIMAPConn.conn.imap.FullResult[n][Length(mIMAPConn.conn.imap.FullResult[n])] = '}' then begin
                Value.Text := mIMAPConn.conn.imap.FullResult[n + 1];
                Break;
            end;
    end;

begin
    devLog.Trace('QSDeleteAttachments');
    UpdateStatusBar('Deleting attachments...',STATUSBAR_MSGS);
    strippedCount:=0;
    request := requestMgr.GetRequest(ThreadInfo.requestId);
    request.activityInfo.errors.Clear;
    request.activityInfo.log.Clear;

    if mIMAPConn.conn.imap.SelectFolder(mFullMailboxName) then begin
        bs:=TBodyStructure.Create;
        header:=TStringList.Create;
        mainTextPart:=TStringList.Create;
        newMessage:=TStringList.Create;
        try
            // For each selected message do the following
            for i:=0 to mMsgUIDs.Count-1 do begin
                // Check if the message has attachments
                msgIndex:=mMessageLists.GetMessageInfoByUID(msg,SelectedMailboxIndex,mMsgUIDs[i]);
                if (msgIndex<>-1) then begin
                    if msg.mHasAttachments then begin
                        // Get the body structure and parse it
                        command:='UID FETCH ' + mMsgUIDs[i] + ' (BODY)';
                        if (mIMAPConn.conn.imap.IMAPCommand(command)='OK') then begin
                            bsString:=mIMAPConn.conn.imap.FullResult[0];
                            // Clear all the lists
                            bs.Clear; header.Clear; mainTextPart.Clear; newMessage.Clear;
                            ParseBodyStructure;  // this will populate the bs object
                            if bsParsed then begin

                                // Get the header
                                command:='UID FETCH ' + mMsgUIDs[i] + ' BODY.PEEK[HEADER]';
                                if mIMAPConn.conn.imap.IMAPCommand(command)='OK' then ParseMess(header);

                                // Get the main text part
                                bs.CalculatePrimaryTextPart;
                                if bs.HasPrimaryTextPart then begin
                                    command:='UID FETCH '+mMsgUIDs[i]+' BODY.PEEK['+bs.GetPrimaryTextPart+']';
                                    if mIMAPConn.conn.imap.IMAPCommand(command)='OK' then ParseMess(mainTextPart);
                                    for j:=0 to mainTextPart.Count-1 do devLog.Trace(mainTextPart[j]);
                                end;

                                // Form the stripped message info
                                attStripper:=TAttachmentStripper.Create;
                                attStripper.StripMessage(header,mainTextPart,bs,false);
                                attStripper.GetStrippedMessage(TStrings(newMessage));
                                attStripper.Free;

                                // Now upload the new message to the server
                                UploadWithErrorCatching(newMessage,true,true,true,GetFlagsStringForAppend(msg.mFlags));

                                // Log success
                                requestMgr.Log(ThreadInfo.requestID,'Successfully removed attachments from message (UID='+mMsgUIDs[i]+')');

                                // Mark original message as deleted
                                command:='UID STORE ' + mMsgUIDs[i] + ' +FLAGS.SILENT (\Deleted)';
                                mIMAPConn.conn.imap.IMAPCommand(command);
                                msg.mFlags.mDeleted:=true;
                                mMessageLists.UpdateMessageInfo(mDisplayedNodeIndex,msgIndex,msg);

                                Inc(strippedCount);
                            end
                            else begin
                                devLog.Error('Unable to parse bodystructure for msgUID: '+mMsgUIDs[i]);
                                requestMgr.LogError(ThreadInfo.requestID,'Unable to parse bodystructure for msgUID: '+mMsgUIDs[i]);
                            end;
                        end
                        else begin
                            devLog.Error('Unable to get bodystructure for msgUID: '+mMsgUIDs[i]);
                            requestMgr.LogError(ThreadInfo.requestID,'Unable to get bodystructure for msgUID: '+mMsgUIDs[i]);
                        end;
                    end
                    else begin
                        requestMgr.Log(ThreadInfo.requestID,'Message (UID='+mMsgUIDs[i]+') does not contain attachments. Skipping');
                    end;
                 end
                 else begin
                     requestMgr.LogError(ThreadInfo.requestID,'Cannot find local message (UID='+mMsgUIDs[i]+'). Skipping');
                 end;
            end;
        finally
            Synchronize(UpdateMessageList);
            UpdateQuota;
            bs.Free;
            header.Free;
            mainTextPart.Free;
            newMessage.Free;
            if strippedCount=0 then
                summary:='Attachments stripped from 0 messages. Click the ''Details'' button for more information.'
            else if strippedCount=1 then
                summary:='Attachments stripped from 1 message.'+CRLF+CRLF+
                         'A new message without attachments has been created. The original message has been marked ''Deleted''. Expunge mailbox to remove it completely.'
            else
                summary:='Attachments stripped from '+IntToStr(strippedCount)+' messages.'+CRLF+CRLF+
                         'A new message without attachments has been created for each of these. Original messages have been marked as ''Deleted''. Expunge mailbox to remove them completely.';
            requestMgr.SetSummary(ThreadInfo.RequestID,summary,mlvInfo);
            UpdateStatusBar('',STATUSBAR_MSGS);
        end;
    end
    else HandleErrorSituation('Attachment deletion failure');
end;

{ Determines the flags string for the Append command based on the specified msgInfo.
  Returns an empty string if there are no flags to set }
function TIMAPWorker.GetFlagsStringForAppend(msgFlags: TMessageFlags): String;
var flags: String; count: Integer;
begin
    flags:=' (';
    if msgFlags.mSeen then flags:=flags+'\Seen ';
    if msgFlags.mAnswered then flags:=flags+'\Answered ';
    if msgFlags.mDeleted then flags:=flags+'\Deleted ';
    if msgFlags.mFlagged then flags:=flags+'\Flagged ';
    if msgFlags.mDraft then flags:=flags+'\Draft ';
    if flags<>' (' then
        // Replace the trailing white space with a closing bracket
        flags[Length(flags)]:=')'
    else
        flags:='';
    Result:=flags;
end;

{ Gets the folder list of the account.
  Places the list into request.data.msgContent }
procedure TIMAPWorker.GetFolderList(subscribedOnly: Boolean; var personalNamespace: String);
var request: TRequest; FolderList: TFolderListProcessor;
begin
    devLog.Trace('Getting Folder List');
    UpdateStatusBar('Getting Folder List',STATUSBAR_MSGS);  // clear messages
    request := requestMgr.GetRequest(ThreadInfo.requestId);
    request.data.msgContent.Clear;
    FolderList:=TFolderListProcessor.Create;
    personalNamespace:='';
    if FolderList.GetMailboxes(mIMAPConn.conn, request.data.msgContent, subscribedOnly) then begin
        // Do nothing, folder list retrieved and ready for use
        request.data.folderSeparator:=FolderList.mSeparator;
        personalNamespace:=FolderList.GetPersonalNamespace;
    end
    else begin
        requestMgr.SetSummary(request.RequestID,'Unable to get the folder list from account ['+request.accountInfoPtr^.Name+']',mlvError);
    end;
    FolderList.Free;
    UpdateStatusBar('',STATUSBAR_MSGS);  // clear messages
end;

{ (Un)Subscribe folders specified in request.data.generalList }
procedure TIMAPWorker.SubscribeFolders;
var command, fullCommand: String; i, cntSuccess: Integer; request: TRequest;
begin
    cntSuccess:=0;
    request := requestMgr.GetRequest(ThreadInfo.requestId);
    for i:=0 to request.data.generalList.Count-1 do begin
        if TSubscriptionPair(request.data.generalList[i]).Subscribe
            then command:='SUBSCRIBE'
            else command:='UNSUBSCRIBE';
        fullCommand:=command+' "'+TSubscriptionPair(request.data.generalList[i]).Name + '"';
        if mIMAPConn.conn.imap.IMAPCommand(fullCommand)='OK' then
            Inc(cntSuccess)
        else
            requestMgr.LogError(ThreadInfo.RequestID,'ERROR Could not '+LowerCase(command)+' folder '+TSubscriptionPair(request.data.generalList[i]).Name+'. Server responded: '+GetResultMessage);
    end;
    if (cntSuccess <> request.data.generalList.Count) then begin
        if cntSuccess=0 then begin
            requestMgr.SetSummary(request.RequestID,'(Un)subscribe operation unsuccessfull. Operation failed for all folders.',mlvError)
        end
        else begin
            requestMgr.SetSummary(request.RequestID,'(Un)subscribe operation was partially successfull. Operation failed for some folders.',mlvError);
        end;
    end;
end;

{ Queries the IMAP server for information which might be usefull for finding problems }
procedure TIMAPWorker.DebugServer;
var request: TRequest; ns, command: String; FolderList: TFolderListProcessor;

    procedure AddServerReply;
    var i: Integer;
    begin
        for i:=0 to mIMAPConn.conn.imap.FullResult.Count-1 do begin
            request.data.msgContent.Add(mIMAPConn.conn.imap.FullResult[i]);
        end;
    end;

begin
    request := requestMgr.GetRequest(ThreadInfo.requestId);
    request.data.msgContent.Clear;

    // Check capabilities
    request.data.msgContent.Add(CRLF+'=== CAPABILITY ===');
    mIMAPConn.conn.imap.IMAPCommand('CAPABILITY');
    AddServerReply;

    // Check namespace
    request.data.msgContent.Add(CRLF+'=== NAMESPACE ===');
    if (mIMAPConn.conn.imap.FindCap('NAMESPACE') <> '') then begin
        mIMAPConn.conn.imap.IMAPCommand('NAMESPACE');
        AddServerReply;
    end
    else request.data.msgContent.Add('Server does not support NAMESPACE');

    // Get folder list
    request.data.msgContent.Add(CRLF+'=== FULL FOLDER LIST ===');
    mIMAPConn.conn.imap.IMAPCommand('LIST "" "*"');
    AddServerReply;

end;

procedure TIMAPWorker.AccountBackup;
var i, backupFolderId, uidValidity: Integer; backupDir, mboxFilename, emlFolder: String;
    dummy, messageIdsToSkip: TStringList; summaryLvl: TMessageLevel; maxUID: String;
begin
    backupDir:=mFilename;
    summaryLvl:=mlvInfo;
    backupRunning:=True;
    //messageIdsToSkip:=TstringList.Create;
    try
        devLog.Debug('Backup dir: '+backupDir);
        // Start backing up (one by one)
        for i:=0 to mFolders.Count-1 do begin
            // messageIdsToSkip.Clear;
            UpdateStatusBar(Format('Backing up folder "%s" (%d of %d)', [mFolders[i],i+1,mFolders.Count]),STATUSBAR_MSGS);
            emlFolder:=GetLocalEMLFolder(backupDir,mFolders[i],mFolderSeparator,true);
            // Get folder UID validity
            if mIMAPConn.conn.imap.SelectFolder(mFolders[i]) then begin
                uidValidity:=mIMAPConn.conn.imap.SelectedUIDvalidity;
            end;
            // Get folder from db, if it doesn't exist add to db...
            backupFolderId:=FMain.backupDb.GetAccountFolderID(mIMAPConn.conn.account.Name,mFolders[i],emlFolder,uidValidity);
            // FMain.backupDb.GetExistingMessageUIDs(messageIdsToSkip,backupFolderId);
            maxUID:=FMain.backupDb.GetMaxMessageIDForFolder(backupFolderId);
            mFullMailboxName:=mFolders[i];
            // mFilename has to be the full path to the local eml folder
            mFilename:=GetLocalEMLFolder(backupDir,mFolders[i],mFolderSeparator,false);
            DownloadEMLsBackupMode(backupFolderId,maxUID,summaryLvl);
        end;
        requestMgr.SetSummary(ThreadInfo.RequestID,'Backup process completed',summaryLvl);
    finally
        //messageIdsToSkip.Free;
        UpdateStatusBar('',STATUSBAR_MSGS);
        backupRunning:=False;
    end;
end;

{ Restore backup }
procedure TIMAPWorker.RestoreBackup;
var
    srcFolderSeparator, dstFolderSeparator: Char;
    folders: TListOfBackupFolderInfos;
    dstFolderWithDestSeparator: String;
    i,m,p, successCnt: Integer;
    command, messageId: String;
    request: TRequest;
    personalNamespace, res: String;
    summaryLvl: TMessageLevel; 
    folderExists: Boolean;
    msgUIDs, mLocalMsgUIDs, mRemoteMsgIDs, mLocalMsgIDs: TStringList;
    filename, newUID: String;
    msgContents: TStringList;
    progressBarValue: Real;
    progressBarIncrement: Real;

    { Restores EMLs from mFilenames to dstFolderWithDestSeparator to the currently selected folder
      @param updateUID If true will update the UID for restored emls in the database }
    procedure RestoreEMLs(updateUID: Boolean);
    var m: Integer;

        { Extracts the UID from a FETCH response (one line) }
        function getUID(line: String): String;
        var UID: String; p: Integer;
        begin
            p:=pos('UID ',line);   // Find beggining of UID
            p:=p+4;                // Find first character of UID value
            UID:='';
            while line[p]<>')' do begin  // Add all strings until the next blank char
                UID:=UID+line[p];
                Inc(p);
            end;
            Result:=UID;
        end;

    begin
        msgContents := TStringList.Create;
        try
            try
                PostMessage(requestMgr.Handle, WM_MAIN_PROGRESS_BAR_VISIBILITY,ThreadInfo.requestID, 1);
                progressBarValue:=0;
                progressBarIncrement := 100/mFilenames.Count;
                PostMessage(requestMgr.Handle, WM_PROGRESS_BAR_UPDATE, ThreadInfo.requestID, Trunc(progressBarValue));

                successCnt:=0;
                for m:=0 to mFilenames.Count-1 do begin
                    if not terminated then begin
                        msgContents.Clear;
                        msgContents.LoadFromFile(mFilenames[m]);
                        UpdateStatusBar('Uploading eml '+IntToStr(m+1)+' of '+IntToStr(mFilenames.Count)+' to '+dstFolderWithDestSeparator,STATUSBAR_MSGS);
                        devLog.Info('Uploading eml '+IntToStr(m+1)+' of '+IntToStr(mFilenames.Count)+' to '+dstFolderWithDestSeparator);
                        res:=mIMAPConn.conn.imap.IMAPuploadCommand('APPEND "' + dstFolderWithDestSeparator + '"', msgContents);
                        if res='OK' then begin
                            Inc(successCnt);
                            if updateUID then begin
                                // Get the UID of the last message
                                mIMAPConn.conn.imap.IMAPcommand('UID FETCH * (UID)');
                                newUID:=getUID(mIMAPConn.conn.imap.FullResult[0]);
                                // Filenames have a unique timestamp, so it's OK to do the following
                                // (no reference to account or folder)
                                FMain.backupDb.Update('UPDATE message SET uid='+newUID+' WHERE filename="'+ExtractFileName(mFilenames[m])+'"');
                            end;
                        end
                        else begin
                            requestMgr.LogError(ThreadInfo.RequestID,'Message not appended due to: '+GetResultMessage);
                            devLog.Error('Message not appended due to: '+GetResultMessage);
                            summaryLvl:=mlvError;
                        end;
                    end;
                    progressBarValue:=progressBarValue+progressBarIncrement;
                    PostMessage(requestMgr.Handle, WM_PROGRESS_BAR_UPDATE, ThreadInfo.requestID, Trunc(progressBarValue));
                end;
                PostMessage(requestMgr.Handle, WM_PROGRESS_BAR_UPDATE, ThreadInfo.requestID, 100);
                requestMgr.Log(request.requestId,'Uploaded '+IntToStr(successCnt)+'/'+IntToStr(mFilenames.Count)+' messages into folder "'+dstFolderWithDestSeparator+'" ');
            except
                on E: Exception do begin
                    devLog.Error(E.Message);
                    requestMgr.SetSummary(ThreadInfo.RequestID,'Can''t continue uploading messages: '+E.Message,mlvError);
                end;
            end;

        finally
            PostMessage(requestMgr.Handle, WM_MAIN_PROGRESS_BAR_VISIBILITY,ThreadInfo.requestID, 0);
            msgContents.Free;
        end;
    end;

begin
    summaryLvl:=mlvInfo;
    // Using the following data: request.data.backupFolders, request.data.folderSeparator (srcAccount),
    //                           request.data.restoreSameAccount, request.data.accountName (srcAccount)
    request := requestMgr.GetRequest(ThreadInfo.requestId);
    srcFolderSeparator:=request.data.folderSeparator;

    // Get remote list of folders (stored in request.data.msgContent)
    // WARN: request.data.folderSeparator is changed in this method (not nice, should refactor)
    GetFolderList(false, personalNamespace);
    devLog.Info('Destination account namespace: '+personalNamespace);
    // The folder separator for the destination account is now stored in mIMAPConn.conn.account.FolderSeparator
    dstFolderSeparator:=mIMAPConn.conn.account.FolderSeparator;

    // Create folders that don't exist on the remote server
    devLog.Info('Checking remote folders');
    for i:=0 to Length(request.data.backupFolders)-1 do begin
        // Adjust folder separators
        dstFolderWithDestSeparator:=StringReplace(request.data.backupFolders[i].newRemoteName,srcFolderSeparator,dstFolderSeparator,[rfReplaceAll]);
        // Adjust personal namespace (prefix if not already there) if the accounts are different

        if not request.data.restoreSameAccount then begin
            p:=Pos(personalNamespace,dstFolderWithDestSeparator);
            if (p<>1) then begin
                // Folder name does not start with the personal namespace prefix
                // Exclude the Inbox.Inbox case, hack for http://www.broobles.com/forum/viewtopic.php?t=578
                if not ((LowerCase(Copy(personalNamespace,0,5))='inbox') and (LowerCase(dstFolderWithDestSeparator)='inbox')) then begin
                    dstFolderWithDestSeparator := personalNamespace + dstFolderWithDestSeparator;
                end;
            end;
        end;

        // Ensure folder exists
        folderExists:=true;
        devLog.Info('Checking folder: '+dstFolderWithDestSeparator);
        if (request.data.msgContent.IndexOf(dstFolderWithDestSeparator)>-1) then begin
            // Folder exists, skip
            devLog.Info('Remote folder exists: '+dstFolderWithDestSeparator);
        end
        else begin
            // Folder does not exist, create it
            devLog.Info('Folder '+dstFolderWithDestSeparator+ ' does NOT exist, creating it');
            res := mIMAPConn.conn.imap.IMAPcommand('CREATE "' + dstFolderWithDestSeparator + '"');
            if (res = 'OK') then begin
                devLog.Info('Folder created');
            end
            else begin
                devLog.Warn('Server refused to create folder');
                requestMgr.LogError(request.requestId,'ERROR Folder "'+dstFolderWithDestSeparator+'" was not created. Server response: "'+GetResultMessage+'"');
                summaryLvl:=mlvError;
                folderExists:=false;
            end;
        end;
        if folderExists then begin
            msgUIDs := TStringList.Create;
            mLocalMsgUIDs := TStringList.Create;
            mLocalMsgIDs := TStringList.Create;
            mRemoteMsgIDs := TStringList.Create;

            // Get message-ids of all remote messages
            mIMAPConn.conn.imap.SelectFolder(dstFolderWithDestSeparator);
            UpdateStatusBar('Checking remote folder "'+dstFolderWithDestSeparator+'"',STATUSBAR_MSGS);
            SearchUIDs('UID 1:*',msgUIDs);

            // We distinguish restore when uploading to the original backup account and to a different account
            if request.data.restoreSameAccount then begin
                // Source and destination accounts are the same, compare by UID
                mFilenames.Clear;
                FMain.backupDb.GetExistingMessageUIDs(mLocalMsgUIDs, request.data.accountName, request.data.backupFolders[i].localFolder);
                for m:=0 to mLocalMsgUIDs.Count-1 do begin
                    if msgUIDs.IndexOf(mLocalMsgUIDs[m]) = -1 then begin
                        // Need to upload this local message, add to mFilenames
                        filename:=FMain.backupDb.GetFilenameByUID(request.data.accountName,request.data.backupFolders[i].localFolder,mLocalMsgUIDs[m]);
                        if (filename<>'') and (mFilenames.IndexOf(filename)=-1) then begin
                            mFilenames.add(filename);
                            devLog.Trace(filename);
                        end;
                    end
                end;
                if mFilenames.Count > 0 then begin
                    RestoreEMLs(true);
                end
                else begin
                    requestMgr.Log(request.requestId,'Folder "'+dstFolderWithDestSeparator+'" is in sync, nothing to restore');
                end;
            end
            else begin
                // Source and destination accounts are not the same, compare by message-id

                for m:=0 to msgUIDs.Count-1 do begin
                    command:='UID FETCH '+ msgUIDs[m] +' BODY.PEEK[HEADER.FIELDS (MESSAGE-ID)]';

                    if mIMAPConn.conn.imap.IMAPCommand(command)='OK' then begin
                        // Extract the message-id from the response
                        messageId:=GetMessageId(mIMAPConn.conn.imap.FullResult);
                        devLog.Trace(messageID);
                        mRemoteMsgIDs.Add(messageId);
                    end;
                end;

                // Get message ids for account-folder from db and put into mLocalMsgIDs
                UpdateStatusBar('Checking local folder "'+request.data.backupFolders[i].localFolder+'" for account "'+request.data.accountName+'"',STATUSBAR_MSGS);
                FMain.backupDb.GetMessageIds(mLocalMsgIDs, request.data.accountName, request.data.backupFolders[i].remoteFolder);
                mFilenames.Clear;
                devLog.Trace('Local files to upload: ');
                for m:=0 to mLocalMsgIDs.Count-1 do begin
                    if mRemoteMsgIDs.IndexOf(mLocalMsgIDs[m]) = -1 then begin
                        // Need to upload this local message, add to mFilenames
                        filename:=FMain.backupDb.GetFilenameByMessageId(request.data.accountName,request.data.backupFolders[i].localFolder,mLocalMsgIDs[m]);
                        if (filename<>'') and (mFilenames.IndexOf(filename)=-1) then begin
                            mFilenames.add(filename);
                            devLog.Trace(filename);
                        end;
                    end;
                end;
                if mFilenames.Count > 0 then begin
                    RestoreEMLs(false);
                end
                else begin
                    requestMgr.Log(request.requestId,'Folder "'+dstFolderWithDestSeparator+'" is in sync, nothing to restore');
                end;
            end;
            UpdateStatusBar('',STATUSBAR_MSGS);
            msgUIDs.Free;
            mLocalMsgUIDs.Free;
            mRemoteMsgIDs.Free;
            mLocalMsgIDs.Free;
        end;
    end;
    requestMgr.SetSummary(ThreadInfo.RequestID,'Restore process completed',summaryLvl);
end;

{ Extracts the Message-Id from a fetch header (message-id). Returns an empty string if not found }
function TIMAPWorker.GetMessageId(const fullReply: TStringList): String;
var found: Boolean; i, openPos, closePos: Integer;
begin
    Result:='';
    found:=false; i:=0;
    while (not found) and (i<fullReply.Count) do begin
        if Copy(LowerCase(fullReply[i]),1,10)='message-id' then begin
            openPos:=Pos('<',fullReply[i]);
            closePos:=Pos('>',fullReply[i]);
            if (openPos>0) and (closePos>0) then begin
                Result:=Copy(fullReply[i],openPos,closePos-openPos+1);
                found:=true;
            end;
        end;
        Inc(i);
    end;
end;


{ Saves all specified attachments (sequentially) }
procedure TIMAPWorker.SaveAttachments;
var command: String; i, cntSuccess: Integer; request: TRequest;
    attSavingInfo: TAttachmentSavingInfo; msgContent, decodedStr: TStringList;
    attType: TAttachmentType; error, warn: Boolean; msg: String;

    procedure logAttSavingInfo(asi: TAttachmentSavingInfo);
    begin
        devLog.Trace('Saving attachment with following parameters: ');
        devLog.Trace('   FullMailboxName: '+asi.fullMailboxName);
        devLog.Trace('   MsgUID:          '+asi.msgUID);
        devLog.Trace('   BodyPartID:      '+asi.bodyPartID);
        devLog.Trace('   Encoding:        '+asi.encoding);
        devLog.Trace('   Mime Type:       '+asi.partMimeType);
        devLog.Trace('   Mime SubType:    '+asi.partMimeSubtype);
        devLog.Trace('   Filename:        '+asi.filename);
    end;

begin
    try
        cntSuccess:=0;
        error:=false;  warn:= false; // will become true in case of any errors
        msgContent:=TStringList.Create;
        request := requestMgr.GetRequest(ThreadInfo.requestId);
        // Process each attachment
        for i:=0 to request.data.generalList.Count-1 do begin
            attSavingInfo:=TAttachmentSavingInfo(request.data.generalList[i]);
            logAttSavingInfo(attSavingInfo);
            UpdateStatusBar(Format('Saving attachment "%s" (%d of %d), Size: %s',[attSavingInfo.filename,i,request.data.generalList.Count,FloatToStrF(attSavingInfo.size,ffNumber,12,0)]),STATUSBAR_MSGS);
            if mIMAPConn.conn.imap.SelectFolder(attSavingInfo.fullMailboxName) then begin
                command:='UID FETCH '+attSavingInfo.msgUID+' BODY['+attSavingInfo.bodyPartID+']';
                if mIMAPConn.conn.imap.IMAPCommand(command)='OK' then begin
                    ParseMess(msgContent);
                    // Now that we have the attachment, save it
                    decodedStr:=TStringList.Create;
                    try
                        attType:=DecodePart(msgContent,decodedStr,attSavingInfo.encoding,attSavingInfo.partMimeType,attSavingInfo.partMimeSubtype);
                        try
                            if attType=attTypeBinary then begin
                                devLog.Trace('Decoding as binary');
                                SaveFileFromString(attSavingInfo.filename,decodedStr[0]);
                            end
                            else begin
                                devLog.Trace('Decoding as text');
                                decodedStr.SaveToFile(attSavingInfo.filename);
                            end;
                            if GetFileSize(attSavingInfo.filename)=0 then begin
                                warn:=true;
                                requestMgr.LogError(ThreadInfo.RequestID,'WARN File '+attSavingInfo.filename+' saved with size 0 (MsgUID='+attSavingInfo.msgUID+')');
                            end;
                        except
                            on E:EFCreateError do begin
                                error:=true;
                                requestMgr.LogError(ThreadInfo.RequestID,'ERROR Failed to save file: '+E.Message+' (MsgUID='+attSavingInfo.msgUID+')');
                            end;
                        end;
                    finally
                        decodedStr.Free;
                    end;
                end
                else begin
                    error:=true;
                    requestMgr.LogError(ThreadInfo.RequestID,'ERROR Server could not execute the command');
                end;
            end
            else begin
                error:=true;
                devLog.Warn('Unable to select mailbox: '+attSavingInfo.fullMailboxName);
                requestMgr.LogError(ThreadInfo.RequestID,'ERROR Unable to select mailbox: '+attSavingInfo.fullMailboxName);
            end;
        end;

    finally
        msgContent.Free;
        msg:='Attachment saving process completed';
        if error then
            requestMgr.SetSummary(ThreadInfo.RequestID,msg + ' with errors.',mlvError)
        else if warn then
            requestMgr.SetSummary(ThreadInfo.RequestID,msg + ' with warnings.',mlvWarn)
        else
            requestMgr.SetSummary(ThreadInfo.RequestID,msg + ' successfully.',mlvInfo);
        UpdateStatusBar('',STATUSBAR_MSGS);
    end;

end;

procedure TIMAPWorker.UpdateStatusBar(newText: String; panelIndex: Integer);
begin
    mStatusBarText:=newText;
    mStatusBarPanelIndex:=panelIndex;
    Synchronize(InternalUpdateStatusBar);
end;

procedure TIMAPWorker.InternalUpdateStatusBar;
begin
    FMain.ShowMessageOnStatusBar(mStatusBarText, mStatusBarPanelIndex);
    FMain.StatusBar1.Repaint;
end;

{ Sends a notification to main that messages should be added to the gui message list.
  Also sets the data which will be used by the processing component }
procedure TIMAPWorker.InvokeMessageListAddition(mboxIndex, msgFrom, msgTo: Integer; clear: Boolean);
begin
    mMessageLists.SetPageDisplayInfo(mboxIndex, msgFrom, msgTo, clear);
    PostMessage(requestMgr.Handle, WM_UPDATE_MESSAGE_LIST,ThreadInfo.requestID,0);
end;

{ Use this when an IMAP command doesn't return an OK response }
procedure TIMAPWorker.HandleErrorSituation(msg: String);
var res: String;
begin
    res := CheckResult;
    if res = 'NO' then begin
        requestMgr.SetSummary(ThreadInfo.RequestID,msg+': '+GetResultMessage,mlvError);
    end
    else if res = 'BAD' then begin
        requestMgr.SetSummary(ThreadInfo.RequestID,msg+': '+GetResultMessage,mlvError);
    end
    else begin
        if (mAttemptCount = 1) then begin
            mReloginAndTryAgain := true;
        end
        else begin
            requestMgr.SetSummary(ThreadInfo.RequestID,msg+':  '+NO_CONNECTION_MSG,mlvError);
        end;
    end;
end;

function TIMAPWorker.CheckResult: String;
var s: String;
begin
    s:=mIMAPConn.conn.imap.ResultString;
    if s<>'' then begin
        s := SeparateRight(s, ' ');
        s:=Uppercase(SeparateLeft(s, ' '));
    end;
    Result:=s;
end;

function TIMAPWorker.GetResultMessage: String;
var s: String;
begin
    s:=mIMAPConn.conn.imap.ResultString;
    if s<>'' then begin
        s := SeparateRight(s,' ');  // Strip off the Sxx part
        s := SeparateRight(s,' ');  // Strip off the OK, NO, BAD part
    end
    else s:='Result message not specified by server';
    Result:=s;
end;

end.
