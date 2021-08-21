{==============================================================================|
| Project : IMAPSize                                                           |
|==============================================================================|
| $Id: RequestManager.pas,v 1.1 2004/03/31 23:27:33 Ivan Exp $
|==============================================================================|
| Copyright (c)2003-2004, Ivan Vecanski                                        |
| =============================================================================}

unit RequestManager;

interface

uses Log, VecLog, HashTables, IMAPConnectionPool, IMAPConnection, CustomThread, ThreadManager, MyUtil,
     MessageLists, GetConnectionResponseInfo,
     Classes, SysUtils, Converter, Accounts, IMAPWorker, CheckerThread, Messages, GlobalConstants,
     Windows, Dialogs, Forms, RequestActivityFrm, CommonTypes;

type

    TRequestType = (rtLogin,
                    rtLogout,
                    rtCheckSize,
                    rtGetQuota,
                    rtGetFolderList,
                    rtGetSubscribedFolderList,
                    rtSubscribe,
                    rtGeneralIMAP,
                    rtMessageIMAP,
                    rtSearch,
                    rtGlobalSearch,
                    rtCopy,
                    rtRename,
                    rtSaveHeaders,
                    rtAppend,
                    rtCreateMailbox,
                    rtPrepareForMessageViewing,  // Invoked prior to showing a message. Gets the BodyStructure string
                    rtFetch,
                    rtFetchHeader,
                    rtFetchHeaderForQSDeletion,
                    rtFetchHeaderForRemoteSaving,
                    rtFetchHeaderForLocalSaving,
                    rtFetchWholeBody,
                    rtFetchWholeBodyAsPrimaryText,
                    rtFetchBodyPart,
                    rtFetchMultipleBodyParts,
                    rtFetchMultipleMsgs,
                    rtFetchPrimaryTextPart,
                    rtFetchPrimaryHtmlPart,
                    rtFetchRawMessage,
                    rtUploadMbox,
                    rtUploadEmls,
                    rtDownloadMbox,
                    rtDownloadEmls,
                    rtEml2Mbox,
                    rtEml2Mboxes,
                    rtMbox2Eml,
                    rtMultipleQSDeletion,
                    rtUploadEMLsToRemoteServer,
                    rtDownloadForRemoteCopy,
                    rtSaveAttachments,
                    rtSaveAttachment,
                    rtViewAttachment,
                    rtDebugServer,
                    rtAccountBackup,
                    rtAccountBackupCommandLine,
                    rtRestoreBackup,
                    rtGetBodyStructures);

    { Defines the component that made the request }
    TRequestComponent = (rcMain, rcMsgPeeker, rcGlobalSearch,
        rcDestinationChooser, rcFolderSubs, rcFolderHierarchyReplicator,
        rcAccountBackup, rcSaveAttachmentsDlg);

    { Record holding information about the quota }
    TQuotaInfo = record
        current, maximum: Integer
    end;

    { Various information specific to a single request }
    TRequestDataInfo = record
        // Add anything else dependant on a single request here...
        mailboxIndex: Integer;
        destMailboxIndex: Integer;
        displayedMailboxIndex: Integer;
        msgUID: String;
        msgUIDs: TStringList;
        msgContent: TStringList;     // Used for passing messages in memory
        msgIDs: TStringList;         // Used for passing message-ids
        msgFlags: TMessageFlags;
        filename: String;
        filenames: TStringList;
        folder: String;
        folders: TStringList;
        folderSeparator: Char;
        searchString: String;
        fullMailboxName: String;
        newNodeName, newFullDisplayedName, newFullMailboxName: String;
        msgInfo: TMessageInfo;
        bodyPart: String;
        convertOptions: TConvertOptions;
        limitLength: Integer;  // e.g. used for limiting the length of fetched bodies, etc
        stringResult: String;  // used for storing string results...
        strQueue: TStringList; // FIFO queue for recieving updates from other requests
        masterRequestId: Integer; // ID of the master request
        copyOperation: TCopyOperation;  // used only for copy operations...
        encoding: String;
        partMimeType: String;
        partMimeSubtype: String;
        generalList: TList;     // TList used for various purposes
        backupFolders: TListOfBackupFolderInfos;    // List of folders for restoring backup
        restoreSameAccount: Boolean;                // True if the backup restore is performed to the original account
        accountName: String;                        

        // Result data
        quotaInfo: TQuotaInfo;
    end;

    { Information about the activity of the request }
    TRequestActivityInfo = record
        status: TRequestActivityStatus;
        percent: Integer;   // percentage of the request (used if applicable)
        errors: TStringList; // Used for errors only
        log: TStringList;  // Used for logging all important request info (including errors)
        summary: String;   // Summary of the request (message to user)
        summaryLevel: TMessageLevel;  // info, warning, error
    end;

    { Each request contains the following information }
    TRequest = class
        requestId: Integer;
        threadType: TThreadType;        // Type of thread that this request will use
        thread: TCustomThread;          // Thread used by this request
        threadId: Integer;              // Sort of redundant, but might be usefull if the thread is externally freed...
        connId: Integer;                // Connection ID used by this request (if any)
        accountInfoPtr: PAccountInfo;   // Pointer to the account this request is using
        requestType: TRequestType;      // Type of request (usually associated with imapOperation)
        requestComponent: TRequestComponent;  // Identifies the component (all singletons) that invoked this request
        data: TRequestDataInfo;         // Record that holds data needed for the processing of this request
        activityInfo: TRequestActivityInfo;     // Info about the request activity
        imapOperation: TIMAPOperation;          // IMAP operation to be performed (if applicable)
        convertOperation: TConvertOperation;    // Convert operation to be performed (if applicable)
        checkerOperation: TCheckerOperation;    // Checker operation to be performed (if applicable)
        commandLine: Boolean;           // True if the request is invoked from a command line (no GUI stuff)

        constructor Create(inThreadType: TThreadType);
        destructor Destroy; override;
        procedure FreeThread;
        procedure SetMsgUIDs(msgUIDs: TStringList);
        procedure SetMessageContents(msg: TStrings);
        procedure SetMsgFlags(flags: TMessageFlags);
        procedure SetFilenames(filenames: TStrings);
        procedure SetFolders(folders: TStrings);
        procedure SetBackupFolders(folders: TListOfBackupFolderInfos);
        procedure Log(msg: String);      // Logs a message to the request activity info
        procedure LogWarning(msg: String); 
        procedure LogError(msg: String); // Logs an error message to the request activity info
        procedure SetSummary(summary: String; summaryLvl: TMessageLevel);
        procedure AddToStringQueue(str: String);
    end;
    PRequest = ^TRequest;

    TRequestManager = class
    private
        cs: TRTLCriticalSection;
        procedure DeallocateHWnd(Wnd: HWND);
        procedure NotifyRequestOver(var request: TRequest; success: Boolean);
        procedure UpdateRequestActivityStatus(var request: TRequest; success: Boolean);
        function GetRequestByThread(threadId: Integer): PRequest;
        procedure SyncDisplayMessage(msg: String; msgLevel: TMessageLevel);
        procedure SyncDisplayMessageActivityInfo(reqId: Integer; var actInfo: TRequestActivityInfo);
        procedure PrepareMsgPeekerForDisplay(requestPtr: PRequest);
        procedure GetCopyOfRequestActivity(var fromInfo, toInfo: TRequestActivityInfo);
        procedure FreeActivityInfo(var actInfo: TRequestActivityInfo);
    public
        Handle: HWND;  // Handle for sending messages to this (non-windowed) object (see http://delphi.about.com/library/weekly/aa093003a.htm)
        mRequestCounter: Integer;
        requests: THashTable;
        connectionPool: TIMAPConnectionPool;
        constructor Create;
        destructor Destroy; override;
        function InvokeRequest(requestPtr: PRequest): Integer;
        function GetRequest(requestId: Integer): TRequest;
        procedure RemoveRequest(requestId: Integer; keepOnErrors: Boolean);
        procedure WndProc(var msg: TMessage); // All messages handled by this procedure
        procedure HandleGetConnection(var Message: TMessage); // message WM_GOTCONNECTION;
        procedure HandleSizeCheckOver(var Message: TMessage); // message WM_SIZE_CHECKER_OVER;
        procedure HandleConverterOver(var Message: TMessage); // message WM_CONVERTER_OVER;
        procedure HandleIMAPOpOver(var Message: TMessage); // message WM_IMAPOPOVER;
        procedure HandleUpdateProgressBar(var Message: TMessage); // message WM_PROGRESS_BAR_UPDATE;
        procedure HandleMainProgressBarVisibility(var Message: TMessage); //WM_MAIN_PROGRESS_BAR_VISIBILITY
        procedure HandleUpdateMessageList(var Message: TMessage);
        procedure GetActiveRequests(var numberOfActiveRequests, firstActiveRequestID: Integer);
        function IsAccountActive(accountPtr: PAccountInfo): Boolean;
        procedure Log(requestId: Integer; msg: String);
        procedure LogWarning(requestId: Integer; msg: String);
        procedure LogError(requestId: Integer; msg: String);
        procedure SetSummary(requestId: Integer; summary: String; summaryLvl: TMessageLevel);
        procedure AddToStringQueue(requestId: Integer; str: String);
        procedure DumpContents;
    end;

var
    threadMgr: TThreadManager;       // Thread manager (global)

implementation

uses Main, MsgPeeker, ActivityDlg, DisplayRequestLogDlg,
     DestinationChooser, FolderSubDlg, FolderHierarchyReplicatorDlg, AccountBackupDlg, SaveAttachmentsDlg, BackupDB;

{ =========== TRequest ============= }

constructor TRequest.Create(inThreadType: TThreadType);
begin
    threadType:=inThreadType;
    if (threadType <> thtNone) then begin
        thread:=threadMgr.createThread(threadType); // Creates the right thread based on the thread type
        threadId:=thread.ThreadInfo.ID;
    end;
    data.msgUIDs := TStringList.Create;
    data.msgIDs := TStringList.Create;
    data.filenames := TStringList.Create;
    data.folders := TStringList.Create;
    data.msgContent := TStringList.Create;
    data.strQueue := TStringList.Create;
    data.generalList := TList.Create;
    SetLength(data.backupFolders,0);
    activityInfo.errors := TStringList.Create;
    activityInfo.log := TStringList.Create;
    activityInfo.status := rasNotInvoked;
    activityInfo.summary:='';
    activityInfo.summaryLevel:=mlvInfo;  // Default
    activityInfo.percent := 0;
    connId:=-1; // default value, meaning no connection...
    commandLine:=false;  
end;

destructor TRequest.Destroy;
begin
    devLog.Trace('Destroying request '+IntToStr(requestId));
    // Free the thread in case it wasn't yet freed...
    FreeThread;
    data.msgUIDs.Free;
    data.msgIds.Free;
    data.filenames.Free;
    data.folders.Free;
    data.msgContent.Free;
    data.strQueue.Free;
    data.generalList.Clear;
    data.generalList.Free;
    SetLength(data.backupFolders,0);
    activityInfo.errors.Free;
    activityInfo.log.Free;
end;

{ Releases the thread (if not already freed) }
procedure TRequest.FreeThread;
begin
    if Assigned(thread) then threadMgr.removeThread(threadId)
    else devLog.Trace('Thread for this request has already been freed.');
end;

procedure TRequest.SetMsgUIDs(msgUIDs: TStringList);
begin
    CopyStringList(TStrings(data.msgUIDs), TStrings(msgUIDs));
end;

procedure TRequest.SetMessageContents(msg: TStrings);
begin
    CopyStringList(TStrings(data.msgContent), msg);
end;

procedure TRequest.SetMsgFlags(flags: TMessageFlags);
begin
    data.msgFlags:=flags;
end;

procedure TRequest.SetFilenames(filenames: TStrings);
begin
    CopyStringList(TStrings(data.filenames), filenames);
end;

procedure TRequest.SetFolders(folders: TStrings);
begin
    CopyStringList(TStrings(data.folders), folders);
end;

procedure TRequest.SetBackupFolders(folders: TListOfBackupFolderInfos);
var i: Integer;
begin
    SetLength(data.backupFolders,Length(folders));
    for i:=0 to Length(folders)-1 do begin
        data.backupFolders[i].remoteFolder:=folders[i].remoteFolder;
        data.backupFolders[i].localFolder:=folders[i].localFolder;
        data.backupFolders[i].newRemoteName:=folders[i].newRemoteName;
    end;
end;

{ Logs a message to the request activity info }
procedure TRequest.Log(msg: String);
begin
    activityInfo.log.Add(msg);
end;

procedure TRequest.LogWarning(msg: String);
begin
    // Add to both the log and errors lists
    activityInfo.log.Add(msg);
    activityInfo.errors.Add(msg);  //@todo add the warning list
end;

{ Logs an error message to the request activity info }
procedure TRequest.LogError(msg: String);
begin
    // Add to both the log and errors lists
    activityInfo.log.Add(msg);
    activityInfo.errors.Add(msg);
end;

procedure TRequest.SetSummary(summary: String; summaryLvl: TMessageLevel);
begin
    activityInfo.summary:=summary;
    activityInfo.summaryLevel:=summaryLvl;
end;

procedure TRequest.AddToStringQueue(str: String);
begin
    data.strQueue.Add(str);
end;

{ =========== TRequestManager ============= }

constructor TRequestManager.Create;
begin
    Handle := AllocateHWND(WndProc);
    mRequestCounter := 0;
    requests := THashTable.Create;
    connectionPool := TIMAPConnectionPool.Create;
    threadMgr := TThreadManager.Create;
    InitializeCriticalSection(cs);
end;

destructor TRequestManager.Destroy;
var reqInfoPtr: PRequest;
begin
    DeleteCriticalSection(cs);
    devLog.Trace('Shutting down the Request Manager');
    DeallocateHWnd(Handle);
    devLog.Trace('Deallocated windows handle');
    threadMgr.Free;
    devLog.Trace('Thread Manager shutdown');

    devLog.Trace('Freeing any existing requests');
    reqInfoPtr:=requests.First;
    while (reqInfoPtr<>nil) do begin
        reqInfoPtr^.Free;
        Dispose(reqInfoPtr);
        reqInfoPtr:=requests.Next;
    end;
    devLog.Trace('Requests freed');

    requests.Free;
    connectionPool.Free;
    devLog.Trace('Connection pool shutdown');

    devLog.Trace('Request Manager shutdown complete');
end;

procedure TRequestManager.DeallocateHWnd(Wnd: HWND);
var Instance: Pointer;
begin
    Instance := Pointer(GetWindowLong(Wnd, GWL_WNDPROC));
    if Instance <> @DefWindowProc then
        // make sure we restore the old, original windows procedure before leaving
        SetWindowLong(Wnd, GWL_WNDPROC, Longint(@DefWindowProc));
    FreeObjectInstance(Instance);
    DestroyWindow(Wnd);
end;

{ Adds a request to the list of requests and invokes it.
  Returns the ID associated with this request }
function TRequestManager.InvokeRequest(requestPtr: PRequest): Integer;
var getConnResponseInfoPtr: PGetConnectionResponseInfo;
begin

    EnterCriticalSection(cs);
        Inc(mRequestCounter);
        Result:=mRequestCounter;
        requestPtr^.requestId:=mRequestCounter;
        devLog.Trace('Invoke new request (ID='+IntToStr(requestPtr^.requestId)+')');
        if (requestPtr^.threadType<>thtNone) then begin
            // no threads for thtNone - see TRequest.Create
            requestPtr^.thread.threadInfo.requestId := mRequestCounter;
        end;
        requests[mRequestCounter]:=requestPtr;
    LeaveCriticalSection(cs);

    // Add to the activity dialog
    if not requestPtr.commandLine then FActivityDlg.AddRequest(mRequestCounter);

    // Mark as running
    requestPtr^.activityInfo.status:=rasRunning;
    if not requestPtr.commandLine then FActivityDlg.SetActivityImage(mRequestCounter,rasRunning);  

    if (requestPtr^.requestType = rtLogout) then begin
        // Stop any active requests (threads) for this account
        //@todo

        // Find connection(s) with this account and logout
        // If there are multiple connections remove all except one
        connectionPool.LogoutAccount(requestPtr^.accountInfoPtr);
        //@todo logout is currently not threaded
    end
    else begin
        // If connection is needed (IMAP call) get the connection.
        // The connection is currently needed for all thread types except the converter.
        if (requestPtr^.threadType <> thtConverter) then begin
            New(getConnResponseInfoPtr);
            getConnResponseInfoPtr^.requestId := requestPtr^.requestId;
            connectionPool.GetConnection(requestPtr^.accountInfoPtr^, getConnResponseInfoPtr);
            // Will get notified in HandleGetConnection (with populated connId and success)
            // Further processing for each request is defined there
        end
        else begin
            // Directly invoke the converter methods (threads)
            TConverterThread(requestPtr^.thread).SetOperation(requestPtr^.convertOperation);
            TConverterThread(requestPtr^.thread).SetOptions(requestPtr^.data.convertOptions);
            TConverterThread(requestPtr^.thread).SetFileList(requestPtr^.data.filenames);
            case requestPtr^.convertOperation of
                cnvOpEml2Mbox: TConverterThread(requestPtr^.thread).SetFile(requestPtr^.data.filename);
                cnvOpEml2Mboxes:
                    begin
                        TConverterThread(requestPtr^.thread).SetEmlDir(requestPtr^.data.folder);
                        TConverterThread(requestPtr^.thread).SetMboxDir(requestPtr^.data.filename);
                    end;
                cnvOpMbox2Eml: TConverterThread(requestPtr^.thread).SetFolders(requestPtr^.data.folders);
            end;
            TConverterThread(requestPtr^.thread).Priority:=tpLowest;
            TConverterThread(requestPtr^.thread).Resume;
            FMain.VCLAdaptConverterThreadRunning;
        end;
    end;
end;

procedure TRequestManager.WndProc(var msg: TMessage);
begin
    case msg.Msg of
        WM_GOTCONNECTION: HandleGetConnection(msg);
        WM_SIZE_CHECKER_OVER: HandleSizeCheckOver(msg);
        WM_IMAPOPOVER: HandleIMAPOpOver(msg);
        WM_CONVERTER_OVER: HandleConverterOver(msg);
        WM_PROGRESS_BAR_UPDATE: HandleUpdateProgressBar(msg);
        WM_MAIN_PROGRESS_BAR_VISIBILITY: HandleMainProgressBarVisibility(msg);
        WM_UPDATE_MESSAGE_LIST: HandleUpdateMessageList(msg);
    else
        // for all other messages call  the default window procedure
        msg.Result := DefWindowProc(Handle, Msg.Msg, Msg.wParam, Msg.lParam);
    end;
end;

{ Handles messages when a connection is retrieved }
procedure TRequestManager.HandleGetConnection(var Message: TMessage);
var getConnResponseInfoPtr: PGetConnectionResponseInfo;
    requestId, connId, success: Integer;
    request: TRequest;
    failureMsg: String;
begin
    devLog.Trace('TRequestManager.HandleGetConnection');

    EnterCriticalSection(cs);
    getConnResponseInfoPtr := PGetConnectionResponseInfo(Message.WParam);
    requestId := getConnResponseInfoPtr^.requestId;
    connId := getConnResponseInfoPtr^.connId;
    success := getConnResponseInfoPtr^.success;
    devLog.Trace('Login over for requestId='+IntToStr(requestId)+', connection id='+IntToStr(connID)+', success='+IntToStr(success));
    // Now perform tasks as needed
    request:=GetRequest(requestId);
    request.connId:=connId;
    LeaveCriticalSection(cs);

    if ((success = ord(lrHostNotFound)) or (success = ord(lrWrongUserPass))
            or (success = ord(lrTerminated))) then
    begin
        if success = ord(lrHostNotFound) then
            failureMsg := 'Can''t connect to server '+request.accountInfoPtr^.IMAPServer +
                          ' (port '+IntToStr(request.accountInfoPtr^.Port) + ') - Host not found or port invalid'
        else if success = ord(lrWrongUserPass) then
            // lrWrongUserPass
            failureMsg := 'Unable to login to server '+request.accountInfoPtr^.IMAPServer +' - Invalid username/password';
        // don't do anything for lrTerminated and lrDontDisplayResult

        UpdateRequestActivityStatus(request,false);
        if success <> ord(lrTerminated) then
            if consoleMode then backupLog.Error(failureMsg)
            else SyncDisplayMessage(failureMsg,mlvError);
        // Remove thread and inform invoker
        // LoginThread is self terminating and is not TCustomThread, but we still have the
        // CheckerThread running around...
        threadMgr.markDone(request.threadID);
        threadMgr.removeThread(request.threadId);
        FMain.VCLAdaptLoginThreadStopped;
        NotifyRequestOver(request, false);
    end
    else begin
        // OK, logged in, we can now perform the request (only IMAPworker and CheckerThread)
        if (request.threadType = thtIMAPWorker) or (request.threadType=thtChecker) then begin
            // First set the connection for the thread and other common tasks
            if request.threadType = thtIMAPWorker then begin
                TIMAPWorker(request.thread).SetIMAPConnection(connectionPool.GetConnection(connId, requestId));
                TIMAPWorker(request.thread).Operation:=request.imapOperation;
                FMain.VCLAdaptIMAPWorkerRunning;
            end
            else if request.threadType = thtChecker then begin
                // TCheckerThread(request.thread).Priority:=tpLowest;
                TCheckerThread(request.thread).SetIMAPConnection(connectionPool.GetConnection(connId,requestId));
                TCheckerThread(request.thread).SetOperation(request.checkerOperation);
                FMain.PrepareGUIForSizeCheck;
                FMain.VCLAdaptCheckerThreadRunning;
            end
            else devLog.error('Unknown thread type'); // we are never requesting connections for converter threads
            request.thread.Priority:=tpLowest;

            case request.requestType of

                // Checker thread related requests
                rtLogin, rtCheckSize: ; // do nothing (resume thread)

                // IMAPWorker related requests
                rtGetFolderList, rtGetSubscribedFolderList, rtGetQuota, rtDebugServer:
                    begin
                        // Nothing specific to set for this request... (resume thread)
                    end;
                rtSubscribe:
                    begin
                        // Nothing, the worker thread will use request.data
                    end;
                rtGeneralIMAP:
                    begin
                        TIMAPWorker(request.thread).FullMailboxName:=mboxTree.Items[request.data.mailboxIndex].mFullMailboxName;
                        TIMAPWorker(request.thread).SelectedMailboxIndex:=request.data.mailboxIndex;
                    end;
                rtMessageIMAP, rtFetchMultipleMsgs, rtMultipleQSDeletion, rtGetBodyStructures:
                    begin
                        TIMAPWorker(request.thread).FullMailboxName:=mboxTree.Items[request.data.mailboxIndex].mFullMailboxName;
                        TIMAPWorker(request.thread).SelectedMailboxIndex:=request.data.mailboxIndex;
                        TIMAPWorker(request.thread).setMsgUIDs(request.data.msgUIDs);
                    end;
                rtSearch:
                    begin
                        TIMAPWorker(request.thread).FullMailboxName:=mboxTree.Items[request.data.mailboxIndex].mFullMailboxName;
                        TIMAPWorker(request.thread).SelectedMailboxIndex:=request.data.mailboxIndex;
                        TIMAPWorker(request.thread).SearchString:=request.data.searchString;
                    end;
                rtGlobalSearch: TIMAPWorker(request.thread).SearchString:=request.data.searchString;
                rtCopy:
                    begin
                        TIMAPWorker(request.thread).FullMailboxName:=mboxTree.Items[request.data.mailboxIndex].mFullMailboxName;
                        TIMAPWorker(request.thread).SelectedMailboxIndex:=request.data.mailboxIndex;
                        TIMAPWorker(request.thread).DestMailboxIndex:=request.data.destMailboxIndex;
                        TIMAPWorker(request.thread).FullDestMailboxName:=mboxTree.Items[request.data.destMailboxIndex].mFullMailboxName;
                        TIMAPWorker(request.thread).setMsgUIDs(request.data.msgUIDs);
                    end;
                rtRename:
                    begin
                        TIMAPWorker(request.thread).FullMailboxName:=mboxTree.Items[request.data.mailboxIndex].mFullMailboxName;
                        TIMAPWorker(request.thread).SelectedMailboxIndex:=request.data.mailboxIndex;
                        TIMAPWorker(request.thread).NewFullDisplayedName:=request.data.newFullDisplayedName;
                        TIMAPWorker(request.thread).NewFullMailboxName:=request.data.newFullMailboxName;
                        TIMAPWorker(request.thread).NewNodeName:=request.data.newNodeName;
                    end;
                rtSaveHeaders:
                    begin
                        TIMAPWorker(request.thread).FullMailboxName:=mboxTree.Items[request.data.displayedMailboxIndex].mFullMailboxName;
                        TIMAPWorker(request.thread).setMsgUIDs(request.data.msgUIDs);
                        TIMAPWorker(request.thread).Filename:=request.data.FileName;
                    end;
                rtPrepareForMessageViewing:
                    begin
                        TIMAPWorker(request.thread).FullMailboxName:=mboxTree.Items[request.data.mailboxIndex].mFullMailboxName;
                        TIMAPWorker(request.thread).SelectedMailboxIndex:=request.data.mailboxIndex;
                        request.data.msgUID:=request.data.msgInfo.mUID;
                        TIMAPWorker(request.thread).MsgUID:=request.data.msgInfo.mUID;
                    end;
                rtFetch, rtFetchHeader, rtFetchWholeBody, rtFetchWholeBodyAsPrimaryText, rtFetchRawMessage,
                rtFetchHeaderForQSDeletion, rtFetchHeaderForRemoteSaving, rtFetchHeaderForLocalSaving:
                    begin
                        TIMAPWorker(request.thread).FullMailboxName:=mboxTree.Items[request.data.mailboxIndex].mFullMailboxName;
                        TIMAPWorker(request.thread).SelectedMailboxIndex:=request.data.mailboxIndex;
                        if request.requestType=rtFetch
                            then TIMAPWorker(request.thread).MsgUID:=request.data.msgInfo.mUID
                            else TIMAPWorker(request.thread).MsgUID:=request.data.msgUID;
                    end;
                rtFetchBodyPart, rtFetchPrimaryTextPart, rtFetchPrimaryHTMLPart, rtFetchMultipleBodyParts,
                rtSaveAttachment, rtViewAttachment:
                    // These request types are the same at this stage, only difference is
                    // the behaviour once the body part has been retrieved...
                    begin
                        TIMAPWorker(request.thread).FullMailboxName:=mboxTree.Items[request.data.mailboxIndex].mFullMailboxName;
                        TIMAPWorker(request.thread).SelectedMailboxIndex:=request.data.mailboxIndex;
                        TIMAPWorker(request.thread).MsgUID:=request.data.msgUID;
                        TIMAPWorker(request.thread).BodyPart:=request.data.bodyPart;
                    end;
                rtSaveAttachments:
                    begin
                        TIMAPWorker(request.thread).FullMailboxName:=mboxTree.Items[request.data.mailboxIndex].mFullMailboxName;
                        TIMAPWorker(request.thread).SelectedMailboxIndex:=request.data.mailboxIndex;
                        TIMAPWorker(request.thread).setMsgUIDs(request.data.msgUIDs);
                    end;
                rtUploadMbox:
                    begin
                        TIMAPWorker(request.thread).SelectedMailboxIndex:=request.data.mailboxIndex;
                        TIMAPWorker(request.thread).FullMailboxName:=mboxTree.Items[request.data.mailboxIndex].mFullMailboxName;
                        TIMAPWorker(request.thread).Filename:=request.data.filename;
                    end;
                rtUploadEMLs:
                    begin
                        TIMAPWorker(request.thread).SelectedMailboxIndex:=request.data.mailboxIndex;
                        TIMAPWorker(request.thread).FullMailboxName:=mboxTree.Items[request.data.mailboxIndex].mFullMailboxName;
                        TIMAPWorker(request.thread).setFilenames(request.data.filenames);
                    end;
                rtDownloadMbox, rtDownloadEMLs:
                    begin
                        TIMAPWorker(request.thread).FullMailboxName:=request.data.fullMailboxName;
                        TIMAPWorker(request.thread).setMsgUIDs(request.data.msgUIDs);
                        TIMAPWorker(request.thread).Filename:=request.data.filename;
                    end;
                rtAppend:
                    begin
                        TIMAPWorker(request.thread).FullMailboxName:=mboxTree.Items[request.data.mailboxIndex].mFullMailboxName;
                        TIMAPWorker(request.thread).SelectedMailboxIndex:=request.data.mailboxIndex;
                        TIMAPWorker(request.thread).setMessageContents(request.data.msgContent);
                        TIMAPWorker(request.thread).MsgUID:=request.data.msgUID;
                        TIMAPWorker(request.thread).setMsgFlags(request.data.msgFlags);
                    end;
                rtCreateMailbox:
                    begin
                        TIMAPWorker(request.thread).FullMailboxName:=request.data.FullMailboxName;
                        TIMAPWorker(request.thread).SelectedMailboxIndex:=request.data.mailboxIndex;
                    end;
                rtUploadEMLsToRemoteServer:
                    begin
                        TIMAPWorker(request.thread).FullDestMailboxName:=request.data.fullMailboxName;
                        // TIMAPWorker(request.thread).SetMsgUIDs(request.data.msgUIDs);  // Will use it directly
                    end;
                rtDownloadForRemoteCopy:
                    begin
                        TIMAPWorker(request.thread).FullMailboxName:=mboxTree.Items[request.data.mailboxIndex].mFullMailboxName;
                        TIMAPWorker(request.thread).SetMsgUIDs(request.data.msgUIDs);
                        TIMAPWorker(request.thread).Filename:=request.data.filename;  // Temporary folder name
                    end;
                rtAccountBackup, rtAccountBackupCommandLine:
                    begin
                        TIMAPWorker(request.thread).SetFolders(request.data.folders);
                        TIMAPWorker(request.thread).Filename:=request.data.filename;  // Backup dir (includes account dir)
                        TIMAPWorker(request.thread).FolderSeparator:=request.data.folderSeparator;
                        TIMAPWorker(request.thread).CommandLineMode:=request.commandLine;
                    end;
                rtRestoreBackup:
                    begin
                        // Cut out the middle man. Will use the following data directly
                        // request.data.backupFolders
                        // request.data.folderSeparator
                        // request.data.restoreSameAccount
                    end;
            else devLog.Warn('Unknown request type: ' + IntToStr(ord(request.requestType)));
            end;

            request.thread.Resume;
        end;
    end;
end;

{ Updates the status and the status image of the request }
procedure TRequestManager.UpdateRequestActivityStatus(var request: TRequest; success: Boolean);
begin
    if success then begin
        if request.activityInfo.errors.Count=0 then begin
            request.activityInfo.status:=rasCompleted;
            if not request.commandLine then FActivityDlg.SetActivityImage(request.RequestId,rasCompleted);
        end
        else begin
            request.activityInfo.status:=rasCompletedWithErrors;
            if not request.commandLine then FActivityDlg.SetActivityImage(request.RequestId,rasCompletedWithErrors);
        end;
    end
    else begin
        request.activityInfo.status:=rasFailed;
        if not request.commandLine then FActivityDlg.SetActivityImage(request.RequestId,rasFailed);
    end;
end;

{ Notifies the invoker of the request that the request has finished.
  You should make sure that the thread is freed at this point... }
procedure TRequestManager.NotifyRequestOver(var request: TRequest; success: Boolean);
var msg: TMessage;
begin
    // The thread has been freed in the calling methods, mark as nil
    request.thread:=nil;
    devLog.Trace('TRequestManager.NotifyRequestOver');

    EnterCriticalSection(cs);
    msg.Msg := WM_REQUEST_OVER;
    msg.WParam := request.requestId;
    // We no longer need the connection so we return it to the pool.
    // This is a single point of return (no need to perform anywhere else)
    // Only return if connection is assigned
    if (request.connId <> -1) then
        connectionPool.ReturnConnection(request.connId);
    LeaveCriticalSection(cs);

    // Notify the requesting component...
    if success then msg.LParam:=1
    else msg.LParam:=0;
    devLog.Trace('TRequestManager.NotifyRequestOver - about to post message to invoking component');
    case request.requestComponent of
        rcMain: PostMessage(FMain.Handle,msg.Msg,msg.WParam,msg.LParam);
        rcMsgPeeker: PostMessage(FMsgPeeker.Handle,msg.Msg,msg.WParam,msg.LParam);
        rcGlobalSearch: PostMessage(FMain.FGlobalSearch.Handle,msg.Msg,msg.WParam,msg.LParam);
        rcDestinationChooser: PostMessage(FDestinationChooserDlg.Handle,msg.Msg,msg.WParam,msg.LParam);
        rcFolderSubs: PostMessage(FFolderSubDlg.Handle,msg.Msg,msg.WParam,msg.LParam);
        rcFolderHierarchyReplicator: PostMessage(FFolderHierarchyReplicatorDlg.Handle,msg.Msg,msg.WParam,msg.LParam);
        rcAccountBackup: PostMessage(FAccountBackupDlg.Handle,msg.Msg,msg.WParam,msg.LParam);
        rcSaveAttachmentsDlg: PostMessage(FMain.FSaveAttachmentsDlg.Handle,msg.msg,msg.WParam,msg.LParam);
        else devLog.Warn('Unknown request invoker!');
    end;
end;

{ Handles message WM_SIZE_CHECKER_OVER }
procedure TRequestManager.HandleSizeCheckOver(var Message: TMessage);
var threadId: Integer; requestId: Integer;
begin
    devLog.Trace('RequestManager.HandleSizeCheckOver');
    threadID:=Message.WParam;
    // Stop the thread
    threadMgr.markDone(threadID);
    threadMgr.removeThread(threadId);
    // Update gui
    FMain.VCLAdaptCheckerThreadStopped;
    UpdateRequestActivityStatus(PRequest(GetRequestByThread(threadId))^,true);
    // Send message
    NotifyRequestOver(PRequest(GetRequestByThread(threadId))^,true);
    // Request will be freed by the main thread when not needed anymore...
end;

{ Handles message WM_CONVERTER_OVER }
procedure TRequestManager.HandleConverterOver(var Message: TMessage);
var threadId: Integer; requestId: Integer; tInfo: TThreadInfo; reqActInfo: TRequestActivityInfo;
begin
    devLog.Trace('RequestManager.HandleConverterOver');
    threadID:=Message.WParam;
    tInfo := threadMgr.getThreadInfo(threadId);
    threadMgr.markDone(threadID);
    threadMgr.removeThread(threadId);

    FMain.VCLAdaptConverterThreadStopped;
    UpdateRequestActivityStatus(PRequest(GetRequestByThread(threadId))^,true);

    // Get the RequestActivityInfo so we can free the request
    GetCopyOfRequestActivity(PRequest(GetRequestByThread(threadId))^.activityInfo,reqActInfo);
    // Send message
    NotifyRequestOver(PRequest(GetRequestByThread(threadId))^,true);
    // Display the message
    SyncDisplayMessageActivityInfo(PRequest(GetRequestByThread(threadId))^.requestId,reqActInfo);
    FreeActivityInfo(reqActInfo);
end;

{ Handles WM_IMAPOPOVER messages sent by TIMAPWorker when finished regularly }
procedure TRequestManager.HandleIMAPOpOver(var Message: TMessage);
var tInfo: TThreadInfo; requestPtr: PRequest; threadID: Integer; showPeeker: Boolean;
begin
    showPeeker:=false;
    devLog.Trace('RequestManager.HandleIMAPOpOver');
    threadID:=Message.WParam;
    tInfo := threadMgr.getThreadInfo(threadId);
    threadMgr.markDone(threadID);
    threadMgr.removeThread(threadId);
    requestPtr:=GetRequestByThread(threadId);

    // Update VCL
    FMain.VCLAdaptIMAPWorkerStopped;
    UpdateRequestActivityStatus(requestPtr^,true);

    if not tInfo.WasTerminated then begin
        devLog.Trace('RequestManager.HandleIMAPOver, tInfo.WasTerminated is false...');
        if requestPtr^.requestType=rtPrepareForMessageViewing then begin
            PrepareMsgPeekerForDisplay(requestPtr);
            showPeeker:=true;
        end
        else begin
            case requestPtr^.imapOperation of
                // iopFetchBodyPart: FMsgPeeker.PopulateStrippedBody(FMsgPeeker.mFetchedBodyPart);
                iopGetQuota: begin
                    // If there is no error messages, display the quota (see TIMAPWorker.DisplayQuota)
                    if requestPtr^.activityInfo.summary='' then PostMessage(FMain.Handle, WM_SHOW_QUOTA, Integer(requestPtr), 0);
                  end;
                iopCreateMbox, iopDeleteMbox, iopRenameMbox:
                    if requestPtr^.activityInfo.summary='' then PostMessage(FMain.Handle, WM_HIERARCHY_CHANGE, 0, 0);
                iopSearchGlobal, iopSearchAdvancedGlobal: begin
                    if requestPtr^.activityInfo.summary='' then PostMessage(FMain.FGlobalSearch.Handle, WM_GLOBAL_SEARCH_OVER, 0, 0);
                end;
            else
                // do nothing
            end;
            SyncDisplayMessageActivityInfo(requestPtr^.requestId,requestPtr^.activityInfo);
        end;
    end
    else begin
        // Thread was terminated, don't perform any actions
        devLog.Debug('Thread was terminated, not performing any actions');
    end;

    // Send message
    NotifyRequestOver(requestPtr^,true);

    // This is a hack. If the ShowModal was placed before the NotifyRequestOver then
    // the request would not finish until the dialog is closed...
    if showPeeker then FMsgPeeker.ShowModal;

end;

{ Handles message WM_PROGRESS_BAR_UPDATE }
procedure TRequestManager.HandleUpdateProgressBar(var Message: TMessage);
var requestID, position, activeCnt, firstActive: Integer; request: TRequest;
begin
    requestID:=Message.WParam;
    position := Message.LParam;
    request:=GetRequest(requestId);
    if not request.commandLine then begin
        // Update the progress bar in the activity dialog
        FActivityDlg.UpdateProgress(requestID, position);
        // Also update the progress bar if this is the only active request
        // or if it is the first active request.
        GetActiveRequests(activeCnt, firstActive);
        if (activeCnt=1) or ((activeCnt>1) and (requestID=firstActive)) then begin
            PostMessage(FMain.Handle, WM_PROGRESS_BAR_UPDATE, 0, position);
        end;
    end;
end;

{ Behaves like a proxy for WM_MAIN_PROGRESS_BAR_VISIBILITY,
  will forward the message to Main only if it makes sense to do so:
  If should set visible, forwards only if there is no other active requests
  If should set invisible, forwards only if this is the last active request.
  This means, in both cases, forward the message if there is only one active request }
procedure TRequestManager.HandleMainProgressBarVisibility(var Message: TMessage);
var visible, activeCnt, firstActive: Integer;
begin
    visible := Message.LParam;
    GetActiveRequests(activeCnt,firstActive);
    if (activeCnt=1) then PostMessage(FMain.Handle, WM_MAIN_PROGRESS_BAR_VISIBILITY,0, visible);
end;

procedure TRequestManager.HandleUpdateMessageList(var Message: TMessage);
begin
    PostMessage(FMain.Handle, WM_UPDATE_MESSAGE_LIST,0, 0);
end;

function TRequestManager.GetRequest(requestId: Integer): TRequest;
begin
    Result:=PRequest(requests.Items[requestId])^;
end;

{ Gets a request that handles the specified thread.
  Will return nil if the request is not found... }
function TRequestManager.GetRequestByThread(threadId: Integer): PRequest;
var reqInfoPtr: PRequest; found: Boolean;
begin
    EnterCriticalSection(cs);
    found:=false;
    reqInfoPtr:=requests.First;
    while (reqInfoPtr<>nil) and (not found) do begin
        if reqInfoPtr^.threadId = threadId then found:=true
        else reqInfoPtr:=requests.Next;
    end;
    LeaveCriticalSection(cs);
    if found then begin
        Result:=reqInfoPtr
    end
    else begin
        Result:=nil;
    end;
end;

{ Public method to remove the request from the list of requests }
{ if keepOnErrors is true, the request will be removed only if the
  request doesn't have any errors (activityInfo.errors is empty),
  otherwise the request is still kept in the system. This is used
  for keeping the request in the activity dialog until the user
  selects to clear it. Note that the thread should have been freed by now. }
{ Note: I've added the DisplayRequestLogDlg through which the user gets
  information about errors. This dialog will remove the request from
  the activity dlg, so even if keepOnErrors is true, if this dialog has been
  invoked (user has seen the error details) it will have no effect
  (the request will already be removed from the activity dlg) }
procedure TRequestManager.RemoveRequest(requestId: Integer; keepOnErrors: Boolean);
var requestPtr: PRequest; shouldRemoveRequest: Boolean;
begin
    EnterCriticalSection(cs);
    requestPtr:=requests[requestId];
    shouldRemoveRequest:=true;
    if requestPtr<>nil then begin
        if keepOnErrors and (requestPtr^.activityInfo.status>rasCompleted) then begin
            shouldRemoveRequest:=false;
            // make sure the thread is removed
            requestPtr^.FreeThread;
        end
        else begin
            requestPtr^.Free;  // Will free the thread too
            Dispose(requestPtr);
            if not requestPtr.commandLine then FActivityDlg.RemoveRequest(requestId);
        end;
    end;
    if shouldRemoveRequest then requests.Remove(requestId);
    LeaveCriticalSection(cs);
end;

{ Returns the number of active requests (rasRunning)
  and the request ID of the first active request (-1 if none) }
procedure TRequestManager.GetActiveRequests(var numberOfActiveRequests, firstActiveRequestID: Integer);
var reqInfoPtr: PRequest; cnt, first: Integer;
begin
    numberOfActiveRequests:=0; firstActiveRequestID:=MAXINT;
    reqInfoPtr:=requests.First;
    while (reqInfoPtr<>nil) do begin
        if reqInfoPtr^.activityInfo.status=rasRunning then begin
            numberOfActiveRequests:=numberOfActiveRequests+1;
            if reqInfoPtr^.requestId<firstActiveRequestID then firstActiveRequestID:=reqInfoPtr^.requestId;
        end;
        reqInfoPtr:=requests.Next;
    end;
end;

{ Checks if any connections for the specified account are busy }
function TRequestManager.IsAccountActive(accountPtr: PAccountInfo): Boolean;
begin
    Result:=connectionPool.IsAccountActive(accountPtr^);
end;

procedure TRequestManager.Log(requestId: Integer; msg: String);
begin
    PRequest(requests.Items[requestId])^.Log(msg);
end;

procedure TRequestManager.LogWarning(requestId: Integer; msg: String);
begin
    PRequest(requests.Items[requestId])^.LogWarning(msg);
end;

procedure TRequestManager.LogError(requestId: Integer; msg: String);
begin
    PRequest(requests.Items[requestId])^.LogError(msg);
end;

procedure TRequestManager.SetSummary(requestId: Integer; summary: String; summaryLvl: TMessageLevel);
begin
    PRequest(requests.Items[requestId])^.SetSummary(summary, summaryLvl);
end;

procedure TRequestManager.AddToStringQueue(requestId: Integer; str: String);
begin
    PRequest(requests.Items[requestId])^.AddToStringQueue(str);
end;

{ Displays the header and body }
procedure TRequestManager.PrepareMsgPeekerForDisplay(requestPtr: PRequest);
begin
    FMsgPeeker.PrepareContent(requestPtr);
end;

{ Displays a message to the user taking care that no sync issues happen
  No need to sync, this is called in the main VCL thread... }
procedure TRequestManager.SyncDisplayMessage(msg: String; msgLevel: TMessageLevel);
begin
    case msgLevel of
        mlvInfo: MessageDlg(msg,mtInformation,[mbOk],0);
        mlvWarn: MessageDlg(msg,mtWarning,[mbOk],0);
        mlvError: MessageDlg(msg,mtError,[mbOk],0);
    end;
end;

{ Displays a message based on values of the TRequestActivityInfo }
procedure TRequestManager.SyncDisplayMessageActivityInfo(reqId: Integer; var actInfo: TRequestActivityInfo);
var msgType: TMsgDlgType; FDisplayRequestLogDlg: TFDisplayRequestLogDlg; request: TRequest;
begin
    // Display message only if there is a summary...
    request:=GetRequest(reqId);
    if not request.commandLine then begin
        if actInfo.summary<>'' then begin
            if (actInfo.log.Count=0) then begin
                // There's no log, display a simple messagedlg
                SyncDisplayMessage(actInfo.summary, actInfo.summaryLevel);
            end
            else begin
                // There is some logged info, display the Details button too...
                case actInfo.summaryLevel of
                    mlvWarn: msgType:=mtWarning;
                    mlvError: msgType:=mtError;
                else
                    msgType:=mtInformation; // default
                end;
                // Invoke a customized messagedlg (MyUtil.pas)
                if MyMessageDialog(actInfo.summary, msgType, ['Details...', 'OK']) = 1 then begin
                    // The Details button has been clicked, invoke the DisplayRequestLogDlg
                    FDisplayRequestLogDlg:=TFDisplayRequestLogDlg.Create(Application);
                    FDisplayRequestLogDlg.SetLogs(actInfo.log, actInfo.errors);
                    FDisplayRequestLogDlg.PopulateMemo;
                    FDisplayRequestLogDlg.ShowModal;
                    FDisplayRequestLogDlg.Free;
                end;
                // Remove request from the activity dialog.
                // Also see comments on TRequestManager.RemoveRequest
                // FActivityDlg.RemoveRequest(requestId);
            end;
        end;
    end;
end;

procedure TRequestManager.DumpContents;
var reqInfoPtr: PRequest;
begin
    devLog.Trace('====== RequestManager contents: =========');
    reqInfoPtr:=requests.First;
    while (reqInfoPtr<>nil) do begin
        devLog.Trace(Format('requestID=%2d, threadId=%2d',[reqInfoPtr^.requestId,reqInfoPtr^.threadId]));
        reqInfoPtr:=requests.Next;
    end;
    devLog.Trace('====== DONE =========');
end;

{ Following two methods are used for managing the copy of an activity info.
  This is necessary since the activity info might outlive the request.
  NOTE: A record created with this method should use the next method to free
  its fields.
  //@todo Should consider having a class instead of a record... }
procedure TRequestManager.GetCopyOfRequestActivity(var fromInfo, toInfo: TRequestActivityInfo);
var i: Integer;
begin
    toInfo.errors:=TStringList.Create;
    toInfo.log:=TStringList.Create;
    for i:=0 to fromInfo.errors.Count-1 do toInfo.errors.Add(fromInfo.errors.Strings[i]);
    for i:=0 to fromInfo.log.Count-1 do toInfo.log.Add(fromInfo.log.Strings[i]);
    toInfo.status:=fromInfo.status;
    toInfo.summary:=fromInfo.summary;
    toInfo.summaryLevel:=fromInfo.summaryLevel;
end;

{ Free the specified activity info }
procedure TRequestManager.FreeActivityInfo(var actInfo: TRequestActivityInfo);
begin
    actInfo.errors.Clear;
    actInfo.log.Clear;
end;

end.
