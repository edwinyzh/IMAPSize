{==============================================================================|
| Project : IMAPSize                                                           |
|==============================================================================|
| $Id: CheckerThread.pas,v 1.21 2004/04/04 20:17:12 Ivan Exp $
|==============================================================================|
| Copyright 2003-2004 Ivan Vecanski                                            |
| =============================================================================}

unit CheckerThread;

interface

uses windows,classes, Accounts, NodeStack, Nodes, dialogs, sysutils, SplitFns,
     IMAPConnection, VecLog, CustomThread, Log, GlobalConstants, ICache, MailboxTree,
     HashTables, messages, Namespace;

type

TCheckerOperation = (chOpNone, chOpHierarchyOnly, chOpCheckSize);

{ Thread for performing initial check of account size }
TCheckerThread = class(TCustomThread)
private
    mIMAPConn: TIMAPConnection;
    mProgressBarPosition: Integer;
    mStatusBarText: String;
    mStatusBarPanelIndex: Integer;
    mMessageText: String;
    mboxes: TStringList;
    mSeparator: Char;           // Separator used by the current server
    mVirtualINBOXInboxAdded: Boolean; // True if the virtual INBOX.Inbox node is added to the list of mailboxes

    mCacheEnabled: Boolean;   // just a shorter version of settings.miscsettings.CacheEnabled
    mNoInferiorsHash: TStringTable;  // HashTable for temporary storage of NoInferior mailbox attributes

    function getQuota(var currentQuota, maxQuota: Integer): Integer;
    procedure DisplayQuotaResultsOnStatusBar;
    procedure UpdateProgressBar(newPosition: Integer);
    procedure UpdateStatusBar(newText: String; panelIndex: Integer);
    procedure InternalUpdateStatusBar;
    procedure SyncDisplayMessage(msg: String);
    procedure InternalDisplayMessage;
    function GetMailboxes(var mboxes: TStringList): Boolean;
    function ParseUidRfc822Reply(var uid: LongInt; var rfcSize: Integer; line: String): Boolean;
    procedure GetSizeOfMessages(uidRange: Boolean; msgRange: String; var totalMsgSize: Int64; var maxUID: LongInt; var cachedMsgs: THashTable; updateCache: Boolean);
    procedure SearchUIDs(criteria: String; var res: TStringList);
    procedure RemoveDeletedMsgsFromCache(var cachedMsgs: THashTable; const existingMsgUIDs: TStringList);
    procedure GetMailboxSize(var totalMsgSize: Int64; var maxUID: LongInt; remoteMailboxName: String);
    procedure GetMailboxSizeUsingCache(var totalMsgSize: Int64; var maxUID: LongInt; remoteMailboxName: String; realMboxNumMsgs: Integer; realMboxUIDValidity: LongInt);
    function GetSeparator: Char;
    function GetNamespaceString: String;
    function ShouldIncludeFolder(folderName: String; ns: TNamespace; includeShared, includePublic: Boolean; rootFolder: String; excludeRoot: Boolean): Boolean;
    function GetFullFolderName(listReplyLine: String): String;
    function GetParentFolderName(fName: String): String;
    procedure GetFoldersFromPath(var path: TStringList; fullFolderName: String; separator: Char);
    function GetNoInferiors(listReplyLine: String): Boolean;
    procedure addVirtualRootNode(var stack: TNodeStack);
    procedure FullTreeUpdate;
public
    operation: TCheckerOperation;
    constructor Create(CreateSuspended: Boolean);
    procedure Execute; override;
    procedure SetIMAPConnection(imapConn: TIMAPConnection);
    procedure SetOperation(op: TCheckerOperation);
    function CreateMailboxHierarchy: Boolean;
    procedure CheckAccountSize(progressBarStartPos, progressBarEndPos: Integer);
    procedure DoCalculations(checkSize: Boolean;
                             var stack: TNodeStack; var folders: TStringList;
                             var parentNodeObj: TNodeRecord;
                             var NodeLevel, prevLevel, tempLevel, absIndexInTree: Integer;
                             nodeName: String;
                             mboxCnt: Integer);
    destructor Destroy; override;
end;

implementation

uses Main, MyTypes, MyUtil, RegExpr;

{ TCheckerThread }

constructor TCheckerThread.Create(CreateSuspended: Boolean);
begin
    Inherited Create(CreateSuspended, thtChecker);
    //FreeOnTerminate:=true;
end;

procedure TCheckerThread.SetIMAPConnection(imapConn: TIMAPConnection);
begin
    mIMAPConn:=imapConn;
end;

procedure TCheckerThread.SetOperation(op: TCheckerOperation);
begin
    operation:=op;
end;

procedure TCheckerThread.Execute;

    { Updates the VCL when the thread is terminated }
    procedure UpdateVCLCheckTerminated;
    begin
        UpdateStatusBar('Check aborted',STATUSBAR_MSGS);
        UpdateProgressBar(0);
        Synchronize(FMain.ClearTreeList);
    end;

begin
    mCacheEnabled:=settings.MiscSettings.EnableCaching;
    if mCacheEnabled then mCache.LoadMasterCache(mIMAPConn.conn.account.Name);
    mVirtualINBOXInboxAdded:=false;
    mboxes := TStringList.create;
    mNoInferiorsHash := TStringTable.Create;
    try
        if operation=chOpHierarchyOnly then begin
            UpdateStatusBar('Retrieving hierarchy',STATUSBAR_MSGS);
            DisplayQuotaResultsOnStatusBar;
            if not CreateMailboxHierarchy then begin
                requestMgr.SetSummary(ThreadInfo.RequestID,'Error occurred while trying to retrieve the mailbox hierarchy',mlvError);
                UpdateVCLCheckTerminated;
            end
            else begin
                treeState.content:=tcHierarchy;
                devLog.Trace('Mailbox hierarchy successfully retrieved');
                treeState.account:=FMain.pActiveAccount;
                mboxTree.CalculateSizes;     // necessary for painting the tree... Not much of an overhead
                mboxTree.SetImageIndexes;
                Synchronize(FMain.PopulateTree);
                mMessageLists.createMailboxes(mboxTree.Count);
            end;
        end
        else begin // operation = copCheckSize
            UpdateProgressBar(0);
            DisplayQuotaResultsOnStatusBar;
            UpdateProgressBar(5);
            if CreateMailboxHierarchy then begin
                UpdateProgressBar(15);
                if terminated then UpdateVCLCheckTerminated
                else begin
                    treeState.content:=tcSize;
                    treeState.account:=FMain.pActiveAccount;
                    mboxTree.CalculateSizes;
                    if terminated then UpdateVCLCheckTerminated
                    else begin
                        mboxTree.SetImageIndexes;
                        Synchronize(FMain.PopulateTree);  // populate tree with mailboxes
                        mMessageLists.createMailboxes(mboxTree.Count);
                        if terminated then UpdateVCLCheckTerminated
                        else begin
                            CheckAccountSize(15,95);  // params are start/end progress bar values
                            if terminated then UpdateVCLCheckTerminated
                            else begin
                                UpdateStatusBar('Performing Calculations',STATUSBAR_MSGS);
                                mboxTree.CalculateSizes;
                                if terminated then UpdateVCLCheckTerminated
                                else begin
                                    mboxTree.SetImageIndexes;
                                    Synchronize(FullTreeUpdate);
                                    mboxTree.MakeListOfNodesForSize;
                                    if terminated then UpdateVCLCheckTerminated
                                    else begin
                                        mboxTree.SortListOfNodesForSize;
                                        UpdateStatusBar('',STATUSBAR_MSGS);
                                        UpdateProgressBar(0);
                                        if terminated then UpdateVCLCheckTerminated
                                        else begin
                                            // mMessageLists.createMailboxes(mboxTree.Count);
                                            if terminated then UpdateVCLCheckTerminated
                                            else begin
                                                Synchronize(FMain.SetInitialDestinationMailbox);
                                                FMain.mLastSizeCheckedAccount:=mIMAPConn.conn.account.Name;
                                                FMain.EnableCSVExport(true);
                                                FMain.LogNodesArray('After checking');
                                            end;
                                        end;
                                    end;
                                end;
                            end;
                        end;
                    end;
                end;
            end
            else begin
                requestMgr.SetSummary(ThreadInfo.RequestID,'Error occurred while trying to retrieve the mailbox hierarchy',mlvError);
                UpdateVCLCheckTerminated;  // could not create mailbox hierarchy
            end;
        end; // operation = copCheckSize
    finally
        devLog.Trace('TCheckerThread.Execute - Thread terminating, ID='+IntToStr(ThreadInfo.ID));
        mboxes.Free;
        mNoInferiorsHash.Free;
        vcLog.Flush;
        PostMessage(requestMgr.Handle,WM_SIZE_CHECKER_OVER,ThreadInfo.ID,0);
    end;
end;

{ Returns quota info through the var parameters.
  Function returns:
  0 - Quota retrieved
  1 - Server doesn't support quota
  2 - Server supports quota, but unable to retrieve it
}
function TCheckerThread.getQuota(var currentQuota, maxQuota: Integer): Integer;
var fullReply: TStringList; found: Boolean;
    i, posOpen, posClose :Integer;  storageInfo: String; valueList: TStringList;
    currentQuotaStr, maxQuotaStr: String;
    code: Integer;
    quotaRetreived: Boolean;
begin
    Result:=1;
    quotaRetreived:=false;
    UpdateStatusBar('Getting storage quota',STATUSBAR_MSGS);

    // Synapse retreives capability on login
    if (mIMAPConn.conn.imap.FindCap('QUOTA') <> '') then begin
        Result:=2;
        if mIMAPConn.conn.imap.IMAPCommand('GETQUOTAROOT INBOX')='OK' then begin
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
        end
        else begin
            devLog.Warn('Server says it supports QUOTA, but it doesn''t recognize the command');
        end;
        if not quotaRetreived then Result:=2;
    end
    else begin
        Result:=1;
    end;
end;

{ Handles GetQuota as part of the global size check }
procedure TCheckerThread.DisplayQuotaResultsOnStatusBar;
var percent: Integer; msg: String; getQuotaResult: Integer; currentQuota, maxQuota: Integer;
begin
     getQuotaResult:=GetQuota(currentQuota,maxQuota);
     case getQuotaResult of
     0: begin // Quota retrieved
            percent:=Round(currentQuota/maxQuota*100);
            msg:='Storage Quota: '+IntToStr(currentQuota)+' of '+IntToStr(maxQuota)+' ('+IntToStr(percent)+'%)';
            UpdateStatusBar(msg,STATUSBAR_QUOTA);
        end;
     1: begin
            msg:='Storage Quota: Not supported by server';
            UpdateStatusBar(msg,STATUSBAR_QUOTA);
        end;
     2: begin  // Server supports quota, but couldn't retrieve it
            msg:='Couldn''t retrieve storage quota';
            UpdateStatusBar(msg,STATUSBAR_QUOTA);
        end;
     end;

end;

{ Make the call to the server and place into 'mboxes' the actual
  lines representing mailboxes (removes any server reply not listing a mailbox)
  Also retreives the mailbox separator }
function TCheckerThread.GetMailboxes(var mboxes: TStringList): Boolean;
var fullReply: TStringList; i,p1,p2,num: Integer; fullFolderName, listWildcard, fName, parentName: String;
    gotSeparator, noInferiors: Boolean; re, rePath: TRegExpr; parentNames: TStringList;
    command: String;
begin
    re:=TRegExpr.Create;
    re.Expression:=NUM_LITERAL_RE;

    FMain.mNS:=TNamespace.Create(GetNamespaceString);
    if mIMAPConn.conn.account.RootFolder<>'' then FMain.mNS.SetRootFolder(mIMAPConn.conn.account.RootFolder);
    ParseNamespace(FMain.mNS);

    if FMain.mNS.hasPersonalNS then devLog.Info('Account has personal namespace: YES') else devLog.Info('Account has personal namespace: NO');
    if FMain.mNS.hasSharedNS then devLog.Info('Account has shared namespace: YES') else devLog.Info('Account has shared namespace: NO');
    if FMain.mNS.hasPublicNS then devLog.Info('Account has public namespace: YES') else devLog.Info('Account has public namespace: NO');

    // Get separator
    mSeparator := GetSeparator;
    if mSeparator = ' ' then mSeparator:=FMain.mNS.hierarchySeparator;
    mIMAPConn.conn.account.FolderSeparator:=mSeparator;
    FMain.mSeparator:=mSeparator;   // @todo need to remove FMain.mSeparator now that we have Account.FolderSeparator
    if mCacheEnabled then mCache.setMailboxSeparator(mSeparator);
    devLog.Info('Separator is ['+mSeparator+']');

    mboxes.Clear;
    listWildcard:=FMain.mNS.limitedNamespace;

    if mIMAPConn.conn.imap.IMAPCommand( 'LIST "" "' + listWildcard + '"' )='OK' then begin
        fullReply := mIMAPConn.conn.imap.FullResult;
        for i:=0 to fullReply.Count-1 do begin
            if ( Pos('* LIST (',fullReply[i]) = 1) then begin
                // Check if the folder name is a string literal (as oposed to a quoted string)
                fullReply[i]:=TrimRight(fullReply[i]);
                if re.Exec(fullReply[i]) then begin
                    num := StrToInt(copy(fullReply[i], re.MatchPos[0]+1, re.MatchLen[0]-2));
                    // Get num characters from the next line
                    fullFolderName:=Copy(fullReply[i+1],0,num);
                end
                else fullFolderName:=GetFullFolderName(fullReply[i]);
                // Exclude namespaces from check (these might be huge)  @todo use public namespace here also apart from #
                if ShouldIncludeFolder(fullFolderName,FMain.mNS,mIMAPConn.conn.account.IncludeShared,mIMAPConn.conn.account.IncludePublic,mIMAPConn.conn.account.RootFolder,mIMAPConn.conn.account.ExcludeRoot) then
                    // Some servers return duplicate folders (Hamster 2.0.6.0)
                    if (mboxes.IndexOf(fullFolderName)=-1) then begin
                        // Escape any \ characters
                        fName:=StringReplace(fullFolderName,'\','\\',[rfReplaceAll]);
                        // Add to mboxes
                        mboxes.add(fName);
                    end;
                    // Check NoInferiors
                    noInferiors:=GetNoInferiors(fullReply[i]);
                    mNoInferiorsHash.Insert(fullFolderName,TBoolean.Create(noInferiors));
            end;
        end;

        // Check if INBOX is available. If not, get it (affects UW IMAP)
        if (mboxes.IndexOf('INBOX')=-1) then begin
            devLog.Info('Getting INBOX separately...');
            if mIMAPConn.conn.imap.IMAPCommand( 'LIST "" "INBOX"' )='OK' then begin
                fullReply := mIMAPConn.conn.imap.FullResult;
                for i:=0 to fullReply.Count-1 do begin
                    if ( Pos('* LIST (',fullReply[i]) = 1) then begin
                        fullReply[i]:=TrimRight(fullReply[i]);
                        fullFolderName:=GetFullFolderName(fullReply[i]);
                        fName:=StringReplace(fullFolderName,'\','\\',[rfReplaceAll]);
                        mboxes.add(fName);
                    end;
                end;
                // noInferiors:=Pos('\noinferiors',LowerCase(fullReply))>0;
                noInferiors:=GetNoInferiors(fullReply[0]);
                mNoInferiorsHash.Insert(fullFolderName,TBoolean.Create(noInferiors));
            end;
        end;
        devLog.Debug('Mailboxes found: '+IntToStr(mboxes.Count));
    end
    else begin
        devLog.Error('Server replied negatively to the list command. Can''t list mailboxes.');
    end;
    re.Free;
    Result:=mboxes.Count>0;
end;

{ Extracts the IMAP folder separator }
function TCheckerThread.GetSeparator: Char;
var reply: String; r: char;
begin
    r:=' ';     // Default value (non-defined)
    // LIST "" "" returns the mailbox separator (e.g. S: * LIST (\Noselect) "/" "" )
    if mIMAPConn.conn.imap.IMAPCommand( 'LIST "" ""' )='OK' then begin
        if mIMAPConn.conn.imap.FullResult.Count>0 then begin
            reply:=mIMAPConn.conn.imap.FullResult[0];
            if (Pos('"',reply)>0) then begin
                r := reply[Pos('"',reply)+1];
            end
            else devLog.Warn('Could not extract mailbox separator');
        end;
    end
    else begin
        devLog.Warn('Server did not return the mailbox separator');
    end;
    Result:=r;
end;

function TCheckerThread.GetNamespaceString: String;
var fullReply: TStringList; namespaceResponse: String;
begin
    SetLength(namespaceResponse,0);
    if (mIMAPConn.conn.imap.FindCap('NAMESPACE') <> '') then begin
        devLog.Info('Server supports NAMESPACE');
        if mIMAPConn.conn.imap.IMAPCommand('NAMESPACE')='OK' then begin
            try
                fullReply := mIMAPConn.conn.imap.FullResult;
                namespaceResponse:=fullReply[0];
            except
                devLog.Warn('Error has occurred while parsing the NAMESPACE response. Will ignore namespace info');
            end;
        end
        else begin
            devLog.Warn('Server sais it supports NAMESPACE, but it doesn''t recognize the command');
        end;
    end;
    if (namespaceResponse='') then devLog.Info('Server doesn''t support NAMESPACE');
    Result:=namespaceResponse;
end;

{ Extracts the folder name from a reply to the List command.
  E.g. LIST (\HasNoChildren) "." "INBOX.personal.dad" -> INBOX.personal.dad
  The Mailbox name is quoted if spaces are allowed, otherwise it can appear
  unquoted }
function TCheckerThread.GetFullFolderName(listReplyLine: String): String;
var mbox: String; separator: Char; parts: TStringList;
begin
    mbox:='Unknown';
    if listReplyLine[Length(listReplyLine)] = '"' then separator := '"'
    else separator := ' ';
    parts := TStringList.Create;
    try
        Split(listReplyLine,separator,parts);
        mbox := parts[parts.Count-1];
    finally
        parts.Free;
        Result:=mbox;
    end;
end;

function TCheckerThread.GetParentFolderName(fName: String): String;
var parentName: String; parts: TStringList; i: Integer;
begin
    parentName := '';
    parts := TStringList.Create;
    try
        Split(fName,mSeparator,parts);
        if parts.Count>1 then begin
            for i:=0 to parts.Count-2 do parentName:=parentName+parts[i]+mSeparator;
            parentName:=Copy(parentName,0,Length(parentName)-1);
        end;
    finally
        parts.Free;
        Result:=parentName;
    end;
end;

function TCheckerThread.GetNoInferiors(listReplyLine: String): Boolean;
begin
    Result:=Pos('\noinferiors',LowerCase(listReplyLine))>0;
end;

{
  Returns a list of strings representing a path of the mailbox
  specified in the specified line. E.g. for Cyrus a line might look like this:
  T (\HasNoChildren) "." "INBOX.personal.dad"
  and this function would return the following list:
  INBOX, personal, dad
}
procedure TCheckerThread.GetFoldersFromPath(var path: TStringList; fullFolderName: String; separator: Char);
begin
    path.Clear;
    // Extract all folders from path
    Split(fullFolderName,separator,path);
end;

{ Processes a single UID FETCH * (RFC822.SIZE) reply (also: FETCH * (UID RFC8222.SIZE)
  Returns true if all went fine
  Depending on the server the reply (for the first case) can be:
        FETCH (UID 4 RFC822.SIZE 13933)
        FETCH (RFC822.SIZE 13933 UID 4)
}
function TCheckerThread.ParseUidRfc822Reply(var uid: LongInt; var rfcSize: Integer; line: String): Boolean;
var sizeStr, uidStr: String; p,pSpace,pBracket,pBreak: Integer;
begin
    Result:=false;
    p := Pos('RFC822.SIZE',line);
    if (p>0) then begin
        // Extract size 12 is the size of "RFC822.SIZE "
        sizeStr:=Copy(line,p+12,Length(line)-p-12);
        pSpace:=Pos(' ',sizeStr);
        pBracket:=Pos(' ',sizeStr);
        if pSpace<pBracket then pBreak:=pSpace else pBreak:=pBracket;
        if pBreak>0 then sizeStr:=Copy(sizeStr,1,pBreak-1);

        p:=Pos('UID',line);
        if (p>0) then begin
            uidStr:=Copy(line,p+4,Length(line)-p-4);
            pSpace:=Pos(' ',uidStr);
            pBracket:=Pos(' ',uidStr);
            if pSpace<pBracket then pBreak:=pSpace else pBreak:=pBracket;
            if pBreak>0 then uidStr:=Copy(uidStr,1,pBreak-1);

            try
                rfcSize:=StrToInt(sizeStr);
                uid:=StrToInt(uidStr);
                Result:=true;
            except
                on EConvertError do devLog.Warn('Wrong type of size/uid');
            end;
        end;
    end;
end;

{ Returns the size of the _currently selected_ mailbox.
  Ignores the cache - should be called only when cache is not enabled }
procedure TCheckerThread.GetMailboxSize(var totalMsgSize: Int64; var maxUID: LongInt; remoteMailboxName: String);
var range: String; mockHash: THashTable;
begin
    range := '1:*';
    mockHash:=THashTable.Create;
    GetSizeOfMessages(false,range,totalMsgSize,maxUID,mockHash,false);
    mockHash.Free;
end;

// Will get the mailbox size by using cached information
// Throws ECacheException if an unrecoverable error happens
// Will also update the message cache for the mailbox
procedure TCheckerThread.GetMailboxSizeUsingCache(var totalMsgSize: Int64; var maxUID: LongInt; remoteMailboxName: String;
    realMboxNumMsgs: Integer; realMboxUIDValidity: LongInt);
var cachedFolderInfo: TMasterMboxCacheEntry;
    searchedMsgUIDs: TStringList;
    criteria,range: String;
    cachedMsgs: THashTable;
    i: Integer;
    uid: LongInt;
    cachedMsgPtr: PCachedMessage;

    { Go through the updated cached messages list and calculate size and maxUID }
    procedure CalculateTotalSizeAndMaxUID(updateSize: Boolean; updateMaxUID: Boolean);
    begin
        cachedMsgPtr:=cachedMsgs.First;
        while cachedMsgPtr<>nil do begin
            if updateSize then totalMsgSize:=totalMsgSize + cachedMsgPtr^.Size;
            if updateMaxUID then begin
                uid:=cachedMsgPtr^.UID;
                if uid > maxUID then maxUID := uid;
            end;
            cachedMsgPtr:=cachedMsgs.Next;
        end;
    end;

begin
    // Get folder info. Will create a clean one if the folder is not cached
    cachedFolderInfo:=mCache.GetFolderCacheInfo(remoteMailboxName);
    if cachedFolderInfo.UIDValidity <> realMboxUIDValidity then begin
        // Invalidate cache for this mailbox (clear any existing contents)
        mCache.ClearFolderCache(remoteMailboxName);
        cachedFolderInfo.UIDValidity:=realMboxUIDValidity;
    end;

    // Get uids of messages with a uid higher than the highest cached one
    try
        totalMsgSize:=0; maxUID:=0;
        searchedMsgUIDs := TStringList.Create;
        cachedMsgs:=THashTable.Create;
        criteria := 'UID '+IntToStr(cachedFolderInfo.MaxUID+1)+':*';
        SearchUIDs(criteria,searchedMsgUIDs);
        devLog.Trace('BEFORE CachedFolderInfo.MaxUID='+IntToStr(cachedFolderInfo.MaxUID));
        devLog.Trace('BEFORE CachedFolderInfo.Size='+IntToStr(cachedFolderInfo.Size));
        // n:* will return at least one message if n>highest uid in the mailbox.
        // Make sure the element is removed in this case
        if searchedMsgUIDs.Count=1 then begin
            if (StrToInt(searchedMsgUIDs[0])<=cachedFolderInfo.MaxUID) then searchedMsgUIDs.Clear;
        end;

        if searchedMsgUIDs.Count>0 then begin
            // there are some new messages
            mCache.LoadCachedFolderMsgs(remoteMailboxName,cachedMsgs); // get the message cache

            //mCache.DumpContents(remoteMailboxName); //@rem

            if (searchedMsgUIDs.Count + cachedFolderInfo.Exists = realMboxNumMsgs) then begin
                devLog.Debug('Cache - New messages found, no deletion detected');
                // Only new messages were added, there was no deletion of messages
                range:=IntToStr(cachedFolderInfo.maxUID+1) + ':*';
                GetSizeOfMessages(true,range,totalMsgSize,maxUID,cachedMsgs,true);
                totalMsgSize:=totalMsgSize+cachedFolderInfo.Size;
                // Both totalMsgSize and maxUID are now set to the correct value
                //CalculateTotalSizeAndMaxUID(false);
            end
            else begin
                devLog.Debug('Cache - New messages found, deletion detected');
                // There are new messages and some have been deleted
                // First remove any deleted messages from the cache
                criteria := 'UID 1:*';
                SearchUIDs(criteria,searchedMsgUIDs);
                RemoveDeletedMsgsFromCache(cachedMsgs, searchedMsgUIDs);
                // Get the updated size & maxUID
                CalculateTotalSizeAndMaxUID(true, true);
                // Then add the new messages
                range:=IntToStr(cachedFolderInfo.maxUID+1) + ':*';
                GetSizeOfMessages(true,range,totalMsgSize,maxUID,cachedMsgs,true);
            end;
            //mCache.DumpContents(remoteMailboxName); //@rem
            mCache.SaveCachedFolderMsgs(remoteMailboxName,cachedMsgs);
        end
        else begin
            // there are no new messages
            if cachedFolderInfo.Exists = realMboxNumMsgs then begin
                devLog.Debug('Cache - No new messages, no deletion detected');
                // Real mailbox state is same as in the cache
                totalMsgSize:=cachedFolderInfo.Size;
                maxUID:=cachedFolderInfo.maxUID;
                //CalculateTotalSizeAndMaxUID(false);
                //mCache.DumpContents(remoteMailboxName); //@rem
            end
            else begin
                devLog.Debug('Cache - No new messages, deletion detected');
                //mCache.DumpContents(remoteMailboxName); //@rem
                // No new messages, but some might have been deleted
                criteria := 'UID 1:*';
                SearchUIDs(criteria,searchedMsgUIDs);

                // remove from cache any messages that are not in searchMsgUIDs
                mCache.LoadCachedFolderMsgs(remoteMailboxName,cachedMsgs);
                RemoveDeletedMsgsFromCache(cachedMsgs, searchedMsgUIDs);

                maxUID:=cachedFolderInfo.maxUID;
                CalculateTotalSizeAndMaxUID(true,false);
                // Save the cached messages
                //mCache.DumpContents(remoteMailboxName); //@rem
                mCache.SaveCachedFolderMsgs(remoteMailboxName,cachedMsgs);
            end;
        end;
        devLog.Trace('AFTER CachedFolderInfo.MaxUID='+IntToStr(cachedFolderInfo.MaxUID));
        devLog.Trace('AFTER CachedFolderInfo.Size='+IntToStr(cachedFolderInfo.Size));
    finally
        searchedMsgUIDs.Free;
        mCache.FreeCachedMessages(cachedMsgs);
    end;
end;

{ Gets the size of messages in the specified range.
  If the cachedMsgs parameter is defined (not nil), message info will be added to the cache
  @param uidRange true if the range is a uid one, false if it is a message sequence range
  @param msgRange a string representing a rfc3501 range (e.g. 1:*, 134:345, etc)
  @param totalMsgSize the total size of these messages
  @param maxUID the highest uid of messages in the range
}
procedure TCheckerThread.GetSizeOfMessages(
    uidRange: Boolean; msgRange: String; var totalMsgSize: Int64;
    var maxUID: LongInt; var cachedMsgs: THashTable; updateCache: Boolean);
var command: String; fullReply: TStringList; i, size: Integer; uid: LongInt;
    cachedMessagePtr: PCachedMessage;
begin
    // totalMsgSize := 0;
    // maxUID := 0;
    // mIMAPConn.conn.imap.UID := uidRange;
    if uidRange then command:='UID ' else command:='';
    command:=command+'FETCH '+msgRange+' (UID RFC822.SIZE)';
    if mIMAPConn.conn.imap.IMAPCommand(command) = 'OK' then begin
        fullReply := mIMAPConn.conn.imap.FullResult;
        for i:=0 to fullReply.Count-1 do begin
            if ParseUidRfc822Reply(uid,size,fullReply[i]) then begin
                totalMsgSize := totalMsgSize + size;
                if uid>maxUID then maxUID:=uid;
                if updateCache then begin
                    // Add message to cache if not already there
                    if cachedMsgs[uid]=nil then begin
                        new(cachedMessagePtr);
                        cachedMessagePtr^.FullInfo:=false;  // we only have the uid and size
                        cachedMessagePtr^.UID:=uid;
                        cachedMessagePtr^.Size:=size;
                        cachedMsgs[uid]:=cachedMessagePtr;
                    end;
                end;
            end;
        end;
    end
    else begin
        // do nothing, will return NO if there is no messages in the mailbox...
    end;
    mIMAPConn.conn.imap.UID:=false; // revert to the default value
end;

{ Gets a list of uids meeting the UID SEARCH UID criteria
  in the currently SELECTED folder.
  Clears res before getting new results
  @param res UIDs that conform to the search criteria
  @throws ECacheException if anything goes wrong}
procedure TCheckerThread.SearchUIDs(criteria: String; var res: TStringList);
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

{ Removes any messages that do not exist in the 'existingMsgUIDs' from the
  cachedMsgs list }
procedure TCheckerThread.RemoveDeletedMsgsFromCache(var cachedMsgs: THashTable; const existingMsgUIDs: TStringList);
var strTable: TStringTable; // http://www.cs.utu.fi/tjohtela/software.htm#PCL
    i: Integer;
    uid: LongInt;
    cachedMsgPtr: PCachedMessage;
    dummyObj: TObject;
begin
    try
        strTable:=TStringTable.Create;
        dummyObj:=TObject.Create;
        for i:=0 to existingMsgUIDs.Count-1 do begin
            strTable.Items[existingMsgUIDs[i]]:=dummyObj;
        end;

        cachedMsgPtr:=cachedMsgs.First;
        while cachedMsgPtr<>nil do begin
            uid:=cachedMsgPtr^.UID;
            if strTable.Items[IntToStr(uid)] = nil then begin
                // this uid doesn't exist anymore in the mailbox. remove from cache
                cachedMsgPtr:=cachedMsgs.Remove(uid);
                Dispose(cachedMsgPtr);
            end;
            cachedMsgPtr:=cachedMsgs.Next;
        end;
    finally
        dummyObj.Free;
        strTable.Free;  //@todo need to free individual items?
    end;
end;

procedure TCheckerThread.AddVirtualRootNode(var stack: TNodeStack);
begin
    mboxTree.EnterCS;
    mboxTree.Items[0].mNodeName:='Account: '+mIMAPConn.conn.account.Name;
    mboxTree.Items[0].mLevel:=0;
    mboxTree.Items[0].mAbsoluteIndex:=0;
    mboxTree.Items[0].mNumMessages:=0;
    mboxTree.Items[0].mTotalNumMessages:=0;
    mboxTree.Items[0].mSize:=0;
    mboxTree.Items[0].mSizeDisplay:='0';
    mboxTree.Items[0].mTotalSize:=0;
    mboxTree.Items[0].mSizeDisplay:='0';
    mboxTree.Items[0].mPercentOfParent:=0;
    mboxTree.Items[0].mFullDisplayedName:='Account: '+mIMAPConn.conn.account.Name;
    mboxTree.Items[0].mFullMailboxName:='';
    mboxTree.Items[0].mIsLeaf:=true;
    mboxTree.Items[0].mNumOfChildren:=0;
    mboxTree.Items[0].mImageIndex:=3;
    mboxTree.Items[0].mParentIndex:= -1;
    mboxTree.Items[0].mExpanded:=true;
    mboxTree.Items[0].mVirtual:=true;
    mboxTree.Items[0].mNoInferiors:=false;
    stack.Push(mboxTree.Items[0]);
    mboxTree.LeaveCS;
end;

{ checkSize is true if sizes should be checked. If it is false,
  only the hierarchy is retreived. }
procedure TCheckerThread.DoCalculations(checkSize: Boolean;
                                        var stack: TNodeStack; var folders: TStringList;
                                        var parentNodeObj: TNodeRecord;
                                        var NodeLevel, prevLevel, tempLevel, absIndexInTree: Integer;
                                        nodeName: String;
                                        mboxCnt: Integer);
var lastOne: Integer;
    fullFolderName, remoteMailboxName: String;
    personalNamespaceLength: Integer;
    masterCacheEntry: TMasterMboxCacheEntry;
    rootFolderLength: Integer;
begin
    folders.Clear;
    fullFolderName := mboxes[mboxCnt];
    remoteMailboxName := StringReplace(fullFolderName,'\','\\',[rfReplaceAll]);
    if (FMain.mNS.hasPersonalNS) and (mIMAPConn.conn.account.SmartInbox) then begin
        personalNamespaceLength:=Length(FMain.mNS.personalNSPrefix);
        if (mboxes[mboxCnt]=(FMain.mNS.personalNSPrefix+'Inbox')) then begin
            remoteMailboxName := Copy(FMain.mNS.personalNSPrefix,1,personalNamespaceLength-1);
        end;
        // else remoteMailboxName := fullFolderName;
        // Remove 'INBOX.' prefix
        fullFolderName:=Copy(mboxes[mboxCnt],personalNamespaceLength+1,Length(mboxes[mboxCnt])-personalNamespaceLength);
    end;

    // If the root folder is defined and Smart Inbox is turned on, remove the root folder string from the mailbox
    // This is the case with UW IMAP servers (mailfiles example)
    if (mIMAPConn.conn.account.RootFolder<>'') and (mIMAPConn.conn.account.ExcludeRoot) then begin
        devLog.Trace(IntToStr(Pos(mboxes[mboxCnt],mIMAPConn.conn.account.RootFolder)));
        if Pos(mIMAPConn.conn.account.RootFolder,mboxes[mboxCnt])=1 then begin
            devLog.Trace('Applying RootFolder+SmartInbox rule - Removing RootFolder from mailbox name');
            rootFolderLength:=Length(mIMAPConn.conn.account.RootFolder);
            fullFolderName:=Copy(mboxes[mboxCnt],rootFolderLength+1,Length(mboxes[mboxCnt])-rootFolderLength);
        end;
    end;

    devLog.Debug('Fullfoldername: '+fullFolderName+ chr(9) + 'RemoteMailbox:  '+remoteMailboxName);
    GetFoldersFromPath(folders,fullFolderName,mSeparator);
    if (checkSize) then begin
        UpdateStatusBar('Processing: '+remoteMailboxName,STATUSBAR_MSGS);
    end;
    // Process only the last folder in the path
    lastOne:=folders.Count-1;

    mboxTree.EnterCS;
    try
        // Mark INBOX.Inbox as virtual if it exists
        if ((mVirtualINBOXInboxAdded) and (fullFolderName=FMain.mNS.personalNSPrefix+'Inbox')) then
            mboxTree.Items[mboxTree.Count].mVirtual:=true
        else
            mboxTree.Items[mboxTree.Count].mVirtual:=false;
        mboxTree.Items[mboxTree.Count].mNodeName:=folders[lastOne];
        mboxTree.Items[mboxTree.Count].mLevel:=lastOne+1;  // +1 due to the virtual root node
        mboxTree.Items[mboxTree.Count].mAbsoluteIndex:=absIndexInTree;
        mboxTree.Items[mboxTree.Count].mTotalNumMessages:=0;
        mboxTree.Items[mboxTree.Count].mSizeDisplay:='';
        mboxTree.Items[mboxTree.Count].mTotalSizeDisplay:='';
        if not mboxTree.Items[mboxTree.Count].mVirtual then begin
            try
                mboxTree.Items[mboxTree.Count].mNoInferiors:=(mNoInferiorsHash.Items[remoteMailboxName] as TBoolean).val
            except
                devLog.Error('Could not find key in mNoInferiorsHash that should have been there: '+remoteMailboxName);
                mboxTree.Items[mboxTree.Count].mNoInferiors:=false;
            end;
        end
        else
            mboxTree.Items[mboxTree.Count].mNoInferiors:=false;


        if (checkSize) then begin
            mIMAPConn.conn.imap.SelectFolder(remoteMailboxName);  //@todo if OK
            mboxTree.Items[mboxTree.Count].mNumMessages:=mIMAPConn.conn.imap.SelectedCount;
            mboxTree.Items[mboxTree.Count].mUIDValidity:=mIMAPConn.conn.imap.SelectedUIDvalidity;
            if mCacheEnabled then begin

                devLog.Trace('Cache enabled');
                try
                    GetMailboxSizeUsingCache(mboxTree.Items[mboxTree.Count].mSize,mboxTree.Items[mboxTree.Count].mMaxUID,remoteMailboxName,
                        mboxTree.Items[mboxTree.Count].mNumMessages,mboxTree.Items[mboxTree.Count].mUIDValidity);
                except
                    devLog.Warn('Problem occurred getting mailbox size using cache. Will clear the cache and get all info from the server');
                    mCache.ClearAccountCache(mIMAPConn.conn.account.Name);
                    GetMailboxSize(mboxTree.Items[mboxTree.Count].mSize,mboxTree.Items[mboxTree.Count].mMaxUID,remoteMailboxName);
                end;
            end
            else begin
                devLog.Debug('Caching disabled. Getting all info from the server');
                GetMailboxSize(mboxTree.Items[mboxTree.Count].mSize,mboxTree.Items[mboxTree.Count].mMaxUID,remoteMailboxName);
            end;

            if mCacheEnabled then begin
                masterCacheEntry.RealMailboxName:=mboxTree.Items[mboxTree.Count].mFullMailboxName;
                masterCacheEntry.LastUpdated:=Now;
                masterCacheEntry.UIDValidity:=mboxTree.Items[mboxTree.Count].mUIDValidity;
                masterCacheEntry.MaxUID:=mboxTree.Items[mboxTree.Count].mMaxUID;
                masterCacheEntry.Size:=mboxTree.Items[mboxTree.Count].mSize;
                masterCacheEntry.Exists:=mboxTree.Items[mboxTree.Count].mNumMessages;
                mCache.updateFolderCacheInfo(masterCacheEntry);
            end;
        end
        else begin
            mboxTree.Items[mboxTree.Count].mNumMessages:=0;
            mboxTree.Items[mboxTree.Count].mSize:=0;
        end;
        mboxTree.Items[mboxTree.Count].mTotalSize:=0;

        NodeLevel:=mboxTree.Items[mboxTree.Count].mLevel;

        if NodeLevel>prevLevel then begin
            // This node is a child of the previous one
            if NodeLevel = 1 then
                mboxTree.Items[mboxTree.Count].mFullDisplayedName:=mboxTree.Items[mboxTree.Count].mNodeName
            else begin
                stack.Peek(parentNodeObj);
                mboxTree.Items[mboxTree.Count].mFullDisplayedName:=parentNodeObj.mFullDisplayedName + mSeparator + mboxTree.Items[mboxTree.Count].mNodeName;
            end;
            mboxTree.Items[mboxTree.Count].mIsLeaf:=true;
            mboxTree.Items[mboxTree.Count-1].mIsLeaf:=false;   // the previous node has children
            mboxTree.Items[mboxTree.Count].mExpanded:=false;
            stack.Push(mboxTree.Items[mboxTree.Count]);
        end
        else if NodeLevel = prevLevel then begin
            // New node is at the same level as the previous one
            if NodeLevel = 1 then
                mboxTree.Items[mboxTree.Count].mFullDisplayedName:=mboxTree.Items[mboxTree.Count].mNodeName
            else begin
                stack.GetParentNode(parentNodeObj, mboxTree.Items[mboxTree.Count].mLevel);
                mboxTree.Items[mboxTree.Count].mFullDisplayedName:=parentNodeObj.mFullDisplayedName + mSeparator + mboxTree.Items[mboxTree.Count].mNodeName;
            end;

            mboxTree.Items[mboxTree.Count].mIsLeaf:=false;
            mboxTree.Items[mboxTree.Count-1].mIsLeaf:=true;
            mboxTree.Items[mboxTree.Count].mExpanded:=false;
            stack.Push(mboxTree.Items[mboxTree.Count]);
        end
        else begin
            // New node is at a lower level than the previos one

            // Get sum of sizes of nodes of level prevLevel
            // Calculate their total and add to parent
            // Calculate percantage of children of level prevLevel
            // Clear all to prevLevel-1 (to the parent)
            // repeat this until the parent is these kids is of NodeLevel
            tempLevel:=prevLevel;
            repeat
                tempLevel:=stack.GetNewTempLevel(mboxTree.Items, tempLevel)
            until (tempLevel=NodeLevel) or (terminated);

            // then process the current node

            if NodeLevel = 1 then
                mboxTree.Items[mboxTree.Count].mFullDisplayedName:=mboxTree.Items[mboxTree.Count].mNodeName
            else begin
                stack.GetParentNode(parentNodeObj, mboxTree.Items[mboxTree.Count].mLevel);
                mboxTree.Items[mboxTree.Count].mFullDisplayedName:=parentNodeObj.mFullDisplayedName + mSeparator + mboxTree.Items[mboxTree.Count].mNodeName;
            end;

            mboxTree.Items[mboxTree.Count].mIsLeaf:=true;
            mboxTree.Items[mboxTree.Count-1].mIsLeaf:=true;
            mboxTree.Items[mboxTree.Count].mExpanded:=false;
            stack.Push(mboxTree.Items[mboxTree.Count]);
        end;
        mboxTree.Items[mboxTree.Count].mFullMailboxName:=remoteMailboxName;

        // Prepare data for display if should
        if settings.TreeDisplay.UpdateAsSizeCheck then begin
            mboxTree.Items[mboxTree.Count].mSizeDisplay:=formatSizeDisplay(mboxTree.Items[mboxTree.Count].mSize);
            mboxTree.Items[mboxTree.Count].mTotalSizeDisplay:=formatSizeDisplay(mboxTree.Items[mboxTree.Count].mTotalSize);
        end;

        mboxTree.Count := mboxTree.Count +1;
        if (mboxTree.Count=mboxTree.Size) then begin
            mboxTree.Size:=mboxTree.Size+MBOX_TREE_SIZE_INCREMENT;
            SetLength(mboxTree.Items,mboxTree.Size);
        end;
        prevLevel:=NodeLevel;
        absIndexInTree:=absIndexInTree+1;
    finally
        mboxTree.leaveCS;
    end;

end;

function TCheckerThread.CreateMailboxHierarchy: Boolean;
var
      nodeName: String;
      absIndexInTree: Integer;
      stack: TnodeStack;
      NodeLevel, prevLevel, tempLevel: Integer;
      parentNodeObj: TNodeRecord;
      folders: TStringList;
      mboxCnt,i, inboxIndex: Integer;
      prefixWithoutSeparator: String;
      found, completed: Boolean;
begin
    completed:=false;
    stack:=TNodeStack.Create;
    folders:= TStringList.create;
    try
        try
            mboxTree.EnterCS;
            mboxTree.Count:=0;
            mboxTree.Size:=MBOX_TREE_SIZE_INCREMENT;
            SetLength(mboxTree.Items,0);
            SetLength(mboxTree.Items,mboxTree.Size);

            NodeLevel:=0; prevLevel:=0;
            absIndexInTree:=0;

            if mIMAPConn.IsConnected then begin
                mboxes.Clear;
                Synchronize(FMain.ClearTreeList);
                UpdateStatusBar('Retrieving mailboxes',STATUSBAR_MSGS);

                if GetMailboxes(mboxes) then begin
                    found:=false;
                    // Fix root mailbox name (usually INBOX -> INBOX.Inbox)
                    if (FMain.mNS.hasPersonalNS) then begin
                        prefixWithoutSeparator:=Copy(FMain.mNS.personalNSPrefix,1,Length(FMain.mNS.personalNSPrefix)-1);
                        inboxIndex:=0; // satisfy compiler
                        for i:=0 to mboxes.count-1 do begin
                            if mboxes[i]=prefixWithoutSeparator then begin
                                inboxIndex:=i;
                                found := true;
                                break;
                            end;
                        end;
                        if found then
                            if FMain.mNS.hasPersonalNS and mIMAPConn.conn.account.SmartInbox then begin
                                if (mboxes.indexof(FMain.mNS.personalNSPrefix+'Inbox')=-1) then
                                    mboxes[inboxIndex]:=FMain.mNS.personalNSPrefix+'Inbox'
                                else begin
                                    // Item already exists, remove it from the list
                                    mboxes.Delete(inboxIndex);
                                end;
                            end;
                    end;

                    if FMain.mNS.hasPersonalNS then begin
                        if mIMAPConn.conn.account.SmartInbox then begin
                            devLog.Info('Applying smart inbox');
                        end
                        else begin
                            // Check if there are any nodes of type INBOX.Inbox.xxx
                            // If yes, add a virtual node INBOX.Inbox
                            // This is necessary because the CreateMailboxHierarchy would add these
                            // nodes to a parent which is not necessarily INBOX
                            found:=false;
                            if FMain.mNS.personalNSPrefix<>'' then begin
                                for i:=0 to mboxes.count-1 do begin
                                    if Pos(FMain.mNS.personalNSPrefix+'Inbox',mboxes[i])>0 then begin
                                        found:=true;
                                        break;
                                    end;
                                end;
                            end;
                            if found then begin
                                if (mboxes.indexof(FMain.mNS.personalNSPrefix+'Inbox')=-1) then begin
                                    devLog.Debug('Adding virtual node: '+FMain.mNS.personalNSPrefix+'Inbox');
                                    // only add if not already there
                                    mboxes.Add(FMain.mNS.personalNSPrefix+'Inbox');
                                    mVirtualINBOXInboxAdded := true;
                                end;
                            end;
                        end;

                    end;

                    mboxes.Sort;
                    devLog.Debug('Sorted:');
                    for i:=0 to mboxes.Count-1 do devLog.Debug(mboxes[i]);

                    addVirtualRootNode(stack);
                    mboxTree.Count:=1;
                    absIndexInTree:=absIndexInTree+1;
                    NodeLevel:=1; prevLevel:=0;

                    devLog.Debug('Retreived list of mailboxes. Creating hierarchy');
                    // Parse each mbox and add to the tree model
                    for mboxCnt:=0 to mboxes.Count-1 do begin
                        if (not terminated) then begin
                            DoCalculations(false, stack, folders, parentNodeObj, NodeLevel,prevLevel, tempLevel,absIndexInTree,nodeName, mboxCnt);
                        end;
                    end;
                end
                else begin
                    raise Exception.Create('Couldn''t retrieve mailboxes. Try increasing the Server Timeout in Options/Misc');
                end;
            end
            else begin
                raise Exception.Create('Internal error - Not connected to imap server');
            end;

            mboxTree.Items[mboxTree.Count-1].mIsLeaf:=true;  // last node is a leaf
            // calculate totals for nodes remaining on stack
            tempLevel:=NodeLevel;
            repeat
                tempLevel:=stack.GetNewTempLevel(mboxTree.Items, tempLevel);
            until (tempLevel=0) or (terminated);

            // Expand INBOX (namespace) node if exists and smart inbox is not turned on
            if (FMain.mNS.hasPersonalNS) and (not mIMAPConn.conn.account.SmartInbox) and (mboxTree.Count>1) then
                mboxTree.Items[1].mExpanded:=true;

            completed:=true;

        except
           on E: Exception do begin
              UpdateProgressBar(0);
              FMain.ACancelAction.Enabled:=false;
              SyncDisplayMessage('Unable to check account. '+E.Message);

           end;
        end;
    finally
        UpdateStatusBar('',STATUSBAR_MSGS);
        folders.Free;
        stack.Free;
        mboxTree.LeaveCS;
        Result:=completed;
    end;
end;

{ @params progressBarStartPos The position of ProgressBar1 when entering this task
  @params progressBarEndPos The position of ProgressBar1 when exiting this task  }
procedure TCheckerThread.CheckAccountSize(progressBarStartPos, progressBarEndPos: Integer);
var
      nodeName: String;
      absIndexInTree: Integer;
      stack: TnodeStack;
      NodeLevel, prevLevel, tempLevel: Integer;
      parentNodeObj: TNodeRecord;

      folders: TStringList;            // Folders parsed from one line
      mboxCnt: Integer;
      progressBarIncrement: Real;
      progressBarRealPosition: Real;
begin
    devLog.Debug('Checking sizes');
    stack:=TNodeStack.Create;
    folders:= TStringList.create;
    try
        try
            mboxTree.Count:=0;
            mboxTree.Size:=MBOX_TREE_SIZE_INCREMENT;
            SetLength(mboxTree.Items,mboxTree.Size);

            NodeLevel:=0; prevLevel:=0;
            absIndexInTree:=0;

            devLog.Debug('Mailbox count: '+IntToStr(mboxes.Count));

            progressBarIncrement:=0;
            if (mboxes.Count > 0) then
                progressBarIncrement := (progressBarEndPos-progressBarStartPos)/mboxes.Count;

            progressBarRealPosition := progressBarStartPos;
            addVirtualRootNode(stack);
            mboxTree.Count:=1;
            absIndexInTree:=absIndexInTree+1;

            // Parse each mbox and add to the tree model
            for mboxCnt:=0 to mboxes.Count-1 do begin
                if (not terminated) then begin
                    DoCalculations(true, stack, folders, parentNodeObj, NodeLevel,prevLevel, tempLevel,absIndexInTree,nodeName,mboxCnt);
                    progressBarRealPosition := progressBarRealPosition + progressBarIncrement;
                    //UpdateProgressBar(Trunc(FMain.ProgressBar1.Position + progressBarIncrement));
                    UpdateProgressBar(Trunc(progressBarRealPosition));
                    if settings.TreeDisplay.UpdateAsSizeCheck then
                        PostMessage(FMain.Handle, WM_UPDATE_TREE_SIZE_CALC_NO_IMG, ThreadInfo.ID, 0);
                end;
            end;
            mboxTree.Items[mboxTree.Count-1].mIsLeaf:=true;  // last node is a leaf
            // calculate totals for nodes remaining on stack
            tempLevel:=NodeLevel;
            repeat
                tempLevel:=stack.GetNewTempLevel(mboxTree.Items, tempLevel);
            until tempLevel=0;

            UpdateProgressBar(progressBarEndPos);
            if settings.TreeDisplay.UpdateAsSizeCheck then
                PostMessage(FMain.Handle, WM_UPDATE_TREE_SIZE_CALC_NO_IMG, ThreadInfo.ID, 0);

        except
           on E: Exception do begin
              UpdateProgressBar(0);
              if not terminated then SyncDisplayMessage('An unexpected error has occurred. Unable to retrieve mailbox sizes.');
           end;
        end;
    finally
        folders.Free;
        stack.Free;
    end;
end;

{ Sends a message to update the main ProgressBar in the ActivityDlg/FMain }
procedure TCheckerThread.UpdateProgressBar(newPosition: Integer);
begin
    PostMessage(requestMgr.Handle, WM_PROGRESS_BAR_UPDATE, ThreadInfo.requestID, newPosition);
end;

procedure TCheckerThread.UpdateStatusBar(newText: String; panelIndex: Integer);
begin
    mStatusBarText:=newText;
    mStatusBarPanelIndex:=panelIndex;
    Synchronize(InternalUpdateStatusBar);
end;

procedure TCheckerThread.InternalUpdateStatusBar;
begin
    FMain.ShowMessageOnStatusBar(mStatusBarText, mStatusBarPanelIndex);
    FMain.StatusBar1.Repaint;
end;

procedure TCheckerThread.SyncDisplayMessage(msg: String);
begin
    mMessageText:=msg;
    Synchronize(InternalDisplayMessage);
end;

procedure TCheckerThread.InternalDisplayMessage;
begin
    MessageDlg(mMessageText,mtInformation,[mbOK],0);
end;

{ Performs a full update of tree contents with all
  necessary calculations }
procedure TCheckerThread.FullTreeUpdate;
begin
    FMain.RecalculateSizesAndDisplay(true);
end;

destructor TCheckerThread.Destroy;
begin
    //FMain.MarkCheckerThreadStopped;
    Inherited Destroy;
end;

{ This method returns true if the folder should be included in the retrieval }
function TCheckerThread.ShouldIncludeFolder(folderName: String; ns: TNamespace; includeShared, includePublic: Boolean; rootFolder: String; excludeRoot: Boolean): Boolean;
var include: Boolean; i: Integer; personalPrefixWithoutSeparator: String;

    function isSharedFolder: Boolean;
    begin
        if ns.hasSharedNS then
            Result:= Copy(folderName,0,Length(ns.sharedNSPrefix)) = ns.sharedNSPrefix
        else
            Result:=false;
    end;

    function isPublicFolder: Boolean;
    var found: Boolean;
    begin
        found:=false;
        if ns.hasPublicNS then begin
            // Check all public namespaces
            i:=0;
            while not found and (i<ns.publicNSPrefixes.count) do begin
                if (Copy(folderName,0,Length(ns.publicNSPrefixes[i])) = ns.publicNSPrefixes[i]) then found:=true;
                Inc(i);
            end;
        end;
        Result:=found;
    end;

begin
    include:=false;
    if ns.namespaceExists then begin
        // First check if foldername belongs to the personal namespace
        // Have to check the personal prefix without the separator
        personalPrefixWithoutSeparator := Copy(ns.personalNSPrefix,0,Length(ns.personalNSPrefix)-1);
        if (Copy(folderName,0,Length(personalPrefixWithoutSeparator)) = personalPrefixWithoutSeparator) then begin
            if ns.hasPersonalNS then include:=true
            else begin
                // If the personal namespace doesn't exist, we have to check if this folder is a shared or public one
                if not isSharedFolder and not isPublicFolder then include:=true
                else if isSharedFolder and includeShared then include:=true
                else if isPublicFolder and includePublic then include:=true;
            end;
        end
        else if includeShared and ns.hasSharedNS and isSharedFolder then
            include:=true
        else if includePublic and ns.hasPublicNS and isPublicFolder then begin
            include:=true;
        end;
    end
    else begin
        // If the namespaces are not defined, all folders should be included
        include:=true;
    end;
    // In case RootFolder is defined and ExcludeRoot checked, exclude the RootFolder folder
    if (rootFolder<>'') and excludeRoot then begin
        include := folderName <> rootFolder;
    end;

    Result:=include;
end;


end.
