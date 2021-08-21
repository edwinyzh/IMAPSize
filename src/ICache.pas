{==============================================================================|
| Project : IMAPSize                                                           |
|==============================================================================|
| $Id: ICache.pas,v 1.3 2004/03/31 23:27:37 Ivan Exp $
|==============================================================================|
| Copyright 2003-2004 Ivan Vecanski                                            |
| =============================================================================}

unit ICache;

{ Unit that manages the mailbox and msgInfo cache. }

interface

uses Log, VecLog, Classes, sysutils, FileCtrl, MessageLists, HashTables, MyUtil, Windows, GlobalConstants, RegExpr;

const
    MASTER_INDEX_FILENAME = 'master.idx';
    FOLDER_SEPARATOR = '#!#';

type

    ECacheException = class(Exception);

    { Record representing an entry in the master index file of the cache }
    TMasterMboxCacheEntry = packed record
        RealMailboxName: String[120];
        LastUpdated: TDateTime;
        UIDValidity: LongInt;
        MaxUID: LongInt;   // biggest cached uid in the mailbox
        Size: Integer;    // size of mailbox (bytes)
        Exists: Integer;  // cached number of messages
    end;

    PMasterMboxCacheEntry = ^TMasterMboxCacheEntry;
    TMasterMboxCacheFile = File of TMasterMboxCacheEntry;

    { Record representing a single message in the mailbox cache.
      Persistant version of MessageLists.TMessageInfo
      This record contains full information only if the messages have
      actually been retrieved...
    }
    TCachedMessage = packed record
        FullInfo: Boolean;  // true if the record contains full msg info (contains only uid and size)
        UID: LongInt;
        Size: Integer;
        Date: TDateTime;
        Subject: String[120];
        MsgFrom: String[120];
        MsgTo: String[120];
        Flags: TMessageFlags;
        SpamScore: String[20];
        IsSpam: Boolean;
        HasAttachments: Boolean;
    end;

    PCachedMessage = ^TCachedMessage;
    TCachedMessageFile = File of TCachedMessage;

    TICache = class
      private
        cs: TRTLCriticalSection;
        mAppFolder: String;    // Root folder for the cache (appfolder/cache)
        mCurrentAccount: String; // Name of the account whose cache is currently in memory
        mMailboxSeparator: String; // Separator for the current account
        mFolders: TList;         // List of TMasterMboxCacheEntry records
        mMasterCacheNeedsSaving: Boolean; // True if the master cache needs saving
        invPathRegEx: TRegExpr;
        function GetFolderCacheInfoIndex(folderName: String): Integer;
        procedure CopyMasterEntry(var fromEntry, toEntry: TMasterMboxCacheEntry);
        function GetAccountCacheFolder: String; // Returns the folder where the cache files for the current account are stored
        procedure FreeListItems;
        function GetMailboxCacheFilename(realMboxName: String): String;
      public
        constructor Create(logDir: String);
        destructor Destroy; override;
        procedure SetMailboxSeparator(mboxSeparator: String);
        function LoadMasterCache(account: String): Boolean; // Loads the master cache for the specified account. False if cache doesn't exist
        procedure SaveMasterCache;                          // Saves the cache for the current account
        procedure RenameAccount(oldName, newName: String); // Renames account (will delete old phisical folder)
        function GetFolderCacheInfo(folderName: String): TMasterMboxCacheEntry; // Get cached info of the specified folder
        procedure UpdateFolderCacheInfo(folderCachedInfo: TMasterMboxCacheEntry); // Updates (adds if not there) folder info
        function ClearFolderCache(folderName: String): Boolean;  // Clears the cache of the specific folder (used to invalidate the folder cache)
        function ClearAccountCache(account: String): Boolean;    // Clears the cache for the specified account
        function ClearCache: Boolean;  // Clears the whole cache

        procedure LoadCachedFolderMsgs(folderName: String; var cachedMsgs: THashTable);
        procedure SaveCachedFolderMsgs(folderName: String; cachedMsgs: THashTable);
        procedure FreeCachedMessages(var cachedMsgs: THashTable);

        procedure DumpContents(folderName: String);
    end;


function MessageInfoToCachedMessage(const msgInfo: TMessageInfo): TCachedMessage;
function CachedMesageToMessageInfo(const cachedMsg: TCachedMessage): TMessageInfo;

implementation

uses Main, MailboxTree;

constructor TICache.Create(logDir: String);
begin
    Inherited Create;
    mAppFolder:=logDir;
    mMasterCacheNeedsSaving := false;
    mFolders := TList.Create;
    InitializeCriticalSection(cs);

    // Regex for replacing invalid filename characters (eg Courier allows : chars in folder names)
    invPathRegEx:=TRegExpr.Create;
    invPathRegEx.Expression:=invPathRE;
end;

destructor TICache.Destroy;
begin
    invPathRegEx.Free;
    DeleteCriticalSection(cs);
    if mMasterCacheNeedsSaving then saveMasterCache;
    FreeListItems;
    mFolders.Free;
    mFolders := nil;
    Inherited Destroy;
end;

procedure TICache.SetMailboxSeparator(mboxSeparator: String);
begin
    mMailboxSeparator:=mboxSeparator;
end;

{ Loads the master cache for the specified account.
  False if cache doesn't exist }
function TICache.LoadMasterCache(account: String): Boolean;
var filename: String; masterEntryPtr: PMasterMboxCacheEntry;
    mMasterFile: TMasterMboxCacheFile;
begin
    Result:=false; // default
    if account=mCurrentAccount then begin
        // already in memory
        Result:=true;
    end
    else begin
        mCurrentAccount:=account;
        // Clear current cache
        mFolders.Clear;

        // Check if file exists
        filename := getAccountCacheFolder+'\'+MASTER_INDEX_FILENAME;
        if FileExists(filename) then begin
            EnterCriticalSection(cs);
            // Open file for reading
            AssignFile(mMasterFile, filename);
            Reset(mMasterFile);
            try
                while not Eof(mMasterFile) do begin
                    new (masterEntryPtr);
                    Read(mMasterFile, masterEntryPtr^);
                    mFolders.Add(masterEntryPtr);
                end;
                mMasterCacheNeedsSaving:=false;
                Result:=true;
            finally
                Close(mMasterFile);
                LeaveCriticalSection(cs);
            end;
        end;
        // else Result:=false
    end;
end;

{ Saves the cache for the current account }
procedure TICache.SaveMasterCache;
var filename: String; i: Integer;
    masterEntryPtr: PMasterMboxCacheEntry;
    masterEntry: TMasterMboxCacheEntry;
    mMasterFile: TMasterMboxCacheFile;
begin
    // Make sure the folder exists...
    if not DirectoryExists(getAccountCacheFolder) then begin
        ForceDirectories(getAccountCacheFolder);
    end;

    filename:=getAccountCacheFolder+'\'+MASTER_INDEX_FILENAME;
    AssignFile(mMasterFile, filename);
    ReWrite(mMasterFile);

    try
        for i:=0 to mFolders.Count-1 do begin
            masterEntryPtr := PMasterMboxCacheEntry(mFolders[i]);
            masterEntry:=masterEntryPtr^;
            write(mMasterFile, masterEntry);
        end;
    finally
        CloseFile(mMasterFile);
    end;

end;

{ Renames account (will delete old phisical folder) }
procedure TICache.RenameAccount(oldName, newName: String);
begin
    // @todo (need to update old files, then rename account folder)
end;

{ Get cached info of the specified folder.
  If the record is not found will return a default record with
  empty values }
function TICache.GetFolderCacheInfo(folderName: String): TMasterMboxCacheEntry;
var idx: Integer; cachedFolderInfo: TMasterMboxCacheEntry;
begin
    try
        idx:=getFolderCacheInfoIndex(folderName);
        if idx>-1 then
            Result:=PMasterMboxCacheEntry(mFolders[idx])^
        else begin
            devLog.Warn('Entry not found in master cache for folder '+folderName+'. Creating a new entry');
            cachedFolderInfo.RealMailboxName:=folderName;
            cachedFolderInfo.LastUpdated:=Now;
            cachedFolderInfo.UIDValidity:=0;
            cachedFolderInfo.MaxUID:=0;
            cachedFolderInfo.Size:=0;
            cachedFolderInfo.Exists:=0;
            Result:=cachedFolderInfo;
        end;
    finally
        //
    end;
end;

{ Updates (adds if not there) folder info }
procedure TICache.UpdateFolderCacheInfo(folderCachedInfo: TMasterMboxCacheEntry);
var idx: Integer; entryPtr: PMasterMboxCacheEntry;
begin
    idx:=getFolderCacheInfoIndex(folderCachedInfo.RealMailboxName);
    try
        EnterCriticalSection(cs);
        if idx>-1 then begin
            // Entry exists, update it
            CopyMasterEntry(folderCachedInfo, PMasterMboxCacheEntry(mFolders[idx])^);
        end
        else begin
            New(entryPtr);
            CopyMasterEntry(folderCachedInfo, entryPtr^);
            mFolders.Add(entryPtr);
        end;
        mMasterCacheNeedsSaving:=true;
    finally
        LeaveCriticalSection(cs);
    end;
end;

{ Clears the cache for the specific folder }
function TICache.ClearFolderCache(folderName: String): Boolean;
var id: Integer; filename: String;
begin
    id:=GetFolderCacheInfoIndex(folderName);
    // Delete entry in master cache
    if (id>-1) then begin
        Dispose(PMasterMboxCacheEntry(mFolders[id]));
        mFolders.Delete(id);
    end;
    // Delete message cache
    filename := GetAccountCacheFolder + '\' + GetMailboxCacheFilename(folderName);
    Result:=Sysutils.DeleteFile(filename);
end;

function TICache.ClearAccountCache(account: String): Boolean;
var accCacheDir: String;
begin
    // Clear cache in memory (this account is the only one loaded)
    FreeListItems;
    // Clear cache on disk
    accCacheDir:= mAppFolder + '\cache\' + account;
    if DirectoryExists(accCacheDir) then Result:=DelTree(accCacheDir)
    else Result:=true;
end;

function TICache.ClearCache: Boolean;
var cacheDirName: String;
begin
    // Clear cache in memory
    FreeListItems;
    // Clear cache on disk
    cacheDirName:= mAppFolder + '\cache';
    if DirectoryExists(cacheDirName) then Result:=DelTree(cacheDirName)
    else Result:=true;
end;

{ Get the index of the specified folder
  returns -1 if record is not found }
function TICache.GetFolderCacheInfoIndex(folderName: String): Integer;
var entry: TMasterMboxCacheEntry; found: boolean; i: Integer;
begin
    found:=false;
    i:=0;
    try
        //EnterCriticalSection(cs);
        while (not found) and (i<mFolders.Count) do begin
            entry := PMasterMboxCacheEntry(mFolders[i])^;
            if entry.RealMailboxName = folderName then begin
                found:=true;
            end
            else Inc(i);
        end;
        if found then Result:=i
        else Result:=-1;
    finally
        //LeaveCriticalSection(cs);
    end;
end;

procedure TICache.CopyMasterEntry(var fromEntry, toEntry: TMasterMboxCacheEntry);
var i: Integer;
begin
    for i:=1 to 120 do toEntry.RealMailboxName[i]:='x';  // clean string (garbage behind visible in the file)
    toEntry.RealMailboxName:=fromEntry.RealMailboxName;
    toEntry.LastUpdated:=fromEntry.LastUpdated;
    toEntry.UIDValidity:=fromEntry.UIDValidity;
    toEntry.MaxUID:=fromEntry.MaxUID;
    toEntry.Size:=fromEntry.Size;
    toEntry.Exists:=fromEntry.Exists;
end;

{ Returns the folder where the cache files for the current account are stored }
function TICache.GetAccountCacheFolder: String;
begin
    Result:= mAppFolder + '\cache\' + mCurrentAccount;
end;

procedure TICache.FreeListItems;
var i: Integer;
begin
    for i:=mFolders.Count-1 downto 0 do begin
        Dispose(PMasterMboxCacheEntry(mFolders[i]));
        mFolders.Delete(i);
    end;
end;

{ Generates the local phisycal name of the file with
  the folder (mailbox) cache }
function TICache.GetMailboxCacheFilename(realMboxName: String): String;
begin
    // Lotus Domino fix (realMboxName stored with \\ separator - descape)
    realMboxName:=StringReplace(realMboxName,'\\','\',[rfReplaceAll]);

    // Replace invalid filename characters
    realMboxName:=invPathRegEx.Replace(realMboxName,'_');

    Result:=StringReplace(realMboxName,mMailboxSeparator,FOLDER_SEPARATOR,[rfReplaceAll]);
end;

{ Retrieves the contents of the message cache for
  the specified folder }
procedure TICache.LoadCachedFolderMsgs(folderName: String; var cachedMsgs: THashTable);
var filename: String;
    cachedMsgEntryPtr: PCachedMessage;
    cachedMsgFile: TCachedMessageFile;
begin
    filename := GetAccountCacheFolder + '\' + GetMailboxCacheFilename(folderName);
    cachedMsgs.Clear;
    if FileExists(filename) then begin
        EnterCriticalSection(cs);
        AssignFile(cachedMsgFile, filename);
        Reset(cachedMsgFile);
        try
            try
                while not Eof(cachedMsgFile) do begin
                    new (cachedMsgEntryPtr);
                    Read(cachedMsgFile, cachedMsgEntryPtr^);
                    cachedMsgs[cachedMsgEntryPtr^.UID]:=cachedMsgEntryPtr;
                end;
            except
                on E:Exception do
                    raise ECacheException.Create('Unable to load cached messages. Cause: '+E.Message);
            end;
        finally
            Close(cachedMsgFile);
            LeaveCriticalSection(cs);
        end;
    end
    else begin
        devLog.Debug('Message cache doesn''t exist for '+folderName+'. Creating a new one');
    end;
end;

{ Saves the list of messages to the file with cached messages for the
  specified folder. Overwrites the contents }
procedure TICache.SaveCachedFolderMsgs(folderName: String; cachedMsgs: THashTable);
var filename: String;
    cachedMsgEntryPtr: PCachedMessage;
    cachedMsgEntry: TCachedMessage;
    cachedMsgFile: TCachedMessageFile;
begin
    if not DirectoryExists(GetAccountCacheFolder) then begin
        ForceDirectories(GetAccountCacheFolder);
    end;

    filename := GetAccountCacheFolder + '\' + GetMailboxCacheFilename(folderName);
    EnterCriticalSection(cs);
    AssignFile(cachedMsgFile, filename);
    ReWrite(cachedMsgFile);

    try
        cachedMsgEntryPtr:=cachedMsgs.First;
        while (cachedMsgEntryPtr<>nil) do begin
            cachedMsgEntry:=cachedMsgEntryPtr^;
            write(cachedMsgFile, cachedMsgEntry);
            cachedMsgEntryPtr:=cachedMsgs.Next;
        end;
        devLog.Trace('Message cache updated for ' + folderName);
    finally
        CloseFile(cachedMsgFile);
        LeaveCriticalSection(cs);
    end;
end;

{ Call this method to safely deallocate the message cache }
procedure TICache.FreeCachedMessages(var cachedMsgs: THashTable);
var cachedMsgPtr: PCachedMessage;
begin
    if cachedMsgs<>nil then begin
        cachedMsgPtr:=cachedMsgs.First;
        while (cachedMsgPtr<>nil) do begin
            Dispose(cachedMsgPtr);
            cachedMsgPtr:=cachedMsgs.Next;
        end;
        cachedMsgs.Free;
    end;
end;

{ For debugging only }
procedure TICache.DumpContents(folderName: String);
var cachedMsgs: THashTable; cachedMsgPtr: PCachedMessage; masterEntry: TMasterMboxCacheEntry;
begin
    devLog.Trace('====== Dumping cache contents for: '+folderName);
    masterEntry:=GetFolderCacheInfo(folderName);
    devLog.Trace('   FullMailboxName: '+masterEntry.RealMailboxName);
    devLog.Trace('   LastUpdated:     '+DateToStr(masterEntry.LastUpdated));
    devLog.Trace('   UIDValidity:     '+IntToStr(masterEntry.UIDValidity));
    devLog.Trace('   MaxUID:          '+IntToStr(masterEntry.MaxUID));
    devLog.Trace('   Size:            '+IntToStr(masterEntry.Size));
    devLog.Trace('   Exists:          '+IntToStr(masterEntry.Exists));
    try
        try
            cachedMsgs:=THashTable.Create;
            LoadCachedFolderMsgs(folderName, cachedMsgs);
            cachedMsgPtr:=cachedMsgs.First;
            while (cachedMsgPtr<>nil) do begin
                devLog.Trace('UID: '+IntToStr(cachedMsgPtr^.UID)+', Size: '+IntToStr(cachedMsgPtr^.Size));
                cachedMsgPtr:=cachedMsgs.Next;
            end;
        except
            devLog.Trace('Error getting message cache');
        end;
    finally
        FreeCachedMessages(cachedMsgs);
    end;
    devLog.Trace('====== cache dump done ======');
end;


{ Global method for converting TMessageInfo to TCachedMessage }
function MessageInfoToCachedMessage(const msgInfo: TMessageInfo): TCachedMessage;
var cachedMsg: TCachedMessage;
begin
    cachedMsg.FullInfo:=true;
    try
        cachedMsg.UID:=StrToInt(msgInfo.mUID);
    except
        //dbg('---ERROR '+msgInfo.mUID);
    end;
    cachedMsg.Size:=msgInfo.mSize;
    cachedMsg.Date:=msgInfo.mDate;
    cachedMsg.Subject:=msgInfo.mSubject;
    cachedMsg.MsgFrom:=msgInfo.mFrom;
    cachedMsg.MsgTo:=msgInfo.mTo;
    cachedMsg.Flags:=msgInfo.mFlags;
    cachedMsg.SpamScore:=msgInfo.mSpamScore;
    cachedMsg.IsSpam:=msgInfo.mIsSpam;
    cachedMsg.HasAttachments:=msgInfo.mHasAttachments;
    Result:=cachedMsg;
end;

function CachedMesageToMessageInfo(const cachedMsg: TCachedMessage): TMessageInfo;
var msgInfo: TMessageInfo;
begin
    msgInfo.mUID:=IntToStr(cachedMsg.UID);
    msgInfo.mSize:=cachedMsg.Size;
    msgInfo.mSizeDisplay:=formatSizeDisplay(msgInfo.mSize);
    msgInfo.mDate:=cachedMsg.Date;
    msgInfo.mDateStr:=DateTimeToStr(msgInfo.mDate);  //@todo need to sync format with IMAPWorker.fillEnvelopeDetails
    msgInfo.mSubject:=cachedMsg.Subject;
    msgInfo.mFrom:=cachedMsg.MsgFrom;
    msgInfo.mTo:=cachedMsg.MsgTo;
    msgInfo.mImageIndex:=GetMessageImageIndex(cachedMsg.Size,cachedMsg.HasAttachments);
    msgInfo.mFlags:=cachedMsg.Flags;
    msgInfo.mSpamScore:=cachedMsg.SpamScore;
    msgInfo.mIsSpam:=cachedMsg.IsSpam;
    msgInfo.mHasAttachments:=cachedMsg.HasAttachments;
    Result:=msgInfo;
end;

end.