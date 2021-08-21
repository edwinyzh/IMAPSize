{==============================================================================|
| Project : IMAPSize                                                           |
|==============================================================================|
| $Id: Accounts.pas,v 1.8 2004/03/31 23:27:40 Ivan Exp $
|==============================================================================|
| Copyright 2003-2004 Ivan Vecanski                                            |
| =============================================================================}

unit Accounts;

interface

uses Classes, SysUtils, EncryptIt, VecLog, Log, Hashtables, MyUtil;

type

    TAccountInfo = class(TPersistent)
    private
        fName: String;
        fUsername: String;
        fPassword: String;        // Encrypted password
        fSavePassword: Boolean;
        fIMAPServer: String;
        fPort: Integer;
        fSSL: Boolean;
        fSpamHandle: Integer;
        fSmartInbox: Boolean;     // If true will make INBOX -> INBOX.Inbox if all start with INBOX.
        fRootFolder: String;      // Root mailbox for the LIST command (for use in FMain.mLimitedNamespace)
        fExcludeRoot: Boolean;    // Whether to exclude the root folder from the hierarchy (if root folder is defined)
        fIncludeShared: Boolean;
        fIncludePublic: Boolean;
        fBackupFolders: String;   // Folders to backup
        fFolderSeparator: Char;   // Folder separator (not configurable - just stored for reference)
    public
        constructor Create;
    published
        property Name: String read fName write fName;
        property Username: String read fUsername write fUsername;
        property Password: String read fPassword write fPassword;
        property SavePassword: Boolean read fSavePassword write fSavePassword;
        property IMAPServer: String read fIMAPServer write fIMAPServer;
        property Port: Integer read fPort write fPort;
        property SSL: Boolean read fSSL write fSSL;
        property SpamHandle: Integer read fSpamHandle write fSpamHandle;
        property SmartInbox: Boolean read fSmartInbox write fSmartInbox;
        property RootFolder: String read fRootFolder write fRootFolder;
        property ExcludeRoot: Boolean read fExcludeRoot write fExcludeRoot;
        property IncludeShared: Boolean read fIncludeShared write fIncludeShared;
        property IncludePublic: Boolean read fIncludePublic write fIncludePublic;
        property BackupFolders: String read fBackupFolders write fBackupFolders;
        property FolderSeparator: Char read fFolderSeparator write fFolderSeparator;
    end;
    PAccountInfo = ^TAccountInfo;

    { Just a wrapper around a TAccountInfo, since it needs to be a CollectionItem }
    TAccountItem = class(TCollectionItem)
    private
        fAccountInfo: TAccountInfo;
    public
        constructor Create(Collection:TCollection);override;
        destructor  Destroy;override;
    published
        property AccountInfo: TAccountInfo read fAccountInfo write fAccountInfo;
    end;

    TAccounts = class(TPersistent)
    private
        fAccountItems: TCollection;
        fDefaultIndex: Integer;  // Index of the default account
        function GetAccountIndex(name: String): Integer;
    public
        constructor Create;
        function GetAccount(index: Integer): TAccountInfo; overload;
        function GetAccount(name: String): TAccountInfo; overload;
        function accountExists(name: String): Boolean;
        procedure addAccount(var account: TAccountInfo);
        procedure updateAccount(accountName: String; var account: TAccountInfo; backupExists: Boolean);
        procedure removeAccount(name: String);
        procedure adjustDeletedSpamHandle(index: Integer);
        destructor  Destroy;override;
    published
        property DefaultIndex:Integer read fDefaultIndex write fDefaultIndex;
        property AccountItems: TCollection read fAccountItems write fAccountItems;
    end;

    function AccountsEqual(var acc1, acc2: TAccountInfo): Boolean;
    procedure CopyAccount(var acctFrom, acctTo: TAccountInfo);
    procedure StorePassInMemory(accountName: String; password: String);
    function GetPassFromMemory(accountName: String): String;

implementation

uses Main;


var
    memoryPasswords: TStringTable; // Hashtable for storing in-memory passwords (only for accounts where StorePassword is false)

constructor TAccountInfo.Create;
begin
    SetLength(fName,0);
    SetLength(fUsername,0);
    SetLength(fPassword,0);
    SetLength(fIMAPServer,0);
    SetLength(fRootFolder,0);
    SetLength(fBackupFolders,0);
end;

constructor TAccounts.Create;
begin
    //inherited create(Collection);
    Registerclass(TAccountItem);
    fAccountItems:=TCollection.create(TAccountItem);
    memoryPasswords:=TStringTable.Create;
end;

function TAccounts.GetAccount(index: Integer): TAccountInfo;
begin
    Result := (fAccountItems.Items[Index] as TAccountItem).AccountInfo;
end;


function TAccounts.GetAccount(name: String): TAccountInfo;
var index: Integer;
begin
    index := getAccountIndex(name);
    //if (index > -1) then devLog.Error('GetAccount could not find account '+name); //@todo throw exception
    Result := GetAccount(index);
end;

function TAccounts.accountExists(name: String): Boolean;
begin
    Result := (GetAccountIndex(name) <> -1);
end;

function TAccounts.GetAccountIndex(name: String): Integer;
var found: Boolean; i: Integer;
begin
    found:=false;
    i:=0;
    while ((not found) and (i<fAccountItems.Count)) do begin
        if (GetAccount(i).Name = name) then begin
            found := true;
        end
        else Inc(i);
    end;
    if found then Result := i
    else Result := -1;
end;

procedure TAccounts.addAccount(var account: TAccountInfo);
var newAcct: TAccountInfo; newAcctItem: TAccountItem;
begin
    newAcctItem:=AccountItems.Add as TAccountItem;
    newAcct:=newAcctItem.AccountInfo;
    CopyAccount(account,newAcct);
end;

procedure TAccounts.removeAccount(name: String);
var index: Integer;
begin
    index := getAccountIndex(name);
    if index > -1 then begin
        fAccountItems.Delete(index);
    end
end;

procedure TAccounts.updateAccount(accountName: String; var account: TAccountInfo; backupExists: Boolean);
var index: Integer; acct: TAccountInfo;
begin
    if backupExists and (accountName<>account.Name) then begin
        // Account name has been changed, modify for backup stuff
        devLog.Info('Renaming account '+accountName+' to '+account.Name+' for backup');
        FMain.BackupDB.UpdateAccountName(accountName, account.Name);
    end;
    index := getAccountIndex(accountName);
    acct:=(fAccountItems.Items[Index] as TAccountItem).AccountInfo;
    // Copy non-visible account settings from acct to account
    // (these were not created in Options.SaveAccountChange (where this is called from))
    CopyString(account.fBackupFolders, acct.fBackupFolders);
    acct.fFolderSeparator:=account.fFolderSeparator;
    // Finally, copy settings from account to acct
    CopyAccount(account,acct);
end;

procedure TAccounts.adjustDeletedSpamHandle(index: Integer);
var i: Integer; acct: TAccountInfo;
begin
    for i:=0 to fAccountItems.Count-1 do begin
        acct:=(fAccountItems.Items[i] as TAccountItem).AccountInfo;
        if acct.fSpamHandle = index then acct.fSpamHandle:=0    // Set to None
        else if acct.fSpamHandle > index then Dec(acct.fSpamHandle)
        else; // leave index the same
    end;
end;

destructor TAccounts.Destroy;
begin
    try
        fAccountItems.free;
        memoryPasswords.Free;
    finally
        inherited destroy;
    end;
end;


constructor TAccountItem.Create(Collection:TCollection);
begin
    inherited create(Collection);
    Registerclass(TAccountInfo);
    fAccountInfo:=TAccountInfo.create;
    // Initialize var
    fAccountInfo.fSavePassword:=true;
end;

destructor TAccountItem.Destroy;
begin
    try
        fAccountInfo.Free;
    finally
        inherited destroy;
    end;
end;

function AccountsEqual(var acc1, acc2: TAccountInfo): boolean;
begin
    Result:=false;
    if ((acc1<>nil) and (acc2<>nil)) then begin
        devLog.Trace('Account 1 = ['+acc1.Name+']');
        devLog.Trace('Account 2 = ['+acc2.Name+']');
        try
            if ((acc1.Name=acc2.Name) and
               (acc1.Name <> '') and
               (acc1.Username=acc2.Username) and
               (acc1.Password=acc2.Password) and
               (acc1.SavePassword=acc2.SavePassword) and
               (acc1.IMAPServer=acc2.IMAPServer) and
               (acc1.Port=acc2.Port) and
               (acc1.SSL=acc2.SSL) and
               (acc1.SpamHandle=acc2.SpamHandle) and
               (acc1.IncludeShared = acc2.IncludeShared) and
               (acc1.IncludePublic = acc2.IncludePublic) and
               (acc1.ExcludeRoot = acc2.ExcludeRoot) and
               (acc1.RootFolder=acc2.RootFolder))
            then Result:=true;
        except
            devLog.Error('Error while checking AccountsEqual');
            if (acc1=nil) then devLog.Error('acc1 is nil');
            if (acc2=nil) then devLog.Error('acc2 is nil');
            Result:=false;
        end;
    end;
end;

procedure CopyAccount(var acctFrom, acctTo: TAccountInfo);
begin
    //acctTo.fName:=acctFrom.fName;
    CopyString(acctTo.fName,acctFrom.fName);
    CopyString(acctTo.fUsername,acctFrom.fUsername);
    CopyString(acctTo.fPassword,acctFrom.fPassword);
    acctTo.fSavePassword:=acctFrom.fSavePassword;
    CopyString(acctTo.fIMAPServer,acctFrom.fIMAPServer);
    acctTo.fPort:=acctFrom.fPort;
    acctTo.fSSL:=acctFrom.fSSL;
    acctTo.fSpamHandle:=acctFrom.fSpamHandle;
    acctTo.fSmartInbox:=acctFrom.fSmartInbox;
    CopyString(acctTo.fRootFolder,acctFrom.fRootFolder);
    acctTo.fExcludeRoot:=acctFrom.fExcludeRoot;
    acctTo.fIncludeShared:=acctFrom.fIncludeShared;
    acctTo.fIncludePublic:=acctFrom.fIncludePublic;
    CopyString(acctTo.fBackupFolders,acctFrom.fBackupFolders);
    acctTo.fFolderSeparator:=acctFrom.fFolderSeparator;
end;

// Stores the password (encrypted) in memory
procedure StorePassInMemory(accountName: String; password: String);
var pStr: PString;
begin
    New(pStr);
    pStr^ := password;
    memoryPasswords.Insert(accountName,TObject(pStr));
end;

// Get the password (encrypted) from memory
// Will return nil if not found
function GetPassFromMemory(accountName: String): String;
var obj: TObject; s: String; pStr: PString;
begin
    obj:=memoryPasswords.Items[accountName];
    if obj<>nil then begin
        pStr:=PString(obj);
        Result:=pStr^;
    end
    else begin
        Result:='';
    end;
end;



end.