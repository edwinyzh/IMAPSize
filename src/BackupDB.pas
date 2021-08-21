{==============================================================================|
| Project : IMAPSize                                                           |
|==============================================================================|
| $Id$
|==============================================================================|
| Copyright 2003-2005 Ivan Vecanski                                            |
| =============================================================================}

unit BackupDB;

{ Manages database stuff, currently the backup db only }

interface

uses SQLiteTable3, VecLog, Log, MyUtil, sysutils, classes, filectrl, CommonTypes;

const
    LAST_BACKED_UP_DATE_FORMAT = 'yyyy-mm-dd hh:nn';
    DATABASE_VERSION = '1'; // Database version, could be useful for updates

type

    TBackupDB = class
        slDBpath: string;
        sldb: TSQLiteDatabase;
        sltb: TSQLiteTable;
    public
        constructor Create(rootBackupDir: String);
        destructor Destroy; override;
        procedure InitializeTables;
        function Insert(sql: String): Int64;
        procedure Update(sql: String);
        function GetAccountFolderID(accountName, remoteFolder, emlFolder: String; uidValidity: Integer): Integer;
        procedure GetExistingMessageUIDs(messageUIDs: TStringList; backupFolderId: Integer); overload;
        procedure GetExistingMessageUIDs(messageUIDs: TStringList; accountName, localFolder: String); overload;
        procedure GetMessageIds(var messageIds: TStringList; accountName, remoteFolder: String);
        function GetFilenameByUID(accountName, localFolder, UID: String): String;
        function GetFilenameByMessageId(accountName, localFolder, messageId: String): String;
        function GetMaxMessageIDForFolder(backupFolderId: Integer): String;
        procedure UpdateAccountName(oldAccountName, newAccountName: String);
        procedure UpdateLastBackedUp(backupFolderId: Integer);
        procedure UpdateRootBackupDir(newRootBackupDir: String);
        procedure GetBackupFolderInfos(accountName: String; var folderInfos: TListOfBackupFolderInfos);
        property db: TSQLiteDatabase read sldb;
    end;

implementation

constructor TBackupDB.Create(rootBackupDir: String);
var backupDbFile: String;
begin
    devLog.Debug('Initializing embedded backup database');

    slDBpath := rootBackupDir;
    // Check if the previous backup exists without the backup.db file.
    // If yes, rename it to something else and recreate the backup folder
    backupDbFile := rootBackupDir + '\backup.db';

    devLog.Info('Backup directory ['+rootBackupDir+']');

    if DirectoryExists(rootBackupDir) then begin
        devLog.Info('Backup directory already exists');
        if not FileExists(backupDbFile) then begin
            // backup.db doesn't exist, check if there are any subfolders
            // All is fine if there are no subfolders
            if HasSubdirs(rootBackupDir) then begin
                devLog.Warn('Old (0.2.x) backup found. Renaming existing backup to '+rootBackupDir+'-old');
                if RenameFile(rootBackupDir,rootBackupDir+'--old') then
                    devLog.Info('Existing backup folder renamed. Recreating backup folder')
                else
                    devLog.Error('Failed to rename folder');
                if ForceDirectories(rootBackupDir) then
                    devLog.Info('Recreated')
                else begin
                    devLog.Error('Failed to create '+rootBackupDir);
                    // raise Exception.Create('Failed to create '+rootBackupDir);
                end;
            end;
        end;
    end
    else begin
        // rootBackupDir does not exist, create it
        devLog.Info('Backup directory does not exist. Creating '+rootBackupDir);
        ForceDirectories(rootBackupDir);
        if DirectoryExists(rootBackupDir) then devLog.Info('Folder created')
        else devLog.Info('Failed to create folder!');
    end;

    sldb := TSQLiteDatabase.Create(backupDbFile);
    InitializeTables;
end;

destructor TBackupDB.Destroy;
begin
    sldb.Free;
    devLog.Debug('Embedded database closed');
end;

{ Creates tables if they don't already exist }
procedure TBackupDB.InitializeTables;
var sql: String;
begin
    sldb.BeginTransaction;
    if not sldb.TableExists('backup_folder') then begin
        devLog.Debug('Creating table [backup_folder]');
        // accountFolder is the name of the folder where emls are stored for an account
        // While accountName can be changed, accountFolder remains static
        sql := 'CREATE TABLE backup_folder(folderId INTEGER PRIMARY KEY AUTOINCREMENT,accountName TEXT NOT NULL,remoteFolder TEXT NOT NULL, uidValidity INTEGER,relativeFolderPath TEXT NOT NULL,lastBackedUp TEXT);';
        sldb.ExecSQL(sql);
    end;
    if not sldb.TableExists('message') then begin
        devLog.Debug('Creating table [message]');
        sql := 'CREATE TABLE message (folderId INTEGER NOT NULL,uid INTEGER NOT NULL,messageID TEXT, filename TEXT NOT NULL);';
        sldb.ExecSQL(sql);
    end;
    if not sldb.TableExists('version') then begin
        sldb.ExecSQL('CREATE TABLE version(version INTEGER NOT NULL);');
        sldb.ExecSQL('INSERT INTO version VALUES ('+DATABASE_VERSION+');');
    end;
    sldb.Commit;
end;

{ Executes an insert statement and returns the autoincrement id }
function TBackupDB.Insert(sql: String): Int64;
begin
    // dbg(sql);
    sldb.ExecSQL(sql);
    Result:=sldb.GetLastInsertRowID;
end;

procedure TBackupDB.Update(sql: String);
begin
    // dbg(sql);
    sldb.ExecSQL(sql);
end;

{ Get the id of the row in backup_folder with the specified accountName and emlFolder.
  remoteFolder is stored for reference (needed for restore)
  If the row doesn't exist, a new one will be created }
function TBackupDB.GetAccountFolderID(accountName, remoteFolder, emlFolder: String; uidValidity: Integer): Integer;
var sltb: TSQLIteTable;
begin
    sltb := slDb.GetTable('SELECT folderId FROM backup_folder WHERE accountName="'+accountName+'" and relativeFolderPath="'+emlFolder+'"');
    if sltb.rowcount=0 then begin
        // Create a new entry
        Result:=Insert('INSERT INTO backup_folder VALUES (null,"'+accountName+'","'+remoteFolder+'",'+IntToStr(uidValidity)+',"'+emlFolder+'",null)');
    end
    else begin
        Result:=sltb.FieldAsInteger(sltb.FieldIndex['folderId']);
    end;
end;

{ Get the list of existing UIDs for the specified backup folder }
procedure TBackupDB.GetExistingMessageUIDs(messageUIDs: TStringList; backupFolderId: Integer);
var sltb: TSQLIteTable; i: Integer;
begin
    sltb:=slDb.GetTable('SELECT uid FROM message WHERE folderId='+IntToStr(backupFolderId));
    for i:=0 to sltb.rowcount-1 do begin
        messageUIDs.Add(sltb.FieldAsString(sltb.FieldIndex['uid']));
        sltb.Next;
    end;
end;

{ Get the UIDs for a folder specified by the account name and local folder }
procedure TBackupDB.GetExistingMessageUIDs(messageUIDs: TStringList; accountName, localFolder: String);
var sltb: TSQLIteTable; backupFolderId: Integer;
begin
    // Get the backupFolderId
    sltb := slDb.GetTable('SELECT folderId FROM backup_folder WHERE accountName="'+accountName+'" and relativeFolderPath="'+localFolder+'"');
    if sltb.rowcount>0 then begin
        backupFolderId:=sltb.FieldAsInteger(sltb.FieldIndex['folderId']);
        GetExistingMessageUIDs(messageUIDs, backupFolderId);
    end;
end;

{ Get the list of message-ids for the specified account and folder }
procedure TBackupDB.GetMessageIds(var messageIds: TStringList; accountName, remoteFolder: String);
var sltb: TSQLIteTable; i: Integer; query: String;
begin
    query:=       'select messageID from message ';
    query:=query+ 'where folderId in (select folderId from backup_folder where accountName="'+accountName+'" and remoteFolder="'+remoteFolder+'") ';
    query:=query+ 'and messageID<>""';
    sltb:=slDb.GetTable(query);
    for i:=0 to sltb.rowcount-1 do begin
        messageIds.Add(sltb.FieldAsString(sltb.FieldIndex['messageID']));
        sltb.Next;
    end;
end;

{ Gets the full filename for the account, folder and messageId }
function TBackupDB.GetFilenameByMessageId(accountName, localFolder, messageId: String): String;
var sltb: TSQLIteTable; query: String;
begin
    query:=       'select filename from message ';
    query:=query+ 'where folderId in (select folderId from backup_folder where accountName="'+accountName+'" and relativeFolderPath="'+localFolder+'") ';
    messageId:=StringReplace(messageId,'"','""',[rfReplaceAll]);
    query:=query+ 'and messageID="'+messageId +'"';
    sltb:=slDb.GetTable(query);
    if sltb.rowcount=0 then Result:=''
    else Result:= slDBpath + '\'+ accountName + '\' + localFolder + '\' + sltb.FieldAsString(sltb.FieldIndex['filename']);
end;

function TBackupDB.GetFilenameByUID(accountName, localFolder, UID: String): String;
var sltb: TSQLIteTable; query: String;
begin
    query:=       'select filename from message ';
    query:=query+ 'where folderId in (select folderId from backup_folder where accountName="'+accountName+'" and relativeFolderPath="'+localFolder+'") ';
    query:=query+ 'and UID="'+UID +'"';
    sltb:=slDb.GetTable(query);
    if sltb.rowcount=0 then Result:=''
    else Result:= slDBpath + '\'+ accountName + '\' + localFolder + '\' + sltb.FieldAsString(sltb.FieldIndex['filename']);
end;

{ Returns an empty string if the maxuid is not found }
function TBackupDB.GetMaxMessageIDForFolder(backupFolderId: Integer): String;
var sltb: TSQLIteTable; i: Integer;
begin
    sltb:=slDb.GetTable('SELECT MAX(uid) AS maxuid FROM message WHERE folderId='+IntToStr(backupFolderId));
    if sltb.rowcount=0 then Result:=''
    else Result:=sltb.FieldAsString(sltb.FieldIndex['maxuid']);
end;

procedure TBackupDB.UpdateLastBackedUp(backupFolderId: Integer);
var sql: String; dateStr: String;
begin
    dateStr:=FormatDateTime(LAST_BACKED_UP_DATE_FORMAT, now);
    sql:='update backup_folder set lastBackedUp="'+dateStr+'" where folderId='+IntToStr(backupFolderId);
    sldb.ExecSQL(sql);
end;

procedure TBackupDB.UpdateAccountName(oldAccountName, newAccountName: String);
var sql, accountFolderName: String;
begin
    devLog.Debug('Updating account name for backup (database and folder name)');
    // Change the account name in backup_folder (if it exists)
    sql := 'update backup_folder set accountName="'+newAccountName+'" where accountName="'+oldAccountName+'"';
    sldb.ExecSQL(sql);
    // Now update the folder name
    accountFolderName:=slDBpath+'\'+oldAccountName;
    if DirectoryExists(accountFolderName) then begin
        RenameFile(accountFolderName,slDBPath+'\'+newAccountName);
    end
    else devLog.Debug('Directory '+accountFolderName+' does not exist');
end;

{ Copy the backup folder to another location }
procedure TBackupDB.UpdateRootBackupDir(newRootBackupDir: String);
var FSR: TSearchRec;
begin
    devLog.Info('Moving root backup dir from '+slDBPath+' to '+newRootBackupDir);
    // Need to close the database to enable the file to be moved
    sldb.Free;
    // Move dirs
    if FindFirst(slDBPath + '\*.*', faDirectory, FSR) = 0 then begin
        repeat
            if (FSR.Attr and faDirectory) > 0 then begin  // is a directory
                if not (FSR.Name[1] = '.') then  // not this or parent
                    // Move directory
                    MoveDir(slDBPath+'\'+FSR.Name,newRootBackupDir);
            end;
        until not (FindNext(FSR) = 0);
        FindClose(FSR);
    end;
    // Move backup.db
    RenameFile(slDBPath+'\backup.db',newRootBackupDir+'\backup.db');
    // Leave the old backup dir and any files in place (might contain other users stuff)

    slDBPath:=newRootBackupDir;
    // Reopen database
    sldb := TSQLiteDatabase.Create(slDBPath + '\backup.db');
end;

{ Forms BackupFolderInfos for the specified account.
  The sql could be improved a bit... }
procedure TBackupDB.GetBackupFolderInfos(accountName: String; var folderInfos: TListOfBackupFolderInfos);
var i : Integer; sltb: TSQLIteTable; sql: String;
begin

    sltb := slDb.GetTable('select remoteFolder, relativeFolderPath, lastBackedUp from backup_folder where accountName="'+accountName+'"');
    SetLength(folderInfos,sltb.rowcount);
    for i:=0 to sltb.rowcount-1 do begin

        // now read the data from this table (contains one row only)
        folderInfos[i].remoteFolder:=sltb.FieldAsString(sltb.FieldIndex['remoteFolder']);
        folderInfos[i].localFolder:=sltb.FieldAsString(sltb.FieldIndex['relativeFolderPath']);
        folderInfos[i].numMessages:=0;
        folderInfos[i].lastUpdated:=sltb.FieldAsString(sltb.FieldIndex['lastBackedUp']);
        folderInfos[i].newRemoteName:=folderInfos[i].remoteFolder;

        sltb.Next;
    end;
end;

end.
