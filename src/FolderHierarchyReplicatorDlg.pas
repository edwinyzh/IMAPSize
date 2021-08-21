unit FolderHierarchyReplicatorDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, PBFolderDialog, AppSettings, GlobalConstants, VecLog, RegExpr;

const
    invFolderNameRE='\s*\\';    // looks for cases like "parentFolder \childFolder \rootFolder"

type
  TFFolderHierarchyReplicatorDlg = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Label1: TLabel;
    Button1: TButton;
    Button2: TButton;
    Label3: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    LblLocation: TLabel;
    Label5: TLabel;
    LblMethod: TLabel;
    procedure Button1Click(Sender: TObject);
  private
    backupDir: String;
    procedure InvokeRequest;
  public
    procedure SetFolder(folder: String);
    procedure HandleRequestOver(var Message: TMessage); message WM_REQUEST_OVER;
  end;

function CreateEmptyFile(filename: String): Boolean;
function CreateLocalFolders(var folderList: TStringList; backupDir: String; separator: char): Integer;
function GetLocalEMLFolder(backupDir: String; remoteFolder: String; separator: Char; relative: Boolean): String;
function GetLocalMboxFile(backupDir: String; remoteFolder: String; separator: Char): String;

var
  FFolderHierarchyReplicatorDlg: TFFolderHierarchyReplicatorDlg;

implementation

uses Main, RequestManager, IMAPWorker, CustomThread, FileCtrl;

{$R *.DFM}

procedure TFFolderHierarchyReplicatorDlg.SetFolder(folder: String);
begin
    backupDir:=folder;
    LblLocation.Caption:=folder;
    //if settings.MiscSettings.BackupMethod=0 then LblMethod.Caption:='eml files'
    //else LblMethod.Caption:='mbox files';
end;

{ Invokes request for folder hierarchy retrieval }
procedure TFFolderHierarchyReplicatorDlg.InvokeRequest;
var requestPtr: PRequest;
begin
    New(requestPtr);
    requestPtr^ := TRequest.Create(thtIMAPWorker);
    requestPtr^.requestType:=rtGetFolderList;
    requestPtr^.requestComponent:=rcFolderHierarchyReplicator;
    requestPtr^.imapOperation:=iopGetFolderList;
    requestPtr^.accountInfoPtr:=FMain.pActiveAccount;
    requestMgr.InvokeRequest(requestPtr);
end;

{ Handles messages sent by the request manager when a request is over }
procedure TFFolderHierarchyReplicatorDlg.HandleRequestOver(var Message: TMessage);
var requestID, i, cnt: Integer; success: Integer; request: TRequest; s: String;
begin
    devLog.Trace('TFFolderSubDlg.HandleRequestOver');
    requestId:=Message.WParam;
    try
        request := requestMgr.GetRequest(requestId);
        success:=Message.LParam;
        if request.requestType = rtGetFolderList then begin
            if success=1 then begin
                try
                    cnt:=CreateLocalFolders(request.data.msgContent,backupDir,request.data.folderSeparator);
                    //if settings.MiscSettings.BackupMethod=0 then s:='folders'
                    //else s:='files';
                    s:='folders';
                    if cnt=0 then
                        MessageDlg('All local '+s+' already exist - no '+s+' were created',mtInformation,[mbOk],0)
                    else
                        MessageDlg('Folder hierarchy replicated in local folder: '+backupDir+CRLF+'Number of new '+s+' created: '+IntToStr(cnt),mtInformation,[mbOk],0);
                except
                    on E:Exception do begin
                        MessageDlg('Cannot replicate folder hierarchy. Error: '+E.Message,mtError,[mbOk],0);
                        // requestMgr.LogError(request.requestId,'Cannot replicate folder hierarchy. Error: '+E.Message);
                    end;
                end;
            end;
        end
    finally
        requestMgr.RemoveRequest(requestId, false);
        ModalResult:=mrOK;
    end;
end;

procedure TFFolderHierarchyReplicatorDlg.Button1Click(Sender: TObject);
begin
    InvokeRequest;
end;

function CreateLocalFolders(var folderList: TStringList; backupDir: String; separator: char): Integer;
var i, cnt: Integer; mboxFile, emlFolder: String;
begin
    // Make sure the backup dir exists
    if not DirectoryExists(backupDir) then begin
        devLog.Trace('Creating backup directory: '+backupDir);
        if not ForceDirectories(backupDir) then
            raise Exception.Create('Unable to create folder '+backupDir);
    end;

    cnt:=0;
    // eml storage selected
    for i:=0 to folderList.Count-1 do begin
        emlFolder:=GetLocalEMLFolder(backupDir,folderList[i],separator,false);
        if not DirectoryExists(emlFolder) then begin
            devLog.trace('Creating local folder: '+emlFolder);
            if ForceDirectories(emlFolder) then Inc(cnt)
             else raise Exception.Create('Unable to create folder '+emlFolder);
        end;
    end;

    Result:=cnt;
end;

{ Creates an empty file }
function CreateEmptyFile(filename: String): Boolean;
var f: TextFile;
begin
    if not FileExists(filename) then begin
        AssignFile(f, filename);
        Rewrite(f);
        CloseFile(f);
        Result:=true;
    end
    else Result:=true;
end;

{ Returns a local folder name (full path) where eml files should be stored for the specifed remote folder
  If relative is true will return the path relative to backupDir, otherwise a full path will be returned }
function GetLocalEMLFolder(backupDir: String; remoteFolder: String; separator: Char; relative: Boolean): String;
var emlFolder: String;
    invFolderNameRegEx: TRegExpr;
    invPathRegEx: TRegExpr;
begin
    emlFolder:=remoteFolder;
    emlFolder:=StringReplace(emlFolder,separator,'\',[rfReplaceAll]);
    // Need to remove trailing spaces from all folders in the path
    // otherwise there are problems with performing backups
    invFolderNameRegEx:=TRegExpr.Create;
    invFolderNameRegEx.Expression:=invFolderNameRE;
    emlFolder:=invFolderNameRegEx.Replace(emlFolder,'\');
    invFolderNameRegEx.Free;
    emlFolder:=TrimRight(emlFolder);

    // Replace invalid filename characters
    invPathRegEx:=TRegExpr.Create;
    invPathRegEx.Expression:=invPathRE;
    emlFolder:=invPathRegEx.Replace(emlFolder,'_');
    invPathRegEx.Free;

    if not relative then emlFolder:=backupDir+'\'+emlFolder;
    Result:=emlFolder;
end;

{ Returns a local mbox name for the specifed remote folder }
function GetLocalMboxFile(backupDir: String; remoteFolder: String; separator: Char): String;
var mboxFile: String;
begin
    mboxFile:=remoteFolder;
    mboxFile:=StringReplace(mboxFile,separator,'.',[rfReplaceAll]);
    mboxFile:=backupDir+'\'+mboxFile+'.mbox';
    Result:=mboxFile;
end;

end.
