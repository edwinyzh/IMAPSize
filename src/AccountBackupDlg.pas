unit AccountBackupDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, CheckLst, ExtCtrls, GlobalConstants, VecLog, PBFolderDialog,
  Buttons;

type
  TFAccountBackupDlg = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Button5: TButton;
    BtnBackup: TButton;
    Panel3: TPanel;
    Panel5: TPanel;
    CheckListBox1: TCheckListBox;
    Memo1: TMemo;
    Panel6: TPanel;
    Label8: TLabel;
    LblBackupDir: TLabel;
    LblStatus: TLabel;
    Label1: TLabel;
    Button1: TButton;
    Button2: TButton;
    BtnSaveFolders: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure BtnBackupClick(Sender: TObject);
    procedure BtnSaveFoldersClick(Sender: TObject);
  private
    procedure HandleRequestOver(var Message: TMessage); message WM_REQUEST_OVER;
    function  NumChecked: Integer;
    procedure SelectAll(select: Boolean);
  public
    procedure SetBackupFolder(backupDir: String);
    procedure PrepareMailboxList;
    procedure GetFoldersToBackup(var selectedFolders: TStringList);
    procedure SaveFolders(showMsg: Boolean);
    procedure CheckSavedFolders;
  end;

var
  FAccountBackupDlg: TFAccountBackupDlg;

implementation

{$R *.DFM}

uses Main, RequestManager, CustomThread, IMAPWorker;

procedure TFAccountBackupDlg.SetBackupFolder(backupDir: String);
begin
    LblBackupDir.Caption:=backupDir;
    LblStatus.Caption:='';
end;

{ Fills (or invokes filling from remote server) of the mailbox list }
procedure TFAccountBackupDlg.PrepareMailboxList;
var requestPtr: PRequest;
begin
    LblStatus.Caption:='Getting folder list...';
    New(requestPtr);
    requestPtr^ := TRequest.Create(thtIMAPWorker);
    requestPtr^.requestType:=rtGetFolderList;
    requestPtr^.requestComponent:=rcAccountBackup;
    requestPtr^.imapOperation:=iopGetFolderList;
    requestPtr^.accountInfoPtr:=FMain.pActiveAccount;
    requestMgr.InvokeRequest(requestPtr);
end;

{ Handles messages sent by the request manager when a request is over }
procedure TFAccountBackupDlg.HandleRequestOver(var Message: TMessage);
var requestID, i: Integer; success: Integer; request: TRequest;
begin
    devLog.Trace('TFFolderSubDlg.HandleRequestOver');
    requestId:=Message.WParam;
    request := requestMgr.GetRequest(requestId);
    success:=Message.LParam;
    if request.requestType = rtGetFolderList then begin
        if success=1 then begin
            CheckListBox1.Clear;
            for i:=0 to request.data.msgContent.Count-1 do begin
                CheckListBox1.Items.Add(request.data.msgContent[i]);
            end;
            FMain.pActiveAccount.FolderSeparator:=request.data.folderSeparator;
        end;
        LblStatus.Caption:='';
        CheckSavedFolders;
    end;
end;

{ Returns number of checked items in the check box list }
function TFAccountBackupDlg.NumChecked: Integer;
var count, i: Integer;
begin
    count:=0;
    for i:=0 to CheckListBox1.Items.Count-1 do begin
        if CheckListBox1.Checked[i] then Inc(count);
    end;
    Result:=count;
end;

procedure TFAccountBackupDlg.SelectAll(select: Boolean);
var i: Integer;
begin
    for i:=0 to CheckListBox1.Items.Count-1 do begin
        CheckListBox1.Checked[i]:=select;
    end;
end;


procedure TFAccountBackupDlg.Button1Click(Sender: TObject);
begin
    SelectAll(true);
end;

procedure TFAccountBackupDlg.Button2Click(Sender: TObject);
begin
    SelectAll(false);
end;

procedure TFAccountBackupDlg.GetFoldersToBackup(var selectedFolders: TStringList);
var i: Integer;
begin
    selectedFolders.Clear;
    for i:=0 to CheckListBox1.Items.Count-1 do begin
        if CheckListBox1.Checked[i] then selectedFolders.Add(CheckListBox1.Items[i]);
    end;
end;

procedure TFAccountBackupDlg.BtnBackupClick(Sender: TObject);
begin
    if NumChecked>0 then begin
        SaveFolders(false);
        ModalResult:=mrOK;
    end
    else
        MessageDlg('No folders selected! Please select at least one folder or click the Close button.',mtInformation,[mbOK],0);
end;

// Save selected Folders
procedure TFAccountBackupDlg.BtnSaveFoldersClick(Sender: TObject);
begin
    SaveFolders(true);
end;

procedure TFAccountBackupDlg.SaveFolders(showMsg: Boolean);
var selectedFolders: TStringList;
begin
    selectedFolders:=TStringList.Create;
    try
        GetFoldersToBackup(selectedFolders);
        FMain.pActiveAccount.BackupFolders:=selectedFolders.Text;
        if showMsg then MessageDlg('Folders saved',mtInformation,[mbOK],0);
    finally
        selectedFolders.Free;
    end;
end;

{ Check the folders that are saved for this account }
procedure TFAccountBackupDlg.CheckSavedFolders;
var selectedFolders: TStringList; i: Integer;
begin
    selectedFolders:=TStringList.Create;
    try
        selectedFolders.Text := FMain.pActiveAccount.BackupFolders;
        for i:=0 to CheckListBox1.Items.Count-1 do begin
            CheckListBox1.Checked[i] := selectedFolders.IndexOf(CheckListBox1.Items[i]) <> -1;
        end;
    finally
        selectedFolders.Free;
    end;
end;

end.
