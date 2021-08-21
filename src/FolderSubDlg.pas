unit FolderSubDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, CheckLst, ExtCtrls, Log, VecLog, GlobalConstants, Accounts;

type
  TFFolderSubDlg = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Label1: TLabel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button5: TButton;
    BtnApply: TButton;
    CheckListBox1: TCheckListBox;
    LblStatus: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure CheckListBox1ClickCheck(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtnApplyClick(Sender: TObject);
  private
    fAccountName: String;
    fOriginalSubscriptions: TStringList;  // stores the original subscribtions
    fSubResponseCnt: Integer;
    procedure InvokeGetSubscribedFolders;
    procedure HandleRequestOver(var Message: TMessage); message WM_REQUEST_OVER;
    function  NumChecked: Integer;
    procedure SelectAll(select: Boolean);
    procedure EnableApplyButton(enable: Boolean);
  public
    procedure PrepareMailboxList;
  end;

var
  FFolderSubDlg: TFFolderSubDlg;

implementation

uses Main, RequestManager, MailboxTree, CustomThread, CheckerThread, IMAPWorker, MyUtil;

{$R *.DFM}

procedure TFFolderSubDlg.FormCreate(Sender: TObject);
begin
    fOriginalSubscriptions:= TStringList.Create;
    LblStatus.Caption:='';
end;

procedure TFFolderSubDlg.FormDestroy(Sender: TObject);
begin
    fOriginalSubscriptions.Free;
end;

{ Fills (or invokes filling from remote server) of the mailbox list }
procedure TFFolderSubDlg.PrepareMailboxList;
var requestPtr: PRequest;
begin
    LblStatus.Caption:='Getting folder list...';
    New(requestPtr);
    requestPtr^ := TRequest.Create(thtIMAPWorker);
    requestPtr^.requestType:=rtGetFolderList;
    requestPtr^.requestComponent:=rcFolderSubs;
    requestPtr^.imapOperation:=iopGetFolderList;
    requestPtr^.accountInfoPtr:=FMain.pActiveAccount;
    requestMgr.InvokeRequest(requestPtr);
end;

procedure TFFolderSubDlg.InvokeGetSubscribedFolders;
var requestPtr: PRequest;
begin
    LblStatus.Caption:='Getting subscribed folders...';
    New(requestPtr);
    requestPtr^ := TRequest.Create(thtIMAPWorker);
    requestPtr^.requestType:=rtGetSubscribedFolderList;
    requestPtr^.requestComponent:=rcFolderSubs;
    requestPtr^.imapOperation:=iopGetSubscribedFolderList;
    requestPtr^.accountInfoPtr:=FMain.pActiveAccount;
    requestMgr.InvokeRequest(requestPtr);
end;

{ Handles messages sent by the request manager when a request is over }
procedure TFFolderSubDlg.HandleRequestOver(var Message: TMessage);
var requestID, i: Integer; success: Integer; request: TRequest;
begin
    devLog.Trace('TFFolderSubDlg.HandleRequestOver');
    requestId:=Message.WParam;
    try
        request := requestMgr.GetRequest(requestId);
        success:=Message.LParam;
        if request.requestType = rtGetFolderList then begin
            if success=1 then begin
                InvokeGetSubscribedFolders;
                CheckListBox1.Clear;
                for i:=0 to request.data.msgContent.Count-1 do begin
                    CheckListBox1.Items.Add(request.data.msgContent[i]);
                end;
            end;
        end
        else if request.requestType = rtGetSubscribedFolderList then begin
            // This request *always* comes after rtGetFolderList
            if success=1 then begin
                for i:=0 to request.data.msgContent.Count-1 do begin
                    fOriginalSubscriptions.Add(request.data.msgContent[i]);
                end;
                for i:=0 to CheckListBox1.Items.Count-1 do begin
                    if fOriginalSubscriptions.IndexOf(CheckListBox1.Items[i])>-1 then
                        CheckListBox1.Checked[i]:=true;
                end;
            end;
            LblStatus.Caption:='';
        end
        else if request.requestType = rtSubscribe then begin
            fOriginalSubscriptions.Clear;
            EnableApplyButton(false);
            PrepareMailboxList;
        end;
    finally
        requestMgr.RemoveRequest(requestId, false);
    end;
end;

{ Returns number of checked items in the check box list }
function TFFolderSubDlg.NumChecked: Integer;
var count, i: Integer;
begin
    count:=0;
    for i:=0 to CheckListBox1.Items.Count-1 do begin
        if CheckListBox1.Checked[i] then Inc(count);
    end;
    Result:=count;
end;

procedure TFFolderSubDlg.SelectAll(select: Boolean);
var i: Integer;
begin
    for i:=0 to CheckListBox1.Items.Count-1 do begin
        CheckListBox1.Checked[i]:=select;
    end;
end;

procedure TFFolderSubDlg.Button1Click(Sender: TObject);
begin
    SelectAll(true);
    EnableApplyButton(true);
end;

procedure TFFolderSubDlg.Button2Click(Sender: TObject);
begin
    SelectAll(false);
    EnableApplyButton(true);
end;

procedure TFFolderSubDlg.Button3Click(Sender: TObject);
begin
    PrepareMailboxList;
end;

procedure TFFolderSubDlg.CheckListBox1ClickCheck(Sender: TObject);
begin
    EnableApplyButton(true);
end;

procedure TFFolderSubDlg.FormActivate(Sender: TObject);
begin
    EnableApplyButton(false);
end;

procedure TFFolderSubDlg.EnableApplyButton(enable: Boolean);
begin
    BtnApply.Enabled:=enable;
end;

procedure TFFolderSubDlg.BtnApplyClick(Sender: TObject);
var i: Integer; requestPtr: PRequest; subPair: TSubscriptionPair;
begin
    // Save subscription changes
    // Go through the orginal subscription list and compare with the current state
    LblStatus.Caption:='Applying changes...';
    New(requestPtr);
    requestPtr^ := TRequest.Create(thtIMAPWorker);
    requestPtr^.requestType:=rtSubscribe;
    requestPtr^.requestComponent:=rcFolderSubs;
    requestPtr^.imapOperation:=iopSubscribeFolders;
    requestPtr^.accountInfoPtr:=FMain.pActiveAccount;
    try
        for i:=0 to CheckListBox1.Items.Count-1 do begin
            if fOriginalSubscriptions.IndexOf(CheckListBox1.Items[i])>-1 then begin
                // Folder was originally subscribed
                if (not CheckListBox1.Checked[i]) then begin
                    subPair:=TSubscriptionPair.Create(CheckListBox1.Items[i],false);  // unsubscribe
                    requestPtr^.data.generalList.Add(subPair);
                end;
            end
            else begin
                if CheckListBox1.Checked[i] then begin
                    subPair:=TSubscriptionPair.Create(CheckListBox1.Items[i],true);  // subscribe
                    requestPtr^.data.generalList.Add(subPair);
                end;
            end;
        end;
        requestMgr.InvokeRequest(requestPtr);
    except
        devLog.Warn('Folder subscription invocation failed. Removing request');
        requestMgr.RemoveRequest(requestPtr^.requestId, false);
    end;
end;

end.
