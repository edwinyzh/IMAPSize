{==============================================================================|
| Project : IMAPSize                                                           |
|==============================================================================|
| $Id: MailboxChooser.pas,v 1.4 2004/03/31 23:27:36 Ivan Exp $
|==============================================================================|
| Copyright 2003-2004 Ivan Vecanski                                            |
| =============================================================================}

unit DestinationChooser;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Accounts, GlobalConstants, VecLog, Log;

type

  TFDestinationChooserDlg = class(TForm)
    Panel1: TPanel;
    ListBox1: TListBox;
    Panel2: TPanel;
    Button1: TButton;
    Button2: TButton;
    Panel3: TPanel;
    ComboBox1: TComboBox;
    Label1: TLabel;
    Button3: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ComboBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    mOriginalAccountIndex: Integer;  // Index of the account first displayed as a destination
    ActiveDestinationString: String;
    function SetActiveAccount: Boolean;
    procedure SetDestinationString;
  public
    HasData: Boolean;
    DestinationAccountPtr: PAccountInfo;  // Pointer to the destination account
    procedure InitializeDestination(initialIndex: Integer);
    //procedure ReloadMailboxes;
    procedure SetSelectedMailboxIndex(mboxIndex: Integer);
    function GetSelectedMboxIndex: Integer;
    function GetFullDestinationFolderName: String;
    function GetDestinationString: String;
    procedure HandleRequestOver(var Message: TMessage); message WM_REQUEST_OVER;
  end;

var
  FDestinationChooserDlg: TFDestinationChooserDlg;

implementation

uses Main, RequestManager, CustomThread, IMAPWorker;

{$R *.DFM}

procedure TFDestinationChooserDlg.FormCreate(Sender: TObject);
begin
    New(DestinationAccountPtr);
    HasData:=false;
end;

procedure TFDestinationChooserDlg.FormDestroy(Sender: TObject);
begin
    Dispose(DestinationAccountPtr);
end;

{ Invoked from FMain whenever an account is loaded }
procedure TFDestinationChooserDlg.InitializeDestination(initialIndex: Integer);
var i: Integer;
begin
    // Populate the account combo with values from FMain.ComboBox1
    ComboBox1.Items.Clear;
    for i:=0 to FMain.ComboBox1.Items.Count-1 do begin
        ComboBox1.Items.Add(FMain.ComboBox1.Items[i]);
    end;
    ComboBox1.ItemIndex:=FMain.ComboBox1.ItemIndex;
    mOriginalAccountIndex:=ComboBox1.ItemIndex;
    SetActiveAccount;

    ListBox1.Items.Clear;
    for i:=1 to mboxTree.Count-1 do begin
        ListBox1.Items.Add(mboxTree.Items[i].mFullDisplayedName);
    end;
    ListBox1.ItemIndex:=initialIndex;

    SetDestinationString;
    HasData:=true;
end;

{
procedure TFDestinationChooserDlg.ReloadMailboxes;
var i: Integer;
begin
    ListBox1.Items.Clear;
    for i:=1 to mboxTree.Count-1 do begin
        ListBox1.Items.Add(mboxTree.Items[i].mFullDisplayedName);
    end;
end;
}

procedure TFDestinationChooserDlg.SetSelectedMailboxIndex(mboxIndex: Integer);
begin
    if mboxIndex<ListBox1.Items.Count then
        ListBox1.ItemIndex:=mboxIndex-1;
end;

function TFDestinationChooserDlg.GetSelectedMboxIndex: Integer;
begin
    Result:=ListBox1.ItemIndex+1;
end;

function TFDestinationChooserDlg.GetDestinationString: String;
begin
    Result:=ActiveDestinationString;
end;

function TFDestinationChooserDlg.GetFullDestinationFolderName: String;
begin
    Result:=ListBox1.Items[ListBox1.ItemIndex];
end;

procedure TFDestinationChooserDlg.Button1Click(Sender: TObject);
begin
    if ListBox1.ItemIndex<>-1 then begin
        SetDestinationString;
        ModalResult:=mrOK;
    end
    else MessageDlg('Please choose a destination mailbox or click the Cancel button',mtInformation,[mbOK],0);
end;

procedure TFDestinationChooserDlg.Button2Click(Sender: TObject);
begin
    ModalResult:=mrCancel;
end;

procedure TFDestinationChooserDlg.ListBox1DblClick(Sender: TObject);
begin
    SetDestinationString;
    ModalResult:=mrOK;
end;

{ Sets the destination string, called internally when a mailbox is selected }
procedure TFDestinationChooserDlg.SetDestinationString;
begin
    ActiveDestinationString:=ListBox1.Items[ListBox1.ItemIndex]+' @ '+ComboBox1.Items[ComboBox1.ItemIndex];
end;

procedure TFDestinationChooserDlg.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
    if Key=VK_ESCAPE then ModalResult:=mrCancel
    else if Key=VK_RETURN then ModalResult:=mrOK;
end;

procedure TFDestinationChooserDlg.ComboBox1Change(Sender: TObject);
var requestPtr: PRequest;
begin
    if (ComboBox1.ItemIndex<>mOriginalAccountIndex) then begin
        if SetActiveAccount then begin
            New(requestPtr);
            requestPtr^ := TRequest.Create(thtIMAPWorker);
            requestPtr^.requestType:=rtGetFolderList;
            requestPtr^.requestComponent:=rcDestinationChooser;
            requestPtr^.imapOperation:=iopGetFolderList;
            requestPtr^.accountInfoPtr:=DestinationAccountPtr;
            requestMgr.InvokeRequest(requestPtr);
        end;
        // else there are no defined accounts...
    end;
end;

{
  Sets the DestinationAccountPtr pointer to point to the account
  that is currently active in ComboBox1.
  If no accounts are selected returns false }
function TFDestinationChooserDlg.SetActiveAccount: Boolean;
var accountName: String;
begin
    Result:=false;
    if (ComboBox1.ItemIndex <> -1) then begin
        accountName := ComboBox1.Items[ComboBox1.ItemIndex];
        DestinationAccountPtr^ := settings.Accounts.getAccount(accountName);
        Result:=true;
    end;
end;

{ Handles messages sent by the request manager when a request is over }
procedure TFDestinationChooserDlg.HandleRequestOver(var Message: TMessage);
var i,requestID: Integer; success: Integer; request: TRequest;
begin
    devLog.Trace('TFGlobalSearch.HandleRequestOver');
    requestId:=Message.WParam;
    try
        request := requestMgr.GetRequest(requestId);
        success:=Message.LParam;
        if request.requestType = rtGetFolderList then begin
            if success=1 then begin
                // Load list with retrieved folders
                ListBox1.Items.Clear;
                for i:=0 to request.data.msgContent.Count-1 do begin
                    ListBox1.Items.Add(request.data.msgContent.Strings[i]);
                end;
                mOriginalAccountIndex:=ComboBox1.ItemIndex;
            end
            else begin
                ComboBox1.ItemIndex:=mOriginalAccountIndex;
            end;
        end;
    finally
        requestMgr.RemoveRequest(requestId, false);
    end;
end;

procedure TFDestinationChooserDlg.Button3Click(Sender: TObject);
var i: Integer;
begin
    if FMain.pActiveAccount^.Name=ComboBox1.Items[ComboBox1.ItemIndex] then begin
        // If the account whose data is currently held in mbox is shown
        ListBox1.Items.Clear;
        for i:=1 to mboxTree.Count-1 do begin
            ListBox1.Items.Add(mboxTree.Items[i].mFullDisplayedName);
        end;
    end
    else begin
        // If another ("remote") account is shown
        ComboBox1Change(Self);
    end;
end;

end.