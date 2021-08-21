{==============================================================================|
| Project : IMAPSize                                                           |
|==============================================================================|
| $Id$
|==============================================================================|
| Copyright (c)2003-2004, Ivan Vecanski                                        |
| =============================================================================}

unit ActivityDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  RequestActivityFrm, ExtCtrls, StdCtrls, VecLog, Log, GlobalConstants;

const
    FRAME_HORZ_STEP = 21;

type
  TFActivityDlg = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    ScrollBox1: TScrollBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    requestFrames: TList;
    cs: TRTLCriticalSection;
    function FindIDByRequestID(requestId: Integer): Integer;
    function GetRequestDescription(requestId: Integer): String;
  public
    procedure AddRequest(requestId: Integer);
    procedure RemoveRequest(requestId: Integer);
    procedure SetActivityImage(requestId: Integer; ras: TRequestActivityStatus);
    procedure UpdateProgress(requestId, Position: Integer);
    procedure HandleRemoveRequest(var Message: TMessage); message WM_ACTIVITY_DLG_REMOVE_REQUEST;
  end;

var
  FActivityDlg: TFActivityDlg;

implementation

uses Main, RequestManager, Accounts, CustomThread;

{$R *.DFM}

procedure TFActivityDlg.FormCreate(Sender: TObject);
begin
    requestFrames:=TList.Create;
    InitializeCriticalSection(cs);
end;

procedure TFActivityDlg.FormDestroy(Sender: TObject);
var i: Integer;
begin
    DeleteCriticalSection(cs);
    for i:=0 to requestFrames.Count-1 do begin
        TRequestActivityFrame(requestFrames[i]).Free;
    end;
    requestFrames.Free;
end;

procedure TFActivityDlg.AddRequest(requestId: Integer);
var Fr: TRequestActivityFrame; num: Integer; accName: String;
begin
    EnterCriticalSection(cs);
    num:=requestFrames.Count;
    Fr := TRequestActivityFrame.Create(Self);
    Fr.Parent := ScrollBox1;
    Fr.Left:=0;
    Fr.Top:=num*FRAME_HORZ_STEP;
    Fr.Name:='Frm'+IntToStr(requestId);
    Fr.SetRequestID(requestId);
    accName:='';
    if (TRequest(requestMgr.GetRequest(requestId)).threadType<>thtConverter) then begin
        try
            accName:=TRequest(requestMgr.GetRequest(requestId)).accountInfoPtr^.Name;
        except
            // do nothing, leave as empty string
        end;
    end;
    Fr.SetAccountName(accName);
    Fr.SetRequestDescription(GetRequestDescription(requestId));
    Fr.SetRequestFramePosition(num);
    Fr.SetStatusImage(rasNotInvoked);
    requestFrames.Add(Fr);
    LeaveCriticalSection(cs);
end;

procedure TFActivityDlg.RemoveRequest(requestId: Integer);
var id,i: Integer; comp: TComponent; pos, t: Integer;
begin
    id:= FindIDByRequestID(requestId);
    EnterCriticalSection(cs);
    if (id<>-1) then begin
        name := TRequestActivityFrame(requestFrames[id]).Name;
        pos:=TRequestActivityFrame(requestFrames[id]).mPosition;
        TRequestActivityFrame(requestFrames[id]).Free;
        requestFrames.Delete(id);
        // Move all subsequent requests one position up
        for i:=pos to requestFrames.Count-1 do begin
            TRequestActivityFrame(requestFrames[i]).mPosition:=TRequestActivityFrame(requestFrames[i]).mPosition-1;
            TRequestActivityFrame(requestFrames[i]).Top:=TRequestActivityFrame(requestFrames[i]).Top-FRAME_HORZ_STEP;
        end;
        Repaint;
    end;
    LeaveCriticalSection(cs);
end;

{ Returns the id of the request in the requestFrames list
  for the specified requestId. If the request is not found,
  returns -1 }
function TFActivityDlg.FindIDByRequestID(requestId: Integer): Integer;
var found: Boolean; i: Integer;
begin
    Result:=-1;
    found:=false; i:=0;
    while (not found) and (i<requestFrames.Count) do begin
        if TRequestActivityFrame(requestFrames[i]).mRequestId=requestId then found:=true
        else Inc(i);
    end;
    if found then Result:=i;
end;

procedure TFActivityDlg.SetActivityImage(requestId: Integer; ras: TRequestActivityStatus);
var id: Integer;
begin
    id:= FindIDByRequestID(requestId);
    if id<>-1 then TRequestActivityFrame(requestFrames[id]).SetStatusImage(ras);
end;

{ Updates the progress bar of the specified request }
procedure TFActivityDlg.UpdateProgress(requestId, Position: Integer);
var id: Integer;
begin
    try
        id:= FindIDByRequestID(requestId);
        TRequestActivityFrame(requestFrames[id]).SetProgressBarPos(Position);
    except
        on E:Exception do devLog.Error('TFActivityDlg.UpdateProgress: Unable to update progress bar. Error is: '+E.Message);
    end;
end;

{ Handles message WM_ACTIVITY_DLG_REMOVE_REQUEST }
{ Asyncronous wrapper around RemoveRequest - this is to be used when
  a removal is requested from within the RequestActivityFrm itself }
procedure TFActivityDlg.HandleRemoveRequest(var Message: TMessage);
var requestId: Integer;
begin
    requestId:=Message.WParam;
    RemoveRequest(requestId);
end;

function TFActivityDlg.GetRequestDescription(requestId: Integer): String;
begin
    Result:='Unknown';
    case TRequest(requestMgr.GetRequest(requestId)).requestType of
        rtLogin: Result:='Login';
        rtLogout: Result:='Logout';
        rtCheckSize: Result:='Check Size';
        rtGetQuota: Result:='Get Quota';
        rtGetFolderList: Result:='Getting folders';
        rtGetSubscribedFolderList: Result:='Getting subscribed';
        rtGeneralIMAP: Result:='General IMAP';
        rtMessageIMAP: Result:='Message IMAP';
        rtSearch: Result:='Search';
        rtGlobalSearch: Result:='Global Search';
        rtCopy: Result:='Copy Message';
        rtRename: Result:='Rename Mailbox';
        rtSaveHeaders: Result:='Save Headers';
        rtAppend: Result:='Append Message';
        rtCreateMailbox: Result:='Create Mailbox';
        rtFetch: Result:='Fetch Message';
        rtFetchWholeBody, rtFetchWholeBodyAsPrimaryText, rtFetchBodyPart, rtFetchMultipleBodyParts: Result:='Fetch Body';
        rtFetchHeader,rtFetchHeaderForQSDeletion,rtFetchHeaderForRemoteSaving,rtFetchHeaderForLocalSaving: Result:='Fetch Header';
        rtFetchRawMessage: Result:='Fetch Raw';
        rtFetchMultipleMsgs: Result:='Multiple Fetch';
        rtUploadMbox: Result:='Upload mbox';
        rtUploadEmls: Result:='Upload EMLs';
        rtDownloadMbox: Result:='Download mbox';
        rtDownloadEmls: Result:='Download EMLs';
        rtEml2Mbox: Result:='eml2mbox';
        rtMbox2Eml: Result:='mbox2eml';
        rtMultipleQSDeletion: Result:='Attachment deletion';
        rtUploadEMLsToRemoteServer: Result:='Copy (upload)';
        rtDownloadForRemoteCopy: Result:='Copy (download)';
        rtSaveAttachment: Result:='Save Attachment';
        rtAccountBackup: Result:='Backup';
    end;
end;


end.