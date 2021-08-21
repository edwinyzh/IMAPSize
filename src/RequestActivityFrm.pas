{==============================================================================|
| Project : IMAPSize                                                           |
|==============================================================================|
| $Id$
|==============================================================================|
| Copyright (c)2003-2004, Ivan Vecanski                                        |
| =============================================================================}

unit RequestActivityFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, GIFImage, ALProgressBar, StdCtrls, ExtCtrls, Log;

type
  { Defines the various states of the request }
  TRequestActivityStatus = (rasNotInvoked, rasRunning, rasCompleted, rasCompletedWithErrors, rasFailed);


  TRequestActivityFrame = class(TFrame)
    Panel1: TPanel;
    RequestDescriptionLbl: TLabel;
    ALProgressBar1: TALProgressBar;
    GIFImage1: TGIFImage;
    AccountLbl: TLabel;
    SpeedButton1: TSpeedButton;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    Bevel5: TBevel;
    procedure RequestDescriptionLblClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    { Private declarations }
  public
    mRequestId: Integer;
    mPosition: Integer;  // position on the ActivityDlg (count starts from 1)
    procedure SetRequestId(requestId: Integer);
    procedure SetRequestFramePosition(position: Integer);
    procedure SetAccountName(account: String);
    procedure SetRequestDescription(desc: String);
    procedure SetStatusImage(status: TRequestActivityStatus);
    procedure SetProgressBarPos(position: Integer);
  end;

implementation

uses ActivityDlg, GlobalConstants;

{$R *.DFM}

procedure TRequestActivityFrame.SetRequestId(requestId: Integer);
begin
    mRequestId:=requestId;
end;

procedure TRequestActivityFrame.SetRequestFramePosition(position: Integer);
begin
    mPosition:=position;
end;

procedure TRequestActivityFrame.SetAccountName(account: String);
begin
    try
        AccountLbl.Caption:=account;
    except
        // do nothing, leave account label without a caption 
    end;
end;

procedure TRequestActivityFrame.SetRequestDescription(desc: String);
begin
    RequestDescriptionLbl.Caption:=desc;
end;

procedure TRequestActivityFrame.RequestDescriptionLblClick(
  Sender: TObject);
begin
    //@todo Show errors if completed with errors or failed
end;

procedure TRequestActivityFrame.SpeedButton1Click(Sender: TObject);
begin
    PostMessage(FActivityDlg.Handle,WM_ACTIVITY_DLG_REMOVE_REQUEST,mRequestId,0);
end;

{TRequestActivityStatus = (rasNotInvoked, rasRunning, rasCompleted, rasCompletedWithErrors, rasFailed);}
procedure TRequestActivityFrame.SetStatusImage(status: TRequestActivityStatus);
begin
    case status of
        rasNotInvoked: begin GIFImage1.Clear; GIFImage1.Animate:=false; end;
        rasRunning: begin GIFImage1.LoadFromResourceName(HINSTANCE, 'GIF_PROGRESS_BUSY'); GIFImage1.Animate:=true; end;
        rasCompleted: begin GIFImage1.Animate:=false; GIFImage1.LoadFromResourceName(HINSTANCE, 'GIF_PROGRESS_DONE'); end;
        rasCompletedWithErrors: begin GIFImage1.Animate:=false; GIFImage1.LoadFromResourceName(HINSTANCE, 'GIF_PROGRESS_DONE_ERRORS'); end;
        rasFailed: begin GIFImage1.Animate:=false; GIFImage1.LoadFromResourceName(HINSTANCE, 'GIF_PROGRESS_FAILED'); end;
    end;
end;

procedure TRequestActivityFrame.SetProgressBarPos(position: Integer);
begin
    AlProgressBar1.Position:=position;
end;

end.