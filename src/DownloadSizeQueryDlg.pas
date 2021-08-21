unit DownloadSizeQueryDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, Spin, ExTrackBar;

const
    mrGetSpecified = 101;
    mrGetFull = 102;

type
  TFDownloadSizeQueryDlg = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    Button3: TButton;
    Label2: TLabel;
    Label3: TLabel;
    SpinEdit1: TSpinEdit;
    TrackBar1: TExTrackBar;
    procedure TrackBar1Change(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
  private
    mMax: Integer;
    mSpecified: Integer;
  public
    procedure InitializeValues(max, specified: Integer);
    function GetSpecified: Integer;
  end;

var
  FDownloadSizeQueryDlg: TFDownloadSizeQueryDlg;

implementation

{$R *.DFM}

procedure TFDownloadSizeQueryDlg.InitializeValues(max, specified: Integer);
begin
    mMax:=max;
    mSpecified:=specified;

    TrackBar1.Max:=mMax;
    TrackBar1.Position:=mSpecified;
    TrackBar1.PageSize:=Round(mMax/10);
    TrackBar1.Frequency:=Round(mMax/10);
    TrackBar1.LabelsInterval:=Round(mMax/2);

    SpinEdit1.MaxValue:=mMax;
    SpinEdit1.Increment:=Round(mMax/10);
    SpinEdit1.Value:=mSpecified;
end;

function TFDownloadSizeQueryDlg.GetSpecified: Integer;
begin
    Result:=SpinEdit1.Value;
end;

procedure TFDownloadSizeQueryDlg.TrackBar1Change(Sender: TObject);
begin
    SpinEdit1.Value:=TrackBar1.Position;
end;

procedure TFDownloadSizeQueryDlg.SpinEdit1Change(Sender: TObject);
begin
    TrackBar1.Position:=SpinEdit1.Value;
end;

procedure TFDownloadSizeQueryDlg.Button1Click(Sender: TObject);
begin
    ModalResult:=mrGetSpecified;
end;

procedure TFDownloadSizeQueryDlg.Button3Click(Sender: TObject);
begin
    ModalResult:=mrGetFull;
end;

end.
