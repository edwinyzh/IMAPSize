unit IMAPConsole;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, GlobalConstants, ComCtrls, Buttons;

type
  TFIMAPConsole = class(TForm)
    Panel1: TPanel;
    Memo1: TMemo;
    Panel2: TPanel;
    Panel3: TPanel;
    Edit1: TEdit;
    SpeedButton1: TSpeedButton;
    StatusBar1: TStatusBar;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FIMAPConsole: TFIMAPConsole;

implementation

{$R *.DFM}

procedure TFIMAPConsole.FormCreate(Sender: TObject);
begin
    Memo1.Lines[0]:='|========================================|';
    Memo1.Lines[1]:='|  Welcome to the IMAPSize IMAP Console  |';
    Memo1.Lines[2]:='|========================================|';
end;

end.
