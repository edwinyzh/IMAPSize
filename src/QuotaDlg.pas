{==============================================================================|
| Project : IMAPSize                                                           |
|==============================================================================|
| $Id: QuotaDlg.pas,v 1.4 2004/03/31 23:27:33 Ivan Exp $
|==============================================================================|
| Copyright 2003-2004 Ivan Vecanski                                            |
| =============================================================================}

unit QuotaDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, ALProgressBar;

type
  TFQuotaDlg = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    Button1: TButton;
    ProgressBar1: TALProgressBar;
    procedure Button1Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure SetValues(accountName: String; current, maximum: Integer);
  end;

var
  FQuotaDlg: TFQuotaDlg;

implementation

{$R *.DFM}

procedure TFQuotaDlg.SetValues(accountName: String; current, maximum: Integer);
var percent: Integer;
begin
    Caption:='Storage Quota for Account '+accountName;
    ProgressBar1.Max:=100;
    if current<maximum then percent:=Round(current/maximum*100)
    else percent:=100;
    ProgressBar1.Position:=percent;
    Label1.Caption:='Storage Quota: '+IntToStr(current)+' of '+IntToStr(maximum);
end;

procedure TFQuotaDlg.Button1Click(Sender: TObject);
begin
    Close;
end;

procedure TFQuotaDlg.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
    if Key=VK_ESCAPE then Close;
end;

end.
