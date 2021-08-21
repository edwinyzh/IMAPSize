{==============================================================================|
| Project : IMAPSize                                                           |
|==============================================================================|
| $Id: NewMailboxDlg.pas,v 1.4 2004/03/31 23:27:33 Ivan Exp $
|==============================================================================|
| Copyright 2003-2004 Ivan Vecanski                                            |
| =============================================================================}

unit NewMailboxDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TFNewMailboxDlg = class(TForm)
    Edit1: TEdit;
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure SetInitialName(name: String);
  end;

var
  FNewMailboxDlg: TFNewMailboxDlg;

implementation

{$R *.DFM}

procedure TFNewMailboxDlg.SetInitialName(name: String);
begin
    Edit1.Text:=name;
end;

procedure TFNewMailboxDlg.Button1Click(Sender: TObject);
begin
    if Length(Edit1.Text)>0 then ModalResult:=mrOK
    else MessageDlg('Mailbox name is not specified',mtWarning,[mbOK],0);
end;

procedure TFNewMailboxDlg.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
    if Key=VK_ESCAPE then ModalResult:=mrCancel;
end;

end.
