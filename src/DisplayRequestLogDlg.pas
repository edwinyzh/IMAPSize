unit DisplayRequestLogDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TFDisplayRequestLogDlg = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Memo1: TMemo;
    CBErrorsOnly: TCheckBox;
    Button1: TButton;
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CBErrorsOnlyClick(Sender: TObject);
  private
    log: TStringList;
    errors: TStringList;
  public
    procedure setLogs(var inLogs, inErrors: TStringList);
    procedure PopulateMemo;
  end;

var
  FDisplayRequestLogDlg: TFDisplayRequestLogDlg;

implementation

{$R *.DFM}

procedure TFDisplayRequestLogDlg.FormKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
    if Key=VK_ESCAPE then ModalResult:=mrOK;
end;

procedure TFDisplayRequestLogDlg.FormCreate(Sender: TObject);
begin
    errors:=TStringList.Create;
    log:=TStringList.Create;
end;

procedure TFDisplayRequestLogDlg.FormDestroy(Sender: TObject);
begin
    errors.Free;
    log.Free;
end;

{ Call this to set the internal lists before you show the dialog }
procedure TFDisplayRequestLogDlg.setLogs(var inLogs, inErrors: TStringList);
var i: Integer;
begin
    for i:=0 to inLogs.Count-1 do log.Add(inLogs.Strings[i]);
    for i:=0 to inErrors.Count-1 do errors.Add(inErrors.Strings[i]);
    CBErrorsOnly.Enabled:=(errors.Count>0);
end;

{ Populates the memo box with adecuate content, depending on the 'Show Errors Only' checkbox }
procedure TFDisplayRequestLogDlg.PopulateMemo;
begin
    Memo1.Lines.Clear;
    if CBErrorsOnly.Checked then Memo1.Lines:=errors
    else Memo1.Lines:=log;
end;

procedure TFDisplayRequestLogDlg.CBErrorsOnlyClick(Sender: TObject);
begin
    PopulateMemo;
end;

end.