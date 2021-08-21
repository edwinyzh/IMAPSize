unit PasswordDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TFPasswordDlg = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    CBSavePassword: TCheckBox;
    EditPassword: TEdit;
    Label2: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  public
    procedure SetAccountName(name: String);
    function GetPassword: String;
    function SavePassword: Boolean;
  end;

var
  FPasswordDlg: TFPasswordDlg;

implementation

{$R *.DFM}

procedure TFPasswordDlg.SetAccountName(name: String);
begin
    Label2.Caption:=name;
end;

function TFPasswordDlg.GetPassword: String;
begin
    Result:=EditPassword.Text;
end;

function TFPasswordDlg.SavePassword: Boolean;
begin
    Result:=CBSavePassword.Checked;
end;

procedure TFPasswordDlg.Button1Click(Sender: TObject);
begin
    if EditPassword.Text<>'' then ModalResult:=mrOK
    else MessageDlg('Please enter the password or click Cancel',mtInformation,[mbOK],0);
end;

procedure TFPasswordDlg.FormActivate(Sender: TObject);
begin
    EditPassword.SetFocus;
end;

end.
