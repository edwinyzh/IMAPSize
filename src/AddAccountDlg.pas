{==============================================================================|
| Project : IMAPSize                                                           |
|==============================================================================|
| $Id: AddAccountDlg.pas,v 1.5 2004/03/31 23:27:40 Ivan Exp $
|==============================================================================|
| Copyright 2003-2004 Ivan Vecanski                                            |
| =============================================================================}

unit AddAccountDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Accounts, EncryptIt;

const
    DEFAULT_IMAP_PORT = 143;
    DEFAULT_IMAPS_PORT = 993;

type
  TFAddAccountDlg = class(TForm)
    Panel2: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Button1: TButton;
    Button2: TButton;
    CheckBox1: TCheckBox;
    Label6: TLabel;
    ComboBox1: TComboBox;
    CheckBox2: TCheckBox;
    CBSavePassword: TCheckBox;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure CheckBox1Click(Sender: TObject);
  private
    procedure createAccountInfoFromFields(var account: TAccountInfo);
  public
    procedure getNewAccount(var account: TAccountInfo);
  end;

var
  FAddAccountDlg: TFAddAccountDlg;

implementation

uses Main, Log;

{$R *.DFM}

procedure TFAddAccountDlg.Button2Click(Sender: TObject);
begin
    ModalResult:=mrCancel;
end;

procedure TFAddAccountDlg.getNewAccount(var account: TAccountInfo);
begin
    createAccountInfoFromFields(account);
end;

procedure TFAddAccountDlg.createAccountInfoFromFields(var account: TAccountInfo);
begin
    account.name:=Edit5.Text;
    account.Username:=Edit1.Text;
    account.SavePassword:=CBSavePassword.Checked;
    if account.SavePassword then
        account.Password:=Encrypt(Edit2.Text)
    else
        account.Password:='';
    account.IMAPServer:=Edit3.Text;
    try
        account.Port:=StrToInt(Edit4.Text);
    except
        // If the port box was deleted, set it to the default one
        on EConvertError do account.Port:=DEFAULT_IMAP_PORT;
    end;
    account.SSL:=CheckBox1.Checked;
    account.SmartInbox:=CheckBox2.Checked;
    if ComboBox1.ItemIndex=-1 then account.SpamHandle:=0    // Make sure it is not -1
    else account.SpamHandle:=ComboBox1.ItemIndex;
end;

procedure TFAddAccountDlg.Button1Click(Sender: TObject);
begin
   if ((Edit1.Text='') or (Edit2.Text='') or (Edit3.Text='') or (Edit4.Text='') or (Edit5.Text='')) then begin
        MessageDlg('You have to specify values for all fields. Press "Cancel" if you want to quit the account creation.',mtInformation,[mbOK],0);
   end
   else ModalResult:=mrOK;
end;

procedure TFAddAccountDlg.FormCreate(Sender: TObject);
var i: Integer;
begin
    ComboBox1.Items.Add('None');
    for i:=0 to settings.SpamHandles.SpamHandleItems.Count-1 do begin
        ComboBox1.Items.Add(settings.SpamHandles.GetSpamHandle(i).Name);
    end;
    ComboBox1.ItemIndex:=0; // Defaults to None
end;

procedure TFAddAccountDlg.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
    if Key=VK_ESCAPE then ModalResult:=mrCancel;
end;

procedure TFAddAccountDlg.CheckBox1Click(Sender: TObject);
begin
    if CheckBox1.Checked then Edit4.Text:=IntToStr(DEFAULT_IMAPS_PORT)
    else Edit4.Text:=IntToStr(DEFAULT_IMAP_PORT);
end;

end.