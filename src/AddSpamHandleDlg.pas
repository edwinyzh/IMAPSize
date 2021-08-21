{==============================================================================|
| Project : IMAPSize                                                           |
|==============================================================================|
| $Id: AddSpamHandleDlg.pas,v 1.4 2004/03/31 23:27:40 Ivan Exp $
|==============================================================================|
| Copyright 2003-2004 Ivan Vecanski                                            |
| =============================================================================}

unit AddSpamHandleDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, SpamHandles;

type
  TFAddSpamHandleDlg = class(TForm)
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label3: TLabel;
    Label10: TLabel;
    Edit2: TEdit;
    Edit3: TEdit;
    RadioGroup1: TRadioGroup;
    Edit1: TEdit;
    Button1: TButton;
    Button2: TButton;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    procedure createSpamHandleInfoFromFields(var spamHandle: TSpamHandleInfo);
  public
    procedure getNewSpamHandle(var spamHandle: TSpamHandleInfo);
  end;

var
  FAddSpamHandleDlg: TFAddSpamHandleDlg;

implementation

{$R *.DFM}

procedure TFAddSpamHandleDlg.Button2Click(Sender: TObject);
begin
    ModalResult:=mrCancel;
end;

procedure TFAddSpamHandleDlg.getNewSpamHandle(var spamHandle: TSpamHandleInfo);
begin
    createSpamHandleInfoFromFields(spamHandle);
end;

procedure TFAddSpamHandleDlg.createSpamHandleInfoFromFields(var spamHandle: TSpamHandleInfo);
begin
    spamHandle.name:=Edit1.Text;
    spamHandle.SpamHeader:=Edit2.Text;
    spamHandle.SpamComparedBy:=RadioGroup1.ItemIndex;
    spamHandle.CompareValue:=Edit3.Text;
end;

procedure TFAddSpamHandleDlg.Button1Click(Sender: TObject);
begin
   if ((Edit1.Text='') or (Edit2.Text='') or (Edit3.Text='') or (RadioGroup1.ItemIndex=-1)) then begin
        MessageDlg('You have to specify values for all fields. Press "Cancel" if you want to quit the handle creation.',mtInformation,[mbOK],0);
   end
   else ModalResult:=mrOK;
end;

procedure TFAddSpamHandleDlg.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
    if Key=VK_ESCAPE then ModalResult:=mrCancel;
end;

end.
