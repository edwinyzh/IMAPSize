{==============================================================================|
| Project : IMAPSize                                                           |
|==============================================================================|
| $Id: Eml2MboxDlg.pas,v 1.2 2004/03/31 23:27:38 Ivan Exp $
|==============================================================================|
| Copyright 2003-2004 Ivan Vecanski                                            |
| =============================================================================}

unit Eml2MboxDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, GlobalConstants, Converter;

type
  TFEml2MboxDlg = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    Label2: TLabel;
    EdMbox: TEdit;
    SpeedButton1: TSpeedButton;
    CBUnix: TCheckBox;
    BtnAddFiles: TButton;
    BtnRemove: TButton;
    Bevel1: TBevel;
    ScrollBox1: TScrollBox;
    ListBox1: TListBox;
    Label3: TLabel;
    LblNumFiles: TLabel;
    procedure FormActivate(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure BtnRemoveClick(Sender: TObject);
    procedure BtnAddFilesClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    mEmlDir: String;
    mConvertOptions: TConvertOptions;
    procedure CountFiles;
  public
    function GetMboxFilename: String;
    function GetEmlDir: String;
    function GetEmlFiles: TStrings;
    function GetConverterOptions: TConvertOptions;
  end;

var
  FEml2MboxDlg: TFEml2MboxDlg;

implementation

uses Main;

{$R *.DFM}


procedure TFEml2MboxDlg.FormActivate(Sender: TObject);
begin
    EdMbox.Text:=settings.MiscSettings.MboxDir;
    mEmlDir:=settings.MiscSettings.EMLDir;
    mConvertOptions:=GetDefaultConversionOptions;
end;

function TFEml2MboxDlg.GetMboxFilename: String;
begin
    Result:=EdMbox.Text;
end;

function TFEml2MboxDlg.GetEmlDir: String;
begin
    Result:=mEmlDir;
end;

function TFEml2MboxDlg.GetEmlFiles: TStrings;
begin
    Result:=ListBox1.Items;
end;

function TFEml2MboxDlg.GetConverterOptions: TConvertOptions;
begin
    mConvertOptions.UnixLF:=CBUnix.Checked;
    Result:=mConvertOptions;
end;

procedure TFEml2MboxDlg.SpeedButton1Click(Sender: TObject);
begin
    FMain.ULOpenDialog.Title:='Specify a mbox file to convert to';
    FMain.ULOpenDialog.InitialDir:=settings.MiscSettings.MboxDir;
    FMain.ULOpenDialog.Filter := 'Unix mailbox files (*.mbox)|*.MBOX|All files (*.*)|*.*';
    FMain.ULOpenDialog.DefaultExt := 'mbox';
    FMain.ULOpenDialog.Options:= FMain.ULOpenDialog.Options - [ofAllowMultiSelect];
    if FMain.ULOpenDialog.Execute then begin
        EdMbox.Text:=FMain.ULOpenDialog.Filename;
        settings.MiscSettings.MboxDir:=ExtractFilePath(FMain.ULOpenDialog.Filename);
    end;
end;

procedure TFEml2MboxDlg.Button2Click(Sender: TObject);
begin
    ModalResult:=mrCancel;
end;

procedure TFEml2MboxDlg.BtnRemoveClick(Sender: TObject);
var i: Integer;
begin
    if ListBox1.SelCount>0 then begin
        for i:=ListBox1.Items.Count-1 downto 0 do begin
            if ListBox1.Selected[i] then ListBox1.Items.Delete(i);
        end;
        CountFiles;
    end
    else MessageDlg('No files selected for removal',mtInformation,[mbOK],0);
end;

procedure TFEml2MboxDlg.BtnAddFilesClick(Sender: TObject);
var i, MaxWidth: integer;
begin
    FMain.ULOpenDialog.Title:='Select eml files to convert';
    FMain.ULOpenDialog.InitialDir:=settings.MiscSettings.EMLDir;
    FMain.ULOpenDialog.Filter := '*.eml files|*.EML|All files (*.*)|*.*';
    FMain.ULOpenDialog.Options:= FMain.ULOpenDialog.Options + [ofAllowMultiSelect];
    if FMain.ULOpenDialog.Execute then begin
        mEmlDir:=ExtractFilePath(FMain.ULOpenDialog.Files[0]);
        settings.MiscSettings.EMLDir:=mEmlDir;
        ListBox1.Items.AddStrings(FMain.ULOpenDialog.Files);

        // Add a horizontal scrollbar to the listbox
        MaxWidth := 0;
        for i := 0 to ListBox1.Items.Count - 1 do
        if MaxWidth < ListBox1.Canvas.TextWidth(ListBox1.Items.Strings[i]) then
            MaxWidth := ListBox1.Canvas.TextWidth(ListBox1.Items.Strings[i]);
        SendMessage(ListBox1.Handle, LB_SETHORIZONTALEXTENT, MaxWidth+2, 0);
    end;
    CountFiles;
end;

procedure TFEml2MboxDlg.Button1Click(Sender: TObject);
var msgDlgResult: Integer; f: TextFile;
begin
    if ListBox1.Items.Count > 0 then begin

        if FileExists(EdMbox.Text) then begin
            msgDlgResult:= MessageDlg('The mbox file you specified already exists. Do you want to append new emls to this file?'+
                            CRLF+CRLF+
                            'Click YES to append, NO to overwrite, CANCEL to cancel the action',
                            mtInformation,mbYesNoCancel,0);
            if msgDlgResult<>mrCancel then begin
                if msgDlgResult=mrYes then mConvertOptions.overwrite:=false
                else mConvertOptions.overwrite:=true;

                ModalResult:=mrOK;
            end;
        end
        else begin
            // Check if the mbox name is valid
            try
                AssignFile(f,EdMbox.Text);
                Rewrite(f);
                CloseFile(f);
                ModalResult:=mrOK;
            except
                MessageDlg('The mbox filename is invalid. Specify a valid one.',mtWarning,[mbOK],0);
            end;
        end;
    end
    else MessageDlg('You did not select any .eml files',mtInformation,[mbOK],0);
end;

procedure TFEml2MboxDlg.CountFiles;
begin
    LblNumFiles.Caption:=IntToStr(ListBox1.Items.Count);
end;

end.