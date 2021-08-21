unit Eml2MboxesDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, AppSettings, Converter, FileCtrl,
  PBFolderDialog;

type
  TFEml2MboxesDlg = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    EditEmlDir: TEdit;
    Label3: TLabel;
    EditMboxDir: TEdit;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    Button1: TButton;
    Button2: TButton;
    CBUnix: TCheckBox;
    DirDialog1: TPBFolderDialog;
    DirDialog2: TPBFolderDialog;
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    mConvertOptions: TConvertOptions;
  public
    function GetEmlDir: String;
    function GetMboxDir: String;
    function GetConverterOptions: TConvertOptions;
  end;

var
  FEml2MboxesDlg: TFEml2MboxesDlg;

implementation

uses Main, Log;

{$R *.DFM}

procedure TFEml2MboxesDlg.FormActivate(Sender: TObject);
begin
    EditEmlDir.Text:=settings.MiscSettings.EMLDir;
    EditMboxDir.Text:=settings.MiscSettings.MboxDir;
    mConvertOptions:=GetDefaultConversionOptions;
end;

procedure TFEml2MboxesDlg.SpeedButton1Click(Sender: TObject);
begin
    DirDialog1.Folder:=settings.MiscSettings.EMLDir;
    if DirDialog1.Execute then begin
        EditEmlDir.Text:=DirDialog1.Folder;
        settings.MiscSettings.EMLDir:=EditEmlDir.Text;
    end;
end;

procedure TFEml2MboxesDlg.SpeedButton2Click(Sender: TObject);
begin
    DirDialog2.Folder:=settings.MiscSettings.MboxDir;
    if DirDialog2.Execute then begin
        EditMboxDir.Text:=DirDialog2.Folder;
        settings.MiscSettings.MboxDir:=EditMboxDir.Text;
    end;
end;

function TFEml2MboxesDlg.GetEmlDir: String;
begin
    Result:=EditEmlDir.Text;
end;

function TFEml2MboxesDlg.GetMboxDir: String;
begin
    Result:=EditMboxDir.Text;
end;

function TFEml2MboxesDlg.GetConverterOptions: TConvertOptions;
begin
    mConvertOptions.UnixLF:=CBUnix.Checked;
    Result:=mConvertOptions;
end;

procedure TFEml2MboxesDlg.Button1Click(Sender: TObject);
begin
    if not DirectoryExists(EditEmlDir.Text) then
        MessageDlg('The folder with eml files you specified does not exist',mtError,[mbOK],0)
    else if not DirectoryExists(EditMboxDir.Text) then begin
        if MessageDlg('The folder for generated mbox files you specified does not exist. Create it?',mtConfirmation,[mbYes,mbCancel],0)=mrYes then begin
            if ForceDirectories(EditMboxDir.Text) then begin
                settings.MiscSettings.MboxDir:=EditMboxDir.Text;
                ModalResult:=mrOK;
            end
            else MessageDlg('Unable to create directory '+EditMboxDir.Text+'. Please specify a valid directory',mtInformation,[mbOk],0);
        end;
    end
    else begin
        settings.MiscSettings.MboxDir:=EditMboxDir.Text;
        settings.MiscSettings.EMLDir:=EditEMLDir.Text;
        ModalResult:=mrOK;
    end;
end;

end.
