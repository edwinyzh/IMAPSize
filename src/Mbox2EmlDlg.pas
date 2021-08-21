{==============================================================================|
| Project : IMAPSize                                                           |
|==============================================================================|
| $Id: Mbox2EmlDlg.pas,v 1.2 2004/03/31 23:27:34 Ivan Exp $
|==============================================================================|
| Copyright 2003-2004 Ivan Vecanski                                            |
| =============================================================================}

unit Mbox2EmlDlg;

interface


uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, StdCtrls, ExtCtrls, FileCtrl, PBFolderDialog, Grids, VecLog,
  Menus;

type
  TFMbox2EmlDlg = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Button1: TButton;
    Button2: TButton;
    DirDialog1: TPBFolderDialog;
    Label3: TLabel;
    Label2: TLabel;
    EdEmlDir: TEdit;
    SpeedButton2: TSpeedButton;
    Memo1: TMemo;
    Button4: TButton;
    Button3: TButton;
    CBDotsToFolders: TCheckBox;
    procedure FormActivate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    mboxFiles: TStringList;
    emlDirs: TStringList;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    function GetMboxFilenames: TStringList;
    function GetEmlDirs: TStringList;
  end;

var
  FMbox2EmlDlg: TFMbox2EmlDlg;

implementation

uses Main, Log;

{$R *.DFM}

constructor TFMbox2EmlDlg.Create(AOwner: TComponent);
begin
    Inherited Create(Aowner);
    mboxFiles:=TStringList.Create;
    emlDirs:=TStringList.Create;
end;

destructor TFMbox2EmlDlg.Destroy;
begin
    mboxFiles.Free;
    emlDirs.Free;
    Inherited Destroy;
end;

procedure TFMbox2EmlDlg.FormActivate(Sender: TObject);
begin
    EdEmlDir.Text:=settings.MiscSettings.EMLDir;
    CBDotsToFolders.Checked:=true;
end;

procedure TFMbox2EmlDlg.Button3Click(Sender: TObject);
begin
    FMain.ULOpenDialog.Title:='Select mbox files for conversion';
    FMain.ULOpenDialog.InitialDir:=settings.MiscSettings.MboxDir;
    FMain.ULOpenDialog.Filter := 'Unix mailbox files (*.mbox)|*.MBOX|All files (*.*)|*.*';
    FMain.ULOpenDialog.Options:= FMain.ULOpenDialog.Options + [ofAllowMultiSelect];
    if FMain.ULOpenDialog.Execute then begin
        Memo1.Lines.AddStrings(FMain.ULOpenDialog.Files);
        settings.MiscSettings.MboxDir:=ExtractFilePath(FMain.ULOpenDialog.Filename);
    end;
end;

procedure TFMbox2EmlDlg.SpeedButton2Click(Sender: TObject);
begin
    DirDialog1.Folder:=settings.MiscSettings.EMLDir;
    if DirDialog1.Execute then begin
        settings.MiscSettings.EMLDir:=DirDialog1.Folder;
        EdEmlDir.Text:=DirDialog1.Folder;
    end;
end;

{ Convert button }
procedure TFMbox2EmlDlg.Button1Click(Sender: TObject);
var emlFolder: String; i: Integer; emlOk: Boolean;
begin
    // Ensure the root eml dir exists
    emlOk:=true;
    if not DirectoryExists(EdEMLDir.Text) then
        if not ForceDirectories(EdEMLDir.Text) then begin
            emlOk:=false;
        end;

    if emlOk then begin
        if Memo1.Lines.Count>0 then begin
            for i:=0 to Memo1.Lines.Count-1 do begin
                emlOk:=true;
                if FileExists(Memo1.Lines[i]) then begin

                    emlFolder:=ChangeFileExt(ExtractFilename(Memo1.Lines[i]),'');
                    if CBDotsToFolders.Checked then
                        emlFolder:=StringReplace(emlFolder, '.', '\',[rfReplaceAll, rfIgnoreCase]);
                    emlFolder:=IncludeTrailingBackslash(EdEMLDir.Text) + emlFolder;

                    if not DirectoryExists(emlFolder) then
                        if not ForceDirectories(emlFolder) then emlOk:=false;
                    if emlOk then begin
                        mboxFiles.Add(Memo1.Lines[i]);
                        emlDirs.Add(emlFolder);
                    end
                    else
                        MessageDlg('Problem creating '+emlFolder+'. Skipping conversion of '+Memo1.Lines[i]+' mbox file',mtWarning,[mbOk],0);
                end
                else begin
                    MessageDlg('File '+Memo1.Lines[i]+' does not exist. Skipping.',mtWarning,[mbOk],0);
                end;
            end;
            ModalResult:=mrOk;
        end
        else
            MessageDlg('Please specify at least one mbox file',mtWarning,[mbOK],0);
    end
    else
        MessageDlg('Please specify a valid dir for eml files',mtWarning,[mbOK],0);

end;

procedure TFMbox2EmlDlg.Button2Click(Sender: TObject);
begin
    ModalResult:=mrCancel;
end;

function TFMbox2EmlDlg.GetMboxFilenames: TStringList;
begin
    Result:=mboxFiles;
end;

function TFMbox2EmlDlg.GetEmlDirs: TStringList;
begin
    Result:=emlDirs;
end;

procedure TFMbox2EmlDlg.Button4Click(Sender: TObject);
begin
    Memo1.Lines.Clear;
end;

end.
