{==============================================================================|
| Project : IMAPSize                                                           |
|==============================================================================|
| $Id: UpgradeBackupsDlg.pas 33 2006-03-31 20:25:01Z Ivan $
|==============================================================================|
| Copyright 2003-2006 Ivan Vecanski                                            |
| =============================================================================}

unit UpgradeBackupsDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, PBFolderDialog, Buttons, ComCtrls;

type
  TFUpgradeBackupsDlg = class(TForm)
    Memo1: TMemo;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    LblFolder: TLabel;
    Button1: TButton;
    Button2: TButton;
    PBFolderDialog1: TPBFolderDialog;
    SpeedButton1: TSpeedButton;
    ProgressBar1: TProgressBar;
    Label3: TLabel;
    Memo2: TMemo;
    LblOperation: TLabel;
    Label4: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FUpgradeBackupsDlg: TFUpgradeBackupsDlg;

implementation

uses Main;

{$R *.DFM}

procedure TFUpgradeBackupsDlg.FormCreate(Sender: TObject);
begin
    Edit1.Text:=settings.MiscSettings.BackupDir+'-old';
    LblFolder.Caption:=settings.MiscSettings.BackupDir;
    LblOperation.Caption:='';
end;

procedure TFUpgradeBackupsDlg.SpeedButton1Click(Sender: TObject);
begin
    PBFolderDialog1.Folder:=Edit1.Text;
    if PBFolderDialog1.Execute then begin
        Edit1.Text:=PBFolderDialog1.Folder;
    end;
end;

procedure TFUpgradeBackupsDlg.Button1Click(Sender: TObject);
begin
    // Get all eml files in the old-backup folder hierarchy
    Button2.Caption:='Stop';
end;

end.
