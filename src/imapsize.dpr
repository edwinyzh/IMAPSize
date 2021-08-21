program imapsize;

{%ToDo 'imapsize.todo'}


{$WARNINGS OFF}
{$HINTS OFF}

uses
  madExcept,
  Forms,
  classes,
  dialogs,
  Main in 'Main.pas' {FMain},
  Options in 'Options.pas' {FOptions},
  About in 'About.pas' {FAbout},
  ListNodes in 'ListNodes.pas' {FListNodes},
  Accounts in 'Accounts.pas',
  AddAccountDlg in 'AddAccountDlg.pas' {FAddAccountDlg},
  Nodes in 'Nodes.pas',
  QuotaDlg in 'QuotaDlg.pas' {FQuotaDlg},
  AppSettings in 'AppSettings.pas',
  SpamHandles in 'SpamHandles.pas',
  AddSpamHandleDlg in 'AddSpamHandleDlg.pas' {FAddSpamHandleDlg},
  MsgPeeker in 'MsgPeeker.pas' {FMsgPeeker},
  NewMailboxDlg in 'NewMailboxDlg.pas' {FNewMailboxDlg},
  SortArrowBMPs in 'SortArrowBMPs.pas',
  MyTypes in 'MyTypes.pas',
  BodyStructure in 'BodyStructure.pas',
  DestinationChooser in 'DestinationChooser.pas' {FDestinationChooserDlg},
  VecLog in 'VecLog.pas',
  AdvSearchDlg in 'AdvSearchDlg.pas' {FAdvSearchDlg},
  SearchKeyFrm in 'SearchKeyFrm.pas' {SearchKeyFrame: TFrame},
  ICache in 'ICache.pas',
  Eml2MboxDlg in 'Eml2MboxDlg.pas' {FEml2MboxDlg},
  Mbox2EmlDlg in 'Mbox2EmlDlg.pas' {FMbox2EmlDlg},
  GetConnectionResponseInfo in 'GetConnectionResponseInfo.pas',
  IMAPConsole in 'IMAPConsole.pas' {FIMAPConsole},
  RequestActivityFrm in 'RequestActivityFrm.pas' {RequestActivityFrame: TFrame},
  ActivityDlg in 'ActivityDlg.pas' {FActivityDlg},
  DownloadSizeQueryDlg in 'DownloadSizeQueryDlg.pas' {FDownloadSizeQueryDlg},
  AttachmentStripper in 'AttachmentStripper.pas',
  DisplayRequestLogDlg in 'DisplayRequestLogDlg.pas' {FDisplayRequestLogDlg},
  FolderListProcessor in 'FolderListProcessor.pas',
  ImageViewer in 'ImageViewer.pas' {FImageViewer},
  TextViewer in 'TextViewer.pas' {FTextViewer},
  MailUtil in 'MailUtil.pas',
  FolderSubDlg in 'FolderSubDlg.pas' {FFolderSubDlg},
  FolderHierarchyReplicatorDlg in 'FolderHierarchyReplicatorDlg.pas' {FFolderHierarchyReplicatorDlg},
  AccountBackupDlg in 'AccountBackupDlg.pas' {FAccountBackupDlg},
  PasswordDlg in 'PasswordDlg.pas' {FPasswordDlg},
  SaveAttachmentsDlg in 'SaveAttachmentsDlg.pas' {FSaveAttachmentsDlg},
  Namespace in 'Namespace.pas',
  BackupDB in 'BackupDB.pas',
  Eml2MboxesDlg in 'Eml2MboxesDlg.pas' {FEml2MboxesDlg},
  RestoreBackupDlg in 'RestoreBackupDlg.pas' {FRestoreBackupDlg},
  MboxInspectorDlg in 'MboxInspectorDlg.pas' {FMboxInspectorDlg},
  CommonTypes in 'CommonTypes.pas',
  UpgradeBackupsDlg in 'UpgradeBackupsDlg.pas' {FUpgradeBackupsDlg},
  MboxFile in 'MboxFile.pas',
  BackupFilenameFormat in 'BackupFilenameFormat.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'IMAPSize';
  Application.CreateForm(TFMain, FMain);
  Application.CreateForm(TFMsgPeeker, FMsgPeeker);
  Application.CreateForm(TFDestinationChooserDlg, FDestinationChooserDlg);
  Application.CreateForm(TFActivityDlg, FActivityDlg);
  Application.CreateForm(TFUpgradeBackupsDlg, FUpgradeBackupsDlg);
  try
    Application.Run;
  except
    on EOutOfResources do
        MessageDlg('Your system is running low on resources! IMAPSize might experience some instability. Please close any unnecessary applications.',mtWarning,[mbOK],0);
  end;            
end.
