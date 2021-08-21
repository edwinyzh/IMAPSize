{==============================================================================|
| Project : IMAPSize                                                           |
|==============================================================================|
| $Id: Options.pas,v 1.12 2004/03/31 23:27:33 Ivan Exp $
|==============================================================================|
| Copyright 2003-2004 Ivan Vecanski                                            |
| =============================================================================}

unit Options;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls,  ComCtrls, Spin, AppSettings, Accounts, VecLog, SpamHandles, EncryptIt,
  STColors, Log, Buttons, PBFolderDialog;

const
    DEFAULT_IMAP_PORT = 143;
    DEFAULT_IMAPS_PORT = 993;

type

  TFOptions = class(TForm)
    PanelPodloga: TPanel;
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label3: TLabel;
    EditSpamHeader: TEdit;
    EditCompareValue: TEdit;
    GroupBox1: TGroupBox;
    Image2: TImage;
    Image3: TImage;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    GroupBox3: TGroupBox;
    Label2: TLabel;
    CheckBox2: TCheckBox;
    SpinEdit3: TSpinEdit;
    TabSheet3: TTabSheet;
    Panel2: TPanel;
    ListBoxAccounts: TListBox;
    Panel3: TPanel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    EditAcctUser: TEdit;
    EditAcctPass: TEdit;
    EditAcctServer: TEdit;
    EditAcctPort: TEdit;
    EditAcctName: TEdit;
    CBSSL: TCheckBox;
    ButtonRemoveAcct: TButton;
    ButtonAddAcct: TButton;
    ListBoxSpamHandles: TListBox;
    ButtonAddSH: TButton;
    ButtonRemoveSH: TButton;
    Label4: TLabel;
    ComboBoxAcctSpamHandle: TComboBox;
    RadioGroupSpamRecognizedBy: TRadioGroup;
    Label10: TLabel;
    EditHandleName: TEdit;
    TabSheet4: TTabSheet;
    RadioGroup1: TRadioGroup;
    GroupBox4: TGroupBox;
    Image4: TImage;
    Image5: TImage;
    Image6: TImage;
    SpinEdit4: TSpinEdit;
    SpinEdit5: TSpinEdit;
    RadioGroup2: TRadioGroup;
    GroupBox5: TGroupBox;
    Label11: TLabel;
    Label12: TLabel;
    Image1: TImage;
    STColorsCombo1: TSTColorsCombo;
    STColorsCombo2: TSTColorsCombo;
    SmartInboxCB: TCheckBox;
    TabSheet5: TTabSheet;
    Panel4: TPanel;
    CBMsgPeekerLimitSize: TCheckBox;
    SESizeLimit: TSpinEdit;
    Label13: TLabel;
    TabSheet6: TTabSheet;
    RGIMAPTrafficLog: TRadioGroup;
    SEMaxLogSize: TSpinEdit;
    Label14: TLabel;
    Label15: TLabel;
    SEFilesToKeep: TSpinEdit;
    RGAddressStructure: TRadioGroup;
    CBEnableHeaderEdit: TCheckBox;
    Label16: TLabel;
    CBUpdateOnSizeCheck: TCheckBox;
    CBAutoCloseMsgPeeker: TCheckBox;
    TabSheet7: TTabSheet;
    CBEnableCache: TCheckBox;
    Button3: TButton;
    FontDialog1: TFontDialog;
    Button4: TButton;
    CBXPMenus: TCheckBox;
    LblServerTimeout: TLabel;
    SpinEditServerTimeout: TSpinEdit;
    Label18: TLabel;
    CBMsgDelAddFilenamesToBody: TCheckBox;
    CBLoadFoldersStartup: TCheckBox;
    EdBackupDir: TEdit;
    SpeedButton1: TSpeedButton;
    Label19: TLabel;
    DirDialog1: TPBFolderDialog;
    CBSavePassword: TCheckBox;
    CBAnsweredItalics: TCheckBox;
    EditRootFolder: TEdit;
    CBExcludeRoot: TCheckBox;
    Label17: TLabel;
    Bevel1: TBevel;
    Bevel2: TBevel;
    EditBackupFilenameFormat: TEdit;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ButtonAddAcctClick(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure ListBoxAccountsClick(Sender: TObject);
    procedure ButtonRemoveAcctClick(Sender: TObject);
    procedure ListBoxSpamHandlesClick(Sender: TObject);
    procedure ButtonAddSHClick(Sender: TObject);
    procedure ButtonRemoveSHClick(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure SpinEdit4Change(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure RGIMAPTrafficLogClick(Sender: TObject);
    procedure CBMsgPeekerLimitSizeClick(Sender: TObject);
    procedure CBSSLClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure CBAnsweredItalicsClick(Sender: TObject);
  private
    prevSelectedListIndexAcct: Integer; // Previous index in list
    prevSelectedNameAcct: String; // Name (original) of the previously selected account in the list
    prevSelectedListIndexSH: Integer; // Previous index in list
    prevSelectedNameSH: String; // Name (original) of the previously selected account in the list
    initialRootBackupDir: String;

    procedure displaySelectedAcct;
    procedure createAccountInfoFromFields(var account: TAccountInfo);
    procedure clearFieldsAcct;
    procedure setAccounts;
    procedure SaveAccountChange;

    procedure displaySelectedSH;
    procedure createSpamHandleInfoFromFields(spamHandle: TSpamHandleInfo);
    procedure clearFieldsSH;
    procedure setSpamHandles;
    procedure SaveSpamHandleChange;
    procedure SyncSpamHandles(index: Integer);
  public
    changeForReload: Boolean;
    changeForMessageListReload: Boolean;
    logSettingsChanged: Boolean;
    accountDeleted: Boolean;
    procedure SetValues;



  end;

var
  FOptions: TFOptions;

implementation

uses Main, AddAccountDlg, AddSpamHandleDlg, MsgPeeker;

{$R *.DFM}

procedure TFOptions.SetValues;
begin
    // First set spam handles
    setSpamHandles;
    SyncSpamHandles(0);
    // Now set accounts (listbox should be populated)
    setAccounts;

    SpinEdit1.Value:=settings.Thresholds.Lower;
    SpinEdit2.Value:=settings.Thresholds.Upper;
    SpinEdit3.Value:=settings.Thresholds.AllowLowNumThr;
    CheckBox2.Checked:=settings.Thresholds.AllowLowNumChange;
    RadioGroup1.ItemIndex:=settings.TreeDisplay.SizeFormat;
    SpinEdit3.Enabled:=CheckBox2.Checked;
    CBUpdateOnSizeCheck.Checked:=settings.TreeDisplay.UpdateAsSizeCheck;

    SpinEdit4.Value:=settings.MessageDisplay.Lower;
    SpinEdit5.Value:=settings.MessageDisplay.Upper;
    STColorsCombo1.ColorSelected:=settings.MessageDisplay.FlaggedColor;
    STColorsCombo2.ColorSelected:=settings.MessageDisplay.SpamColor;
    RadioGroup2.ItemIndex:=settings.MessageDisplay.SizeFormat;
    RGAddressStructure.ItemIndex:=settings.MessageDisplay.AddressStructure;
    CBAnsweredItalics.Checked:=settings.MessageDisplay.AnsweredItalics;

    CBMsgPeekerLimitSize.Checked:=settings.IMAPOpSettings.LimitTextToFetch;
    SESizeLimit.Value:=settings.IMAPOpSettings.TextToFetchLength;
    CBEnableHeaderEdit.Checked:=settings.IMAPOpSettings.EnableHeaderEditing;
    CBEnableCache.Checked:=settings.MiscSettings.EnableCaching;
    CBAutoCloseMsgPeeker.Checked:=settings.MessagePeeker.AutoCloseMsgPeeker;
    CBMsgDelAddFilenamesToBody.Checked:=settings.IMAPOpSettings.MsgDelAddFilenamesToBody;

    RGIMAPTrafficLog.ItemIndex:=settings.LogSettings.IMAPLog;
    //AppLogging.ItemIndex:=settings.LogSettings.AppLog;
    SEMaxLogSize.Value:=settings.LogSettings.MaxFileSize;
    SEFilesToKeep.Value:=settings.LogSettings.FilesToKeep;

    CBXPMenus.Checked:=settings.MiscSettings.XPMenus;
    SpinEditServerTimeout.Value:=settings.IMAPOpSettings.ServerTimeout;
    if settings.MiscSettings.BackupDir<>'' then
        EdBackupDir.Text:=settings.MiscSettings.BackupDir
    else
        EdBackupDir.Text:=dataDir+'\backup';
    EditBackupFilenameFormat.Text:=settings.MiscSettings.BackupFilenameFormat;
    initialRootBackupDir:=EdBackupDir.Text;
    CBLoadFoldersStartup.Checked:=settings.MiscSettings.LoadFoldersStartup;

    changeForReload:=false;
    changeForMessageListReload:=false;
    logSettingsChanged:=false;
    accountDeleted:=false;
end;

procedure TFOptions.Button1Click(Sender: TObject);
var thresholdWarning: Boolean;
begin
    // Accounts
    SaveAccountChange;
    if (ListBoxAccounts.ItemIndex <> -1) then
        settings.Accounts.DefaultIndex:=ListBoxAccounts.ItemIndex;

    SaveSpamHandleChange;
    //if (ListBoxSpamHandles.ItemIndex <> -1) then
    //    settings.SpamHandles.DefaultIndex:=ListBoxSpamHandles.ItemIndex;

    settings.TreeDisplay.SizeFormat:=RadioGroup1.ItemIndex;
    settings.TreeDisplay.UpdateAsSizeCheck:=CBUpdateOnSizeCheck.Checked;

    // Thresholds
    thresholdWarning:=false;
    if (SpinEdit1.Value>=SpinEdit2.Value) then begin
        SpinEdit2.Value:=SpinEdit1.Value+10;
        thresholdWarning:=true;
    end;
    settings.Thresholds.Lower:=SpinEdit1.Value;
    settings.Thresholds.Upper:=SpinEdit2.Value;
    settings.Thresholds.AllowLowNumThr:=SpinEdit3.Value;
    settings.Thresholds.AllowLowNumChange:=CheckBox2.Checked;

    if (SpinEdit4.Value>=SpinEdit5.Value) then begin
        SpinEdit5.Value:=SpinEdit4.Value*10;
        thresholdWarning:=true;
    end;
    settings.MessageDisplay.Lower:=SpinEdit4.Value;
    settings.MessageDisplay.Upper:=SpinEdit5.Value;
    settings.MessageDisplay.FlaggedColor:=STColorsCombo1.ColorSelected;
    settings.MessageDisplay.SpamColor:=STColorsCombo2.ColorSelected;
    settings.MessageDisplay.SizeFormat:=RadioGroup2.ItemIndex;
    settings.MessageDisplay.AddressStructure := RGAddressStructure.ItemIndex;
    settings.MessageDisplay.AnsweredItalics:=CBAnsweredItalics.Checked;

    settings.IMAPOpSettings.LimitTextToFetch := CBMsgPeekerLimitSize.Checked;
    settings.IMAPOpSettings.TextToFetchLength := SESizeLimit.Value;
    settings.IMAPOpSettings.EnableHeaderEditing := CBEnableHeaderEdit.Checked;
    settings.IMAPOpSettings.ServerTimeout:=SpinEditServerTimeout.Value;
    settings.IMAPOpSettings.MsgDelAddFilenamesToBody:=CBMsgDelAddFilenamesToBody.Checked;
    settings.MiscSettings.EnableCaching:=CBEnableCache.Checked;
    settings.MiscSettings.XPMenus:=CBXPMenus.Checked;
    settings.MiscSettings.LoadFoldersStartup:=CBLoadFoldersStartup.Checked;
    settings.MessagePeeker.AutoCloseMsgPeeker:=CBAutoCloseMsgPeeker.Checked;

    settings.LogSettings.IMAPLog:=RGIMAPTrafficLog.ItemIndex;
    //settings.LogSettings.AppLog:=AppLogging.ItemIndex;
    settings.LogSettings.MaxFileSize:=SEMaxLogSize.Value;
    settings.LogSettings.FilesToKeep:=SEFilesToKeep.Value;

    if thresholdWarning then
        MessageDlg('An image threshold has been automatically modified since a lower threshold was higher than a higher one. Please check the values...',mtWarning,[mbOK],0);

    optionsDlgActiveTab:=PageControl1.ActivePageIndex;

    // Update root backup folder 
    if (settings.MiscSettings.BackupExists and (initialRootBackupDir<>EdBackupDir.Text)) then begin
        try
            FMain.backupDb.UpdateRootBackupDir(EdBackupDir.Text);
            settings.MiscSettings.BackupDir:=EdBackupDir.Text;
        except
            devLog.Error('Unable to update the root backup location');
        end;
    end
    else
        // Just change the setting
        settings.MiscSettings.BackupDir:=EdBackupDir.Text;

    settings.MiscSettings.BackupFilenameFormat:=EditBackupFilenameFormat.Text;

    ModalResult:=mrOK;

end;

procedure TFOptions.Button2Click(Sender: TObject);
begin
    ModalResult:=mrCancel;
end;

procedure TFOptions.SpinEdit1Change(Sender: TObject);
begin
    changeForReload:=true;
end;

procedure TFOptions.CheckBox2Click(Sender: TObject);
begin
    SpinEdit3.Enabled:=CheckBox2.Checked;
    changeForReload:=true;
end;

{ -------- Account specific methods --------- }

procedure TFOptions.setAccounts;
var i: Integer;
begin
    // Now populate List
    if (settings.Accounts.AccountItems.Count > 0) then begin
        for i:=0 to settings.Accounts.AccountItems.Count-1 do begin
            ListBoxAccounts.Items.Add(settings.Accounts.GetAccount(i).Name);
        end;
        if (settings.Accounts.DefaultIndex = -1) then ListBoxAccounts.ItemIndex := 0
        else ListBoxAccounts.ItemIndex:=settings.Accounts.DefaultIndex;
        prevSelectedListIndexAcct := ListBoxAccounts.ItemIndex;
        prevSelectedNameAcct := ListBoxAccounts.Items[ListBoxAccounts.ItemIndex];
        displaySelectedAcct;
    end
end;

procedure TFOptions.displaySelectedAcct;
var acName: String; account: TAccountInfo;
begin
    if ListBoxAccounts.ItemIndex>-1 then begin
        acName:=ListBoxAccounts.Items[ListBoxAccounts.ItemIndex];
        account:=settings.Accounts.GetAccount(acName);
        EditAcctUser.Text:=account.username;
        CBSavePassword.Checked:=account.SavePassword;
        if account.SavePassword then
            EditAcctPass.Text:=Decrypt(account.password)
        else
            EditAcctPass.Text:='';
        EditAcctServer.Text:=account.IMAPServer;
        CBSSL.Checked:=account.SSL;
        EditAcctPort.Text:=IntToStr(account.Port);
        EditAcctName.Text:=account.Name;
        SmartInboxCB.Checked:=account.SmartInbox;
        CBExcludeRoot.Checked:=account.ExcludeRoot;
        // Ensure there is no -1s
        //if account.SpamHandle = -1 then account.SpamHandle:=0;
        try
            ComboBoxAcctSpamHandle.ItemIndex:=account.SpamHandle;
        except
            ComboBoxAcctSpamHandle.ItemIndex:=0;
        end;
        EditRootFolder.Text:=account.RootFolder;
    end;
end;

procedure TFOptions.createAccountInfoFromFields(var account: TAccountInfo);
begin
    account.name:=EditAcctName.Text;
    account.Username:=EditAcctUser.Text;
    account.SavePassword:=CBSavePassword.Checked;
    if account.SavePassword then
        account.Password:=Encrypt(EditAcctPass.Text)
    else
        account.Password:='';
    account.IMAPServer:=EditAcctServer.Text;
    account.SSL:=CBSSL.Checked;
    try
        account.Port:=StrToInt(EditAcctPort.Text);
    except
        // If the port box was deleted, set it to the default one
        on EConvertError do account.Port:=DEFAULT_IMAP_PORT;
    end;
    account.SmartInbox:=SmartInboxCB.Checked;
    account.SpamHandle:=ComboBoxAcctSpamHandle.ItemIndex;
    account.RootFolder:=EditRootFolder.Text;
    account.ExcludeRoot:=CBExcludeRoot.Checked;
end;

procedure TFOptions.clearFieldsAcct;
begin
    EditAcctName.Clear;
    EditAcctUser.Clear;
    EditAcctPass.Clear;
    EditAcctServer.Clear;
    CBSSL.Checked:=false;
    EditAcctPort.Clear;
    SmartInboxCB.Checked:=false;
    ComboBoxAcctSpamHandle.ItemIndex:=-1;
    EditRootFolder.Clear;
    CBExcludeRoot.Checked:=false;
end;

procedure TFOptions.ListBoxAccountsClick(Sender: TObject);
begin
    SaveAccountChange;
end;

procedure TFOptions.SaveAccountChange;
var account: TAccountInfo;
begin
    if (settings.Accounts.AccountItems.count > 0) then begin
        // Save previous
        account:=TAccountInfo.Create;
        createAccountInfoFromFields(account);
        settings.Accounts.updateAccount(prevSelectedNameAcct, account, settings.MiscSettings.BackupExists);
        account.Free;
        ListBoxAccounts.Items[prevSelectedListIndexAcct] := EditAcctName.Text;
        // Display selected
        displaySelectedAcct;
        // Store new index
        prevSelectedListIndexAcct := ListBoxAccounts.ItemIndex;
        prevSelectedNameAcct := ListBoxAccounts.Items[ListBoxAccounts.ItemIndex];
    end;
end;

procedure TFOptions.ButtonAddAcctClick(Sender: TObject);
var account: TAccountInfo;
begin
    FAddAccountDlg := TFAddAccountDlg.Create(Application);
    if FAddAccountDlg.ShowModal = mrOK then begin
        account:=TAccountInfo.Create;
        FAddAccountDlg.getNewAccount(account);
        // Add this account
        ListBoxAccounts.ItemIndex := ListBoxAccounts.Items.Add(account.Name);
        settings.Accounts.addAccount(account);
        displaySelectedAcct;
        // Store new index
        prevSelectedListIndexAcct := ListBoxAccounts.ItemIndex;
        prevSelectedNameAcct := ListBoxAccounts.Items[ListBoxAccounts.ItemIndex];
    end;
    FAddAccountDlg.Free;
end;

procedure TFOptions.ButtonRemoveAcctClick(Sender: TObject);
begin
    if ListBoxAccounts.ItemIndex = -1 then
        MessageDlg('You didn''t select an account! Please select the account you wish to remove',mtInformation,[mbOK],0)
    else begin
        settings.Accounts.removeAccount(ListBoxAccounts.Items[ListBoxAccounts.ItemIndex]);
        ListBoxAccounts.Items.Delete(ListBoxAccounts.ItemIndex);

        if ListBoxAccounts.Items.Count > 0 then
            ListBoxAccounts.ItemIndex := 0;
        if (ListBoxAccounts.ItemIndex > -1) then begin
           // Store new index
            prevSelectedListIndexAcct := ListBoxAccounts.ItemIndex;
            prevSelectedNameAcct := ListBoxAccounts.Items[ListBoxAccounts.ItemIndex];

        end;
        clearFieldsAcct;
        displaySelectedAcct;
        accountDeleted:=true;
    end;
end;

{ -------- SpamHandle specific methods --------- }

procedure TFOptions.setSpamHandles;
var i: Integer;
begin
    // Now populate List
    if (settings.SpamHandles.SpamHandleItems.Count > 0) then begin
        for i:=0 to settings.SpamHandles.SpamHandleItems.Count-1 do begin
            ListBoxSpamHandles.Items.Add(settings.SpamHandles.GetSpamHandle(i).Name);
        end;
        // Code below commented out, no current need for default spam handle
        //if (settings.SpamHandles.DefaultIndex = -1) then ListBoxSpamHandles.ItemIndex := 0
        //else ListBoxSpamHandles.ItemIndex:=settings.SpamHandles.DefaultIndex;
        ListBoxSpamHandles.ItemIndex := 0;
        prevSelectedListIndexSH := ListBoxSpamHandles.ItemIndex;
        prevSelectedNameSH := ListBoxSpamHandles.Items[ListBoxSpamHandles.ItemIndex];
        displaySelectedSH;
    end
end;

procedure TFOptions.displaySelectedSH;
var shName: String; spamHandle: TSpamHandleInfo;
begin
    if ListBoxSpamHandles.ItemIndex>-1 then begin
        shName:=ListBoxSpamHandles.Items[ListBoxSpamHandles.ItemIndex];
        spamHandle:=settings.SpamHandles.GetSpamHandle(shName);
        EditHandleName.Text:=spamHandle.Name;
        EditSpamHeader.Text:=spamHandle.SpamHeader;
        RadioGroupSpamRecognizedBy.ItemIndex:=spamHandle.SpamComparedBy;
        EditCompareValue.Text:=spamHandle.CompareValue;
    end;
end;

procedure TFOptions.createSpamHandleInfoFromFields(spamHandle: TSpamHandleInfo);
begin
    spamHandle.name:=EditHandleName.Text;
    spamHandle.SpamHeader:=EditSpamHeader.Text;
    spamHandle.SpamComparedBy:=RadioGroupSpamRecognizedBy.ItemIndex;
    spamHandle.CompareValue:=EditCompareValue.Text;
end;

procedure TFOptions.clearFieldsSH;
begin
    EditHandleName.Clear;
    EditSpamHeader.Clear;
    RadioGroupSpamRecognizedBy.ItemIndex:=0;
    EditCompareValue.Clear;
end;

procedure TFOptions.ListBoxSpamHandlesClick(Sender: TObject);
begin
    SaveSpamHandleChange;
end;

procedure TFOptions.SaveSpamHandleChange;
var spamHandle: TSpamHandleInfo;
begin
    if (settings.SpamHandles.SpamHandleItems.count > 0) then begin
        // Save previous
        spamHandle:=TSpamHandleInfo.Create;
        createSpamHandleInfoFromFields(spamHandle);
        settings.SpamHandles.updateSpamHandle(prevSelectedNameSH, spamHandle);
        spamHandle.Free;
        ListBoxSpamHandles.Items[prevSelectedListIndexSH] := EditHandleName.Text;
        // Display selected
        displaySelectedSH;
        // Store new index
        prevSelectedListIndexSH := ListBoxSpamHandles.ItemIndex;
        prevSelectedNameSH := ListBoxSpamHandles.Items[ListBoxSpamHandles.ItemIndex];
    end;
end;

procedure TFOptions.ButtonAddSHClick(Sender: TObject);
var spamHandle: TSpamHandleInfo;
begin
    FAddSpamHandleDlg := TFAddSpamHandleDlg.Create(Application);
    if FAddSpamHandleDlg.ShowModal = mrOK then begin
        spamHandle:=TSpamHandleInfo.Create;
        FAddSpamHandleDlg.getNewSpamHandle(spamHandle);
        // Add this account
        ListBoxSpamHandles.ItemIndex := ListBoxSpamHandles.Items.Add(spamHandle.Name);
        settings.SpamHandles.addSpamHandle(spamHandle);
        displaySelectedSH;
        // Store new index
        prevSelectedListIndexSH := ListBoxSpamHandles.ItemIndex;
        prevSelectedNameSH := ListBoxSpamHandles.Items[ListBoxSpamHandles.ItemIndex];
        SyncSpamHandles(0);
    end;
    FAddSpamHandleDlg.Free;
end;

procedure TFOptions.ButtonRemoveSHClick(Sender: TObject);
var deletedIndex: Integer;
begin
    if ListBoxSpamHandles.ItemIndex = -1 then
        MessageDlg('You didn''t select a spam handle! Please select the spam handle you wish to remove',mtInformation,[mbOK],0)
    else begin
        deletedIndex:=ListBoxSpamHandles.ItemIndex+1;
        // +1 is to adjust to the account handle combo display (has a None with index 0 prepended
        settings.SpamHandles.removeSpamHandle(ListBoxSpamHandles.Items[ListBoxSpamHandles.ItemIndex]);
        ListBoxSpamHandles.Items.Delete(ListBoxSpamHandles.ItemIndex);

        if ListBoxSpamHandles.Items.Count > 0 then
            ListBoxSpamHandles.ItemIndex := 0;
        if (ListBoxSpamHandles.ItemIndex > -1) then begin
           // Store new index
            prevSelectedListIndexSH := ListBoxSpamHandles.ItemIndex;
            prevSelectedNameSH := ListBoxSpamHandles.Items[ListBoxSpamHandles.ItemIndex];

        end;
        clearFieldsSH;
        displaySelectedSH;
        SyncSpamHandles(deletedIndex);
    end;
end;

{ Syncs spam handles in the spam handles list and the combo for accounts }
{ @param index - If zero, just populate trees, >0 signals that a handle with
  that index has been deleted. Go through the accounts and if any has that
  handle, set it to None (0). Also decrement indexes of handles for any accounts
  using handles greater than the index }
procedure TFOptions.SyncSpamHandles(index: Integer);
begin
    // First populate the handle combo in accounts tab
    devLog.Debug('SyncSpamHandles index='+IntToStr(index));
    ComboBoxAcctSpamHandle.Clear;
    ComboBoxAcctSpamHandle.Items.Add('None');
    ComboBoxAcctSpamHandle.Items.AddStrings(ListBoxSpamHandles.Items);

    if index>0 then begin
        // Set all accounts
        settings.Accounts.AdjustDeletedSpamHandle(index);
        // Refresh display of the selected account (doing this for the combo only, but won't do any harm)
        displaySelectedAcct;
    end;
end;


procedure TFOptions.RadioGroup1Click(Sender: TObject);
begin
    changeForReload:=true;
end;

procedure TFOptions.SpinEdit4Change(Sender: TObject);
begin
    changeForMessageListReload:=true;
end;

procedure TFOptions.RadioGroup2Click(Sender: TObject);
begin
    changeForMessageListReload:=true;
end;

procedure TFOptions.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
    if Key=VK_ESCAPE then ModalResult:=mrCancel;
end;

procedure TFOptions.RGIMAPTrafficLogClick(Sender: TObject);
begin
    logSettingsChanged:=true;
end;

procedure TFOptions.CBMsgPeekerLimitSizeClick(Sender: TObject);
begin
    SESizeLimit.Enabled:=CBMsgPeekerLimitSize.Checked;
end;

procedure TFOptions.CBSSLClick(Sender: TObject);
begin
    if CBSSL.Checked then EditAcctPort.Text:=IntToStr(DEFAULT_IMAPS_PORT)
    else EditAcctPort.Text:=IntToStr(DEFAULT_IMAP_PORT);
end;

procedure TFOptions.Button3Click(Sender: TObject);
begin
    if MessageDlg('Are you sure you want to delete the whole cache?',mtConfirmation,[mbYes,mbNo],0)=mrYes then begin
        if not mCache.ClearCache then
            MessageDlg('The cache could not be deleted!',mtError,[mbOK],0);
    end;
end;

procedure TFOptions.Button4Click(Sender: TObject);
begin
    FontDialog1.Font.Name:=settings.MessagePeeker.FontName;
    FontDialog1.Font.Size:=settings.MessagePeeker.FontSize;
    if FontDialog1.Execute then begin
        // Save settings
        settings.MessagePeeker.FontName:=FontDialog1.Font.Name;
        settings.MessagePeeker.FontSize:=FontDialog1.Font.Size;
        // ... and apply to the MessageViewer
        FMsgPeeker.ApplyFont(FontDialog1.Font);
    end;
end;

procedure TFOptions.SpeedButton1Click(Sender: TObject);
begin
    DirDialog1.Folder:=EdBackupDir.Text;
    if DirDialog1.Execute then begin
        settings.MiscSettings.BackupDir:=DirDialog1.Folder;
        EdBackupDir.Text:=DirDialog1.Folder;
    end;
end;

procedure TFOptions.CBAnsweredItalicsClick(Sender: TObject);
begin
    settings.MessageDisplay.AnsweredItalics:=CBAnsweredItalics.Checked;
end;

end.
