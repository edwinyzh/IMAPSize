{==============================================================================|
| Project : IMAPSize                                                           |
|==============================================================================|
| $Id: Main.pas,v 1.12 2004/04/04 20:17:11 Ivan Exp $
|==============================================================================|
| Copyright 2003-2004 Ivan Vecanski                                            |
| =============================================================================}

unit Main;

interface

uses

    // Delphi
    Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
    ComCtrls, Buttons, StdCtrls, contnrs, Menus, ExtCtrls, Grids, ActnList,

    // IMAPSize
    Log, VecLog, Accounts, CheckerThread, Nodes, NodeStack,
    IMAPWorker, MessageLists, QuotaDlg,
    AppSettings, SortArrowBMPs, GlobalSearchResult, MyTypes,
    CustomThread, ThreadManager, MailboxTree, SearchKeys, AdvSearchDlg,
    GlobalSearch, GlobalConstants, Converter, ICache, RequestManager,
    FolderHierarchyReplicatorDlg, SaveAttachmentsDlg, Namespace, BackupDB, CommonTypes,

    // 3rd party
    ThemeMgr, PBFolderDialog, ImgList, GIFImage, ALProgressBar, ALStatusBar,
    EnhListView, ExtListView, VirtualTrees,
    middlex, MiddlexRTTI, QuickRTTI, SplitFns, ToolWin, XPMenu;

const

    ONE_MEG = 1048576;  // 1024*1024
    PROG_BAR_WIDTH = 100;
    ANIM_GIF_WIDTH = 24;

    // Index of panels on the statusbar
    STATUSBAR_QUOTA = 0;
    STATUSBAR_MSGS = 1;

    // Index of panels on the message status bar
    MSG_STATUSBAR_DISPLAYED = 0;
    MSG_STATUSBAR_DESTINATION = 1;
    MSG_STATUSBAR_NUMBER = 2;
    
    // Command line switches
    CL_SWITCH_DATA_DIR='dataDir';
    CL_SWITCH_BACKUP_MODE='backup';
    CL_SWITCH_ACCOUNT='account';

type

  TFMain = class(TForm)
    PanelPozadina: TPanel;
    Panel1: TPanel;
    StatusBar1: TALStatusBar;
    ImageList1: TImageList;
    PopupMenu1: TPopupMenu;
    Listarticlesofthisnodessortedbysize1: TMenuItem;
    Expandthisnode1: TMenuItem;
    Collapsethisnode1: TMenuItem;
    N1: TMenuItem;
    TreeList1: TVirtualStringTree;
    RetrieveMessagesinthisfolder1: TMenuItem;
    PopupMenu2: TPopupMenu;
    PanelListView: TPanel;
    Splitter1: TSplitter;
    ToolBar1: TToolBar;
    ComboBox1: TComboBox;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton7: TToolButton;
    ToolButton14: TToolButton;
    ImageList2: TImageList;
    ImageList3: TImageList;
    MarkallMessagesDeleted1: TMenuItem;
    DeleteAllMessagesPermanently1: TMenuItem;
    N3: TMenuItem;
    UndeleteAllMessages1: TMenuItem;
    ExpungeMailbox1: TMenuItem;
    N5: TMenuItem;
    Reload1: TMenuItem;
    N4: TMenuItem;
    MainMenu1: TMainMenu;
    Connection1: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    ExportCSV: TMenuItem;
    CopyMenuItem: TMenuItem;
    CopyDeleteMenuItem: TMenuItem;
    CopyDelExpungeMenuItem: TMenuItem;
    N6: TMenuItem;
    N7: TMenuItem;
    PeekbothHeaderandBody1: TMenuItem;
    CoolBar1: TCoolBar;
    PanelSearch: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    EditSearchString: TEdit;
    ImageList4: TImageList;
    PopupMenu3: TPopupMenu;
    Collapsethisnode2: TMenuItem;
    N8: TMenuItem;
    Createmailbox1: TMenuItem;
    N9: TMenuItem;
    CreateSubMailbox1: TMenuItem;
    DeleteMailbox1: TMenuItem;
    ToolButton6: TToolButton;
    GIFImage1: TGIFImage;
    SpeedButton4: TSpeedButton;
    RenameMailbox1: TMenuItem;
    ListView1: TdfsExtListView;
    MsgStatusBar: TALStatusBar;
    ProgressBar1: TALProgressBar;
    N10: TMenuItem;
    Exit1: TMenuItem;
    Account1: TMenuItem;
    CheckSize1: TMenuItem;
    CheckQuota1: TMenuItem;
    N11: TMenuItem;
    Login1: TMenuItem;
    Logout1: TMenuItem;
    N12: TMenuItem;
    SortMailboxesbySize1: TMenuItem;
    GlobalSearch1: TMenuItem;
    View1: TMenuItem;
    TileHorizontally1: TMenuItem;
    TileVertically1: TMenuItem;
    N13: TMenuItem;
    Options1: TMenuItem;
    News1: TMenuItem;
    About2: TMenuItem;
    N14: TMenuItem;
    DeleteandExpunge1: TMenuItem;
    SaveDialog1: TSaveDialog;
    ActionList1: TActionList;
    AExportToCSV: TAction;
    AExit: TAction;
    ACheckSize: TAction;
    ACheckQuota: TAction;
    ALogin: TAction;
    ALogout: TAction;
    AListSorted: TAction;
    AGlobalSearch: TAction;
    AOptions: TAction;
    AHelp: TAction;
    ACancelAction: TAction;
    AShowDestinationDialog: TAction;
    AFocusAccount: TAction;
    AFocusSearch: TAction;
    AFocusTree: TAction;
    AFocusList: TAction;
    CreateSubmailbox2: TMenuItem;
    SaveMessageHeaders1: TMenuItem;
    AAdvancedSearch: TAction;
    Uploadmbox1: TMenuItem;
    FrommboxUnixmailbox1: TMenuItem;
    Fromemlfiles1: TMenuItem;
    N15: TMenuItem;
    ULOpenDialog: TOpenDialog;
    DLSaveDialog: TSaveDialog;
    DownloadMessages1: TMenuItem;
    Tounixmailboxmbox1: TMenuItem;
    Toemlfiles1: TMenuItem;
    DirDialog1: TPBFolderDialog;
    ThemeManager1: TThemeManager;
    DownloadMessages2: TMenuItem;
    ToUnixMailboxmbox2: TMenuItem;
    Toemlfiles2: TMenuItem;
    Tools1: TMenuItem;
    eml2mbox1: TMenuItem;
    mbox2eml1: TMenuItem;
    N2: TMenuItem;
    ClearAccountCache1: TMenuItem;
    New1: TMenuItem;
    Activity1: TMenuItem;
    ExpungeThisMailbox2: TMenuItem;
    SetFlag1: TMenuItem;
    RemoveFlag1: TMenuItem;
    SetFlagDELETED1: TMenuItem;
    SetFlagSEEN1: TMenuItem;
    SetFlagFLAGGEDMarkasImportant1: TMenuItem;
    RemoveFlagDELETED1: TMenuItem;
    RemoveFlagSEEN1: TMenuItem;
    RemoveFlagFLAGGED1: TMenuItem;
    CopyToDestinationMenuItem: TMenuItem;
    CopyToChooseDestinationMenuItem: TMenuItem;
    CopyDeleteToDestinationMenuItem: TMenuItem;
    CopyDeleteChooseDestinationMenuItem: TMenuItem;
    MoveToDestinationMenuItem: TMenuItem;
    MoveChooseDestinationMenuItem: TMenuItem;
    FAQ1: TMenuItem;
    DeleteAttachments1: TMenuItem;
    ImageList5: TImageList;
    XPMenu1: TXPMenu;
    ImageList6: TImageList;
    KeyboardShortcuts1: TMenuItem;
    FolderSubscriptions1: TMenuItem;
    ServerParameters1: TMenuItem;
    Answered1: TMenuItem;
    Answered2: TMenuItem;
    ReplicateFolderHierarchy1: TMenuItem;
    AccountBackup1: TMenuItem;
    SaveAttachmentsLocally1: TMenuItem;
    eml2mboxes1: TMenuItem;
    ToolButton8: TToolButton;
    AAccountBackup: TAction;
    RestoreBackup1: TMenuItem;
    mboxInspector1: TMenuItem;
    UpgradeBackups1: TMenuItem;
    N16: TMenuItem;
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormPaint(Sender: TObject);
    procedure Expandthisnode1Click(Sender: TObject);
    procedure Collapsethisnode1Click(Sender: TObject);
    procedure Listarticlesofthisnodessortedbysize1Click(Sender: TObject);
    procedure TreeList1GetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure TreeList1InitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure TreeList1FreeNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure TreeList1GetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure TreeList1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ComboBox1Click(Sender: TObject);
    procedure RetrieveMessagesinthisfolder1Click(Sender: TObject);
    procedure SpeedButton9Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListView1DrawItem(Control: TWinControl; var ACanvas: TCanvas;
      Index: Integer; ARect: TRect; State: TOwnerDrawState;
      var DefaultDrawing, FullRowSelect: Boolean);
    procedure ComboBox1DrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure ListView1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MarkallMessagesDeleted1Click(Sender: TObject);
    procedure UndeleteAllMessages1Click(Sender: TObject);
    procedure DeleteAllMessagesPermanently1Click(Sender: TObject);
    procedure ExpungeMailbox1Click(Sender: TObject);
    procedure Reload1Click(Sender: TObject);
    procedure ExpungeMailbox2Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure EditSearchStringKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure MarkSeen1Click(Sender: TObject);
    procedure MarkUnseen1Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure PeekbothHeaderandBody1Click(Sender: TObject);
    procedure SetFlag1Click(Sender: TObject);
    procedure RemoveFlag1Click(Sender: TObject);
    procedure SetFlag2Click(Sender: TObject);
    procedure RemoveFlag2Click(Sender: TObject);
    procedure SetFlag3Click(Sender: TObject);
    procedure RemoveFlag3Click(Sender: TObject);
    procedure DeleteMailbox1Click(Sender: TObject);
    procedure Createmailbox1Click(Sender: TObject);
    procedure CreateSubMailbox1Click(Sender: TObject);
    procedure ListView1ColumnClick(Sender: TObject; Column: TListColumn);
    procedure ListView1DrawHeader(Control: TWinControl;
      var ACanvas: TCanvas; Index: Integer; var ARect: TRect;
      Selected: Boolean; var DefaultDrawing: Boolean);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SpeedButton4Click(Sender: TObject);
    procedure RenameMailbox1Click(Sender: TObject);
    procedure MsgStatusBarMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TileHorizontally1Click(Sender: TObject);
    procedure TileVertically1Click(Sender: TObject);
    procedure About2Click(Sender: TObject);
    procedure DeleteandExpunge1Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure ListView1DblClick(Sender: TObject);
    procedure ListView1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure TreeList1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure AExportToCSVExecute(Sender: TObject);
    procedure AExitExecute(Sender: TObject);
    procedure ACheckSizeExecute(Sender: TObject);
    procedure ACheckQuotaExecute(Sender: TObject);
    procedure AAccountBackupExecute(Sender: TObject);
    procedure ALoginExecute(Sender: TObject);
    procedure ALogoutExecute(Sender: TObject);
    procedure AListSortedExecute(Sender: TObject);
    procedure AGlobalSearchExecute(Sender: TObject);
    procedure AHelpExecute(Sender: TObject);
    procedure ACancelActionExecute(Sender: TObject);
    procedure AShowDestinationDialogExecute(Sender: TObject);
    procedure AFocusAccountExecute(Sender: TObject);
    procedure AFocusSearchExecute(Sender: TObject);
    procedure AFocusTreeExecute(Sender: TObject);
    procedure AFocusListExecute(Sender: TObject);
    procedure AOptionsExecute(Sender: TObject);
    procedure SaveMessageHeaders1Click(Sender: TObject);
    procedure AAdvancedSearchExecute(Sender: TObject);
    procedure FrommboxUnixmailbox1Click(Sender: TObject);
    procedure Fromemlfiles1Click(Sender: TObject);
    procedure Tounixmailboxmbox1Click(Sender: TObject);
    procedure Toemlfiles1Click(Sender: TObject);
    procedure ToUnixMailboxmbox2Click(Sender: TObject);
    procedure Toemlfiles2Click(Sender: TObject);
    procedure eml2mbox1Click(Sender: TObject);
    procedure mbox2eml1Click(Sender: TObject);
    procedure ClearAccountCache1Click(Sender: TObject);
    procedure New1Click(Sender: TObject);
    procedure ChangeDestinationMailboxMboxMenuClick(Sender: TObject);
    procedure CopyToDestinationMenuItemClick(Sender: TObject);
    procedure CopyDeleteToDestinationMenuItemClick(Sender: TObject);
    procedure MoveToDestinationMenuItemClick(Sender: TObject);
    procedure CopyToChooseDestinationMenuItemClick(Sender: TObject);
    procedure CopyDeleteChooseDestinationMenuItemClick(Sender: TObject);
    procedure MoveChooseDestinationMenuItemClick(Sender: TObject);
    procedure TreeList1DblClick(Sender: TObject);
    procedure IMAPConsole1Click(Sender: TObject);
    procedure Activity1Click(Sender: TObject);
    procedure FAQ1Click(Sender: TObject);
    procedure News1Click(Sender: TObject);
    procedure DeleteAttachments1Click(Sender: TObject);
    procedure KeyboardShortcuts1Click(Sender: TObject);
    procedure FolderSubscriptions1Click(Sender: TObject);
    procedure ServerParameters1Click(Sender: TObject);
    procedure Answered1Click(Sender: TObject);
    procedure Answered2Click(Sender: TObject);
    procedure ReplicateFolderHierarchy1Click(Sender: TObject);
    procedure SaveAttachmentsLocally1Click(Sender: TObject);
    procedure eml2mboxes1Click(Sender: TObject);
    procedure RestoreBackup1Click(Sender: TObject);
    procedure mboxInspector1Click(Sender: TObject);
    procedure UpgradeBackups1Click(Sender: TObject);
  private
    sortArrows: TSortArrowBMPs;
    treePaintCS: TRTLCriticalSection;
    TreeLastClick: TPoint;
    function GetSelectedMsgs: TStringList;
    procedure GetSelectedMsgsInfos(var msgInfoTitles,msgInfoFroms,msgInfoDates: TStringList);
    procedure PerformSearchIMAPOperation(operation: TIMAPOperation; mailboxIndex: Integer; searchString: String);
    procedure PerformCopyIMAPOperation(operation: TIMAPOperation; mailboxIndex: Integer; destMailboxIndex: Integer; msgUIDs: TStringList);
    procedure PerformRemoteCopyIMAPOperation(mailboxIndex: Integer; msgUIDs: TStringList; destFullMailboxName: String; copyOperation: TCopyOperation);
    procedure PerformSearch(operation: TIMAPOperation);
    procedure PerformCopy(operation: TIMAPOperation; copyOperation: TCopyOperation);
    procedure PerformMessagePrepare(mailboxIndex: Integer);
    procedure PerformRenameIMAPOperation(mailboxIndex: Integer; newNodeName, newFullDisplayedName, newFullMailboxName: String);
    procedure PerformSaveHeaders(displayedMailboxIndex: Integer; msgUIDs: TStringList);
    procedure DownloadToUnixMbox;
    procedure DownloadToEMLFiles;
    procedure PerformUploadMbox;
    procedure PerformUploadEMLs;
    procedure PerformDownloadMbox(fullMailboxName: String; msgUIDs: TStringList);
    procedure PerformDownloadEMLs(fullMailboxName: String; msgUIDs: TStringList);
    procedure PerformDeleteAttachments(mailboxIndex: Integer; msgUIDs: TStringList);
    procedure PerformLoadFolderHierarchy;
    procedure HandleRequestOver(var Message: TMessage); message WM_REQUEST_OVER;
    procedure HandleUpdateTreeWithSizeCalcNoImg(var Message: TMessage); message WM_UPDATE_TREE_SIZE_CALC_NO_IMG; // Recalculate and update tree view
    procedure HandleUpdateTreeWithSizeCalc(var Message: TMessage); message WM_UPDATE_TREE_SIZE_CALC;
    procedure HandleUpdateMessageList(var Message: TMessage); message WM_UPDATE_MESSAGE_LIST;
    procedure HandleUpdateMainProgressBar(var Message: TMessage); message WM_PROGRESS_BAR_UPDATE;
    procedure HandleProgressBarVisibility(var Message: TMessage); message WM_MAIN_PROGRESS_BAR_VISIBILITY;
    procedure HandleShowQuota(var Message: TMessage); message WM_SHOW_QUOTA;
    procedure HandleHierarchyChange(var Message: TMessage); message WM_HIERARCHY_CHANGE;

    function CreateRequestAndSetMainParams(threadType: TThreadType; requestType: TRequestType): PRequest;
    procedure ShowAppBusyMessage;
    procedure CheckSize;
    procedure CheckQuota;
    procedure AccountBackup;
    procedure CancelAction;
    procedure ViewOptions;
    procedure InvokeGlobalSearchDlg(searchStr: String);
    procedure InvokeAdvSearchDlg;
    procedure InvokeFolderSubsDlg;
    procedure InvokeSaveAttachments;
    procedure InvokeSortBySize;
    procedure FormCSVStrings(var csvStrings: TStringList);
    procedure CancelSearch;
    procedure GIFAnimate(start: Boolean);

    // Methods for actions on the TreeView
    function GetIndexSelectedNodeTree: Integer;
    function IsTreeNodeSelected: Boolean;
    function GetNewMailboxName(currentName: String) : String;
    function ChangeDeepestMailboxName(mailboxName: String; newNodeName: String):String;
    procedure RetrieveMailboxMessages;
    procedure MarkAllMessagesDeleted;
    procedure MarkAllMessagesUndeleted;
    procedure ExpungeSelectedMailbox;
    procedure DeleteAndExpungeSelectedMailbox;
    procedure CreateMailbox;
    procedure CreateSubmailbox;
    procedure RenameMailbox;
    procedure DeleteMailbox;
    procedure ListSubMailboxesBySize;
    procedure FullExpandNode;
    procedure FullCollapseNode;

    // Methods for actions on the ListView
    procedure PeekMessage;
    procedure DeleteFlagSet;
    procedure DeleteFlagReset;
    procedure SeenFlagSet;
    procedure SeenFlagReset;
    procedure FlaggedFlagSet;
    procedure FlaggedFlagReset;
    procedure AnsweredFlagSet;
    procedure AnsweredFlagReset;
    procedure DeleteSelectedMsgsAndExpunge;
    procedure ExpungeThisMailbox;
    procedure CopyMessages;
    procedure CopyDeleteMessages;
    procedure CopyDeleteExpungeMessages;
    procedure ReloadFromServer;
    procedure SaveMessageHeaders;
    procedure QSDeleteAttachments;

    // Methods for main menu actions
    procedure ExportToCSV;
    procedure Eml2Mbox;
    procedure Eml2Mboxes;
    procedure Mbox2Eml;

    function ShowDestinationMailboxDlg: Boolean;
    procedure SaveSettingsToXMLString(var appSettingsStr: TStrings);

    // Backup related
    procedure InitializeBackupDB;
    procedure StartCommandLineBackupMode;
    procedure InvokeCommandLineBackup;
    procedure TerminateBackup(errorMsg: String);
    procedure CommandLineBackupModeCloseResources;


  public
    pActiveAccount: PAccountInfo;    // Pointer to the currently active account
    pDstAccount: PAccountInfo;       // Pointer to a seconde account
    mFirstPass: Boolean;             // True if this is the first pass on program startup
    mFilename: String;               // Filename of the file that is currently being processed
    mInitNodeDataCnt: Integer;       // Global counter for OnInitData
    mSelectedNodeIndex: Integer;     // Index in mboxTree.Items of the selected index in the TreeList
    mSelectedNode: PVirtualNode;     // Selected node
    mSeparator: Char;                // Separator for the current account
    mNS: TNamespace;                 // Namespace of the currently active (displayed) account

    mAccountMenuItems: array[0..50] of TMenuItem; //@todo posebna klasa sa dinamickim arrayom
    mSplitHorz: Boolean;
    xmlParser: TmiddleXQuickRTTI;
    mLastSizeCheckedAccount: String;  // Name of the last account checked (set in CheckerThread)
    mLocalDestinationMailboxIndex: Integer;
    mMsgUIDs: TStringList;                //@todo need this

    backupDb: TBackupDB;

    // Global, singletons
    FGlobalSearch: TFGlobalSearch;
    FSaveAttachmentsDlg: TFSaveAttachmentsDlg;
    FAdvSearchDlg: TFAdvSearchDlg;

    procedure ClearTreeList;
    procedure PopulateTree;
    procedure SaveExpandedState;
    procedure RepaintTree(setImages: Boolean);
    function GetCaptionForListNodes: String;
    function SetActiveAccount: Boolean;
    procedure PopulateMessageList(mailboxIndex: Integer; searched: Boolean);
    procedure AddMessagesToMessageList(mailboxIndex: Integer; first,last: Integer; clear: Boolean);
    procedure UpdateMessageList;
    procedure ClearMessageList;
    procedure RecalculateSizesAndDisplay(calcImages: Boolean);
    procedure ShowQuota(requestPtr: PRequest);
    procedure ShowMessageOnStatusBar(msg: String; panelIndex: Integer);
    function GetAbsoluteMboxIndex(FullMailboxName: String): Integer;
    function GetSeparator: String;
    function EscapeString(s: String; escape: Boolean): String;

    procedure SetDestinationMailbox(dmBoxIndex: Integer);
    procedure ResetDestinationMailboxIndex;
    procedure SetInitialDestinationMailbox;

    procedure VCLAdaptIMAPWorkerRunning;
    procedure VCLAdaptIMAPWorkerStopped;
    procedure VCLAdaptCheckerThreadRunning;
    procedure VCLAdaptCheckerThreadStopped;
    procedure VCLAdaptLoginThreadRunning;
    procedure VCLAdaptLoginThreadStopped;
    procedure VCLAdaptConverterThreadRunning;
    procedure VCLAdaptConverterThreadStopped;

    procedure FillAccountCombo;
    procedure PrepareGUIForSizeCheck;
    procedure PerformMessageIMAPOperation(operation: TIMAPOperation; mailboxIndex: Integer; msgUIDs: TStringList);
    procedure PerformMultipleFetch(operation: TIMAPOperation; mailboxIndex: Integer; msgUIDs: TStringList);
    procedure PerformIMAPOperation(operation: TIMAPOperation; mailboxIndex: Integer);
    procedure EnableCSVExport(enable: Boolean);
    procedure Login;
    procedure Logout;

    procedure SortInvoked(NewSortField: TMsgSortField);

    procedure LogNodesArray(msg: String);
    procedure SplitHorz;
    procedure SplitVert;
  end;



var
  FMain: TFMain;
  dataDir: String;                 // Directory where all the data is being written to
                                   // (specified in the dataDir command line param, or application path if not present)
  consoleMode: Boolean;            // True if the app is running in console mode (non-interactive, without the main frame)
  appMode: String;                 // Mode in which the app is running (backup, etc)
  accBackup: String;               // Account to backup (when invoked from the command line)
  backupRunning: Boolean;          // Used to prevent simultaneous multiple backups
  lg: TLog;                        // Used for line-to-line debug logging (writes to file strait away)
  vcLog: TVecLog;                  // Used for normal app & dev logging

  mboxTree: TMailboxTree;          // Model for the tree view
  mMessageLists: TMessageLists;    // List of lists of messages (one list per mailbox)
  mDisplayedNodeIndex: Integer;    // Index in mboxTree.Items of the node whose messages are displayed in the msg list
  mSearchDisplayedInList: Boolean; // True if the message list displays a search result
  mMsgSortField: TMsgSortField;    // Current message sort field (starts with msfNone)
  mMsgSortDir: TMsgSortDir;        // Sort direction
  mLastColumnClicked: Integer;     // Workaround for drawing arrows... crazy component this enhanced list
  mSelectedAccountIndex: Integer;  // Index of the currently selected account in the account combo box
  mCache: TICache;

  treeState: TMailboxTreeState;    // Visual state of the mailbox tree
  settings: TAppSettings;
  optionsDlgActiveTab: Integer;    // Active tab in OptionsDlg
  srchKeys: TSearchKeys;           // static info about the advanced search criteria

  requestMgr: TRequestManager;     // Global request manager...


implementation

uses Options, About, ListNodes, AddAccountDlg, MsgPeeker, NewMailboxDlg,
  DestinationChooser, Mbox2EmlDlg, Eml2MboxDlg, Eml2MboxesDlg, ActivityDlg, MyUtil, ShellApi,
  TextViewer, MailUtil, FolderSubDlg, AccountBackupDlg, RestoreBackupDlg,
  MboxInspectorDlg, UpgradeBackupsDlg;

{$R *.DFM}
{$R gifs.RES}  // This resource file contains GIFs used in the activity dialog


procedure TFMain.FormCreate(Sender: TObject);
var settingsFileName: String; appSettingsStr: TStrings;
begin
    if IsDebugDefined then lg:=TLog.Create(FMain);
    // If the dataDir parameter is specified in the command line, use it.
    // Otherwise use the application directory
    if not GetParameterValue(CL_SWITCH_DATA_DIR, ['/', '-'], '=', dataDir)
    then dataDir:=ExtractFileDir(Application.ExeName);

    // Load settings from file
    settingsFileName:=dataDir+'\imapsize.xml';
    appSettingsStr:=TStringList.Create;
    try
        xmlParser:=TmiddleXQuickRTTI.create;
        settings:=TAppSettings.create(xmlParser);
        if FileExists(settingsFileName) then begin
            appSettingsStr.LoadFromFile(settingsFileName);
            settings.LoadFromXML('AppSettings',appSettingsStr.text);
        end;
        // else settings will get populated with default values from AppSettings
    finally
        appSettingsStr.Free;
    end;

    vcLog := TVecLog.Create(FMain,dataDir);

    New(pActiveAccount);
    New(pDstAccount);
    requestMgr := TRequestManager.Create;

    if settings.MiscSettings.BackupExists then begin
        try
            backupDB:=TBackupDB.Create(settings.MiscSettings.BackupDir);
        except
            // The backup folder was deleted manually, reset the setting
            settings.MiscSettings.BackupExists:=false;
        end;
    end;

    consoleMode:=false;

    // Check if the backup mode has been specified in the parameters
    if GetParameterValue(CL_SWITCH_BACKUP_MODE, ['/','-'], '=', appMode) then begin
         StartCommandLineBackupMode;
         consoleMode:=true;
    end;
    backupRunning:=false;

    mFirstPass:=true;

    fillAccountCombo;

    mboxTree := TMailboxTree.Create;
    mMessageLists := TMessageLists.Create;
    mMsgUIDs:=TStringList.Create;
    mCache := TICache.Create(dataDir);

    treeState.content:=tcEmpty;
    treeState.account:=nil;

    Application.HelpFile:=ChangeFileExt(Application.ExeName,'.hlp');

    FMain.Top:=settings.Positions.Top;
    FMain.Left:=settings.Positions.Left;
    FMain.Width:=settings.Positions.Width;
    FMain.Height:=settings.Positions.Height;
    if settings.Positions.Maximized then FMain.WindowState:=wsMaximized
    else FMain.WindowState:=wsNormal;
    XPMenu1.Active:=settings.MiscSettings.XPMenus;

    ProgressBar1.Width:=PROG_BAR_WIDTH;

    if settings.Positions.SplitHorz then begin
        SplitHorz;
        TreeList1.Height:=settings.Positions.SplitPos;
    end
    else begin
        SplitVert;
        TreeList1.Width:=settings.Positions.SplitPos;
    end;

    with TreeList1.Header do begin
        Columns[0].Width:=settings.TreeDisplay.Column0Width;
        Columns[1].Width:=settings.TreeDisplay.Column1Width;
        Columns[2].Width:=settings.TreeDisplay.Column2Width;
        Columns[3].Width:=settings.TreeDisplay.Column3Width;
        Columns[4].Width:=settings.TreeDisplay.Column4Width;
        Columns[5].Width:=settings.TreeDisplay.Column5Width;
    end;

    with ListView1 do begin
        Columns[0].Width:=settings.ListDisplay.Column0Width;
        Columns[1].Width:=settings.ListDisplay.Column1Width;
        Columns[2].Width:=settings.ListDisplay.Column2Width;
        Columns[3].Width:=settings.ListDisplay.Column3Width;
        Columns[4].Width:=settings.ListDisplay.Column4Width;
        Columns[5].Width:=settings.ListDisplay.Column5Width;
        Columns[6].Width:=settings.ListDisplay.Column6Width;
    end;

    mMsgSortField:= intToMsgSortField(settings.ListDisplay.MsgSortField);
    mMsgSortDir := intToMsgSortDir(settings.ListDisplay.MsgSortDir);
    sortArrows:=TSortArrowBMPs.Create(ListView1.Font);
    mLastColumnClicked := settings.ListDisplay.MsgSortField;

    srchKeys:=TSearchKeys.Create;

    mDisplayedNodeIndex:=-1;
    mSearchDisplayedInList:=false;
    optionsDlgActiveTab:=0;

    EnableCSVExport(false);
    SaveDialog1.InitialDir := dataDir;

    InitializeCriticalSection(treePaintCS);
end;

{ Invokes the app in backup mode (no gui) }
procedure TFMain.StartCommandLineBackupMode;
var i: Integer; folders: TStringList;
begin
    Application.Minimize;
    Application.ShowMainForm := false;
    backupLog.Info('===== Starting IMAPSize in backup mode =====');
    // Need the account name parameter
    if GetParameterValue(CL_SWITCH_ACCOUNT, ['/','-'], '=', accBackup) then begin
        if (settings.Accounts.accountExists(accBackup)) then begin
            pActiveAccount^ := settings.Accounts.getAccount(accBackup);
            if (pActiveAccount^.BackupFolders<>'') then begin
                backupLog.Info('Backing up account ['+accBackup+']. Folders to backup:');
                folders:=TStringList.Create;
                try
                    folders.Text:=pActiveAccount.BackupFolders;
                    for i:=0 to folders.Count-1 do begin
                        backupLog.Info(folders[i]);
                    end;
                finally
                    folders.Free;
                end;
                InvokeCommandLineBackup;
            end
            else begin
                TerminateBackup('Folders to backup are not defined for account ['+accBackup+']. Please define these from the Account/Backup menu in IMAPSize.');
            end;
        end
        else begin
            TerminateBackup('The account specified as the parameter ['+accBackup+'] does not exist.');
        end;
    end
    else begin
        TerminateBackup('Account name not provided. Please provide the account name in the -'+CL_SWITCH_ACCOUNT+' switch');
    end;
end;

{ We load the file here, when the form has been painted completely }
procedure TFMain.FormPaint(Sender: TObject);
var account: TAccountInfo;
begin
    if mFirstPass then begin
        if settings.Accounts.AccountItems.Count=0 then begin
            if MessageDlg('You don''t have any accounts specified. Would you like to add an account?',
            mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
                FAddAccountDlg := TFAddAccountDlg.Create(Application);
                mFirstPass:=false;
                if FAddAccountDlg.ShowModal = mrOK then begin
                    account:=TAccountInfo.Create;
                    FAddAccountDlg.getNewAccount(account);
                    settings.Accounts.addAccount(account);
                    account.Free;
                    FillAccountCombo;
                end;
                FAddAccountDlg.Free;
            end;
        end;
        //EnableDebugLog(settings.MiscSettings.DebugLog);
        if settings.MiscSettings.LoadFoldersStartup then PerformLoadFolderHierarchy;
        mFirstPass:=false;
    end;
end;

procedure TFMain.FormResize(Sender: TObject);
begin
    StatusBar1.Panels[0].Width:=220;
    StatusBar1.Panels[2].Width:=PROG_BAR_WIDTH+3;
    StatusBar1.Panels[3].Width:=ANIM_GIF_WIDTH+3;

    StatusBar1.Panels[1].Width:=StatusBar1.Width-StatusBar1.Panels[0].Width-StatusBar1.Panels[2].Width-StatusBar1.Panels[3].Width-5;

    //StatusBar1.Panels[0].Width:=StatusBar1.Width-200-150;
    GIFImage1.Left:=StatusBar1.Panels[0].Width+StatusBar1.Panels[1].Width+StatusBar1.Panels[2].Width+4;  // @TODO
    ProgressBar1.Left:=StatusBar1.Panels[0].Width+StatusBar1.Panels[1].Width+2;
end;

procedure TFMain.FillAccountCombo;
var i: Integer;
begin
    ComboBox1.Clear;
    for i:=0 to settings.Accounts.AccountItems.count-1 do begin
        ComboBox1.Items.Add((settings.Accounts.getAccount(i)).Name);
    end;
    // Ensure an account is selected (if there are any accounts)
    if (settings.Accounts.AccountItems.count > 0) then begin
        if (settings.Accounts.DefaultIndex = -1) then settings.Accounts.DefaultIndex := 0;
        ComboBox1.ItemIndex:=settings.Accounts.DefaultIndex;
        mSelectedAccountIndex := ComboBox1.ItemIndex;
    end;
end;

{ Associate node with data. This event is triggered once for each node but
  appears asynchronously, which means when the node is displayed not when it is added. }
procedure TFMain.TreeList1InitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var Data: PNodeRecord;
begin
    with Sender do begin
        if mInitNodeDataCnt<=mboxTree.Count then begin
            Data := GetNodeData(Node);
            Data.mNodeName := mboxTree.Items[mInitNodeDataCnt].mNodeName;
            Data.mImageIndex:=mboxTree.Items[mInitNodeDataCnt].mImageIndex;
            Data.mAbsoluteIndex:=mboxTree.Items[mInitNodeDataCnt].mAbsoluteIndex;

            Data.mNumMessages:=mboxTree.Items[mInitNodeDataCnt].mNumMessages;
            Data.mTotalNumMessages:=mboxTree.Items[mInitNodeDataCnt].mTotalNumMessages;

            Data.mSizeDisplay:=mboxTree.Items[mInitNodeDataCnt].mSizeDisplay;
            Data.mTotalSizeDisplay:=mboxTree.Items[mInitNodeDataCnt].mTotalSizeDisplay;
            Data.mPercentOfParent:=mboxTree.Items[mInitNodeDataCnt].mPercentOfParent;
        end;
    end;
    // Increment global counter for the array
    Inc(mInitNodeDataCnt);
end;

{ Display node data }
procedure TFMain.TreeList1GetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);
var CurrNodeRec: PNodeRecord;
begin
    { Get a pointer to the data we reserved and initialized in OnInitNode }

    CurrNodeRec := PNodeRecord( Sender.GetNodeData(Node));
    if Assigned(CurrNodeRec) then begin
        case Column of
          0: CellText := CurrNodeRec.mNodeName;
          1: CellText := IntToStr(CurrNodeRec.mNumMessages);
          2: CellText := IntToStr(CurrNodeRec.mTotalNumMessages);
          3: CellText := CurrNodeRec.mSizeDisplay;
          4: CellText := CurrNodeRec.mTotalSizeDisplay;
          5: CellText := IntToStr(CurrNodeRec.mPercentOfParent)+'%';
        end;
    end;

end;

{ Display node image }
procedure TFMain.TreeList1GetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var CurrNodeRec: PNodeRecord;
begin
    CurrNodeRec := PNodeRecord( Sender.GetNodeData(Node));
    if column=0 then begin
       if (Kind = ikNormal)  or (Kind = ikSelected) then
          ImageIndex:=CurrNodeRec.mImageIndex;
    end;
end;

{ Populates the tree with nodes }
procedure TFMain.PopulateTree;
var i: Integer;
    CurrNode: PVirtualNode;
begin
    // Reset the global counter for OnInitData
    mInitNodeDataCnt:=0;
    TreeList1.Clear;
    { Reserve data in each node for the data }
    TreeList1.NodeDataSize := SizeOf(TNodeRecord);
    TreeList1.RootNodeCount:=1;
    CurrNode:=TreeList1.GetFirst;

    // Add nodes to the tree
    for i:=0 to mboxTree.Count-1 do begin
        TreeList1.ChildCount[CurrNode] := mboxTree.Items[i].mNumOfChildren;
        CurrNode:=TreeList1.GetNext(CurrNode);
    end;

    // Set the expanded states
    CurrNode:=TreeList1.GetFirst;
    for i:=0 to mboxTree.Count-1 do begin
        TreeList1.Expanded[CurrNode]:=mboxTree.Items[i].mExpanded;
        CurrNode:=TreeList1.GetNext(CurrNode);
    end;
end;

procedure TFMain.TreeList1FreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var Data: PNodeRecord;
begin
    Data := Sender.GetNodeData(Node);
    // Explicitely free the string, the VCL cannot know that there is one but needs to free
    if Assigned(Data) then Finalize(Data^);
end;

{ Saves the expanded state of all nodes. Needed when refreshing icons... }
procedure TFMain.SaveExpandedState;
var CurrNode: PVirtualNode; i: Integer;
begin
    CurrNode:=TreeList1.GetFirst;
    for i:=0 to mboxTree.Count-1 do begin
        mboxTree.Items[i].mExpanded:=TreeList1.Expanded[CurrNode];
        CurrNode:=TreeList1.GetNext(CurrNode);
    end;
end;

procedure TFMain.ClearTreeList;
begin
    TreeList1.Clear;
    TreeList1.Repaint;
    Panel1.Repaint;
    treeState.content:=tcEmpty;
end;

{ Options }
procedure TFMain.TreeList1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var AnItem: PVirtualNode; p: TPoint; CurrNodeRec: PNodeRecord;
begin
    if Button=mbRight then begin
        AnItem := TreeList1.GetNodeAt(X,Y);

        if AnItem<>nil then begin
            TreeList1.Selected[AnItem]:=true;
            TreeList1.FocusedNode:=AnItem;
            CurrNodeRec := PNodeRecord( TreeList1.GetNodeData(AnItem));
            mSelectedNodeIndex:=CurrNodeRec.mAbsoluteIndex;
            if (not mboxTree.Items[mSelectedNodeIndex].mVirtual) then begin
                mSelectedNode:=AnItem;
                p.x := X; p.y:= Y;
                p := TreeList1.ClientToScreen(p);
                // Only allow deletion of leaf nodes
                // This is in order to avoid the mess with existing subfolders (see Delete command imap rfc)

                // Don't allow renaming of INBOX. Although this is permitted, the behaviour is
                // not obvious to the user... Might allow one day if asked...
                // CompareText compares without case sensitivity
                RenameMailbox1.Enabled := CompareText(mboxTree.Items[mSelectedNodeIndex].mFullMailboxName,'INBOX') <> 0;

                DeleteMailbox1.Enabled:=mboxTree.Items[mSelectedNodeIndex].mIsLeaf;
                CreateSubMailbox1.Enabled:=not mboxTree.Items[mSelectedNodeIndex].mNoInferiors;
                PopupMenu1.Popup(p.X,p.Y);
            end
            else begin
                mSelectedNode:=AnItem;
                p.x := X; p.y:= Y;
                p := TreeList1.ClientToScreen(p);
                CreateMailbox1.Visible:=(mSelectedNodeIndex=0);
                CreateSubmailbox2.Visible:=(mSelectedNodeIndex<>0); // Note that it is already virtual
                PopupMenu3.Popup(p.X,p.Y);
            end;
        end;
    end
    else begin
        TreeLastClick.X:=X;
        TreeLastClick.Y:=Y;
    end;
end;

{
  Sets the pActiveAccount pointer to point to the account
  that is currently active in ComboBox1.
  If no accounts are selected returns false }
function TFMain.SetActiveAccount: Boolean;
var accountName: String;
begin
    Result:=false;
    if (ComboBox1.ItemIndex <> -1) then begin
        accountName := ComboBox1.Items[ComboBox1.ItemIndex];
        pActiveAccount^ := settings.Accounts.getAccount(accountName);
        Result:=true;
    end;
end;

function TFMain.GetCaptionForListNodes: String;
begin
    Result:='Mailboxes sorted by size';
end;

{ Saves application settings into the specified string list.
  List needs to be created before calling this method and
  freed when it returns }
procedure TFMain.SaveSettingsToXMLString(var appSettingsStr: TStrings);
var SplitPos: Integer;
begin
    if mSplitHorz then SplitPos:=TreeList1.Height
                  else SplitPos:=TreeList1.Width;

    if FMain.WindowState = wsNormal then begin
       settings.Positions.Top:=FMain.Top;
       settings.Positions.Left:=FMain.Left;
       settings.Positions.Width:=FMain.Width;
       settings.Positions.Height:=FMain.Height;
       settings.Positions.Maximized:=false;
    end
    else if FMain.WindowState=wsMaximized then begin
       settings.Positions.Maximized:=true;
    end;

    settings.Positions.SplitHorz:=mSplitHorz;
    settings.Positions.SplitPos:=SplitPos;

    // Save tree view data
    with TreeList1.Header do begin
        settings.TreeDisplay.Column0Width:=Columns[0].Width;
        settings.TreeDisplay.Column1Width:=Columns[1].Width;
        settings.TreeDisplay.Column2Width:=Columns[2].Width;
        settings.TreeDisplay.Column3Width:=Columns[3].Width;
        settings.TreeDisplay.Column4Width:=Columns[4].Width;
        settings.TreeDisplay.Column5Width:=Columns[5].Width;
    end;
    // Save list view data
    with ListView1 do begin
        settings.ListDisplay.Column0Width:=Columns[0].Width;
        settings.ListDisplay.Column1Width:=Columns[1].Width;
        settings.ListDisplay.Column2Width:=Columns[2].Width;
        settings.ListDisplay.Column3Width:=Columns[3].Width;
        settings.ListDisplay.Column4Width:=Columns[4].Width;
        settings.ListDisplay.Column5Width:=Columns[5].Width;
        settings.ListDisplay.Column6Width:=Columns[6].Width;
    end;

    settings.ListDisplay.MsgSortField:=msfToInt(mMsgSortField);
    settings.ListDisplay.MsgSortDir:=msdToInt(mMsgSortDir);

    appSettingsStr.text:=settings.savetoxml('AppSettings'); 

    devLog.Trace('Settings saved');
end;

procedure TFMain.FormClose(Sender: TObject; var Action: TCloseAction);
var appSettingsStr: TStrings;
begin
    devLog.Trace('Shutting down...');
    CancelAction;
    Application.HelpCommand(HELP_QUIT,0); // Close the help if it is open

    // Hide the main window, perform rest of the shutdown in the background
    Hide;

    // Save app settings
    appSettingsStr:=TStringList.Create;
    SaveSettingsToXMLString(appSettingsStr);
    try
        // appSettingsStr.SaveToFile(ChangeFileExt(Application.ExeName,'.xml'));
        appSettingsStr.SaveToFile(dataDir+'\imapsize.xml');
    except
        MessageDlg('Error was found while trying to save application settings to '+ChangeFileExt(Application.ExeName,'.xml')+'. Please ensure the folder is writable',mtError,[mbOK],0);
    end;
    appSettingsStr.Free;

    devLog.Trace('Freeing resources');

    // Free any dialogs that will not be freed by FMain
    // (we created them and left them to hang around)
    if Assigned(FAdvSearchDlg) then FAdvSearchDlg.Free;
    if Assigned(FGlobalSearch) then FGlobalSearch.Free;
    // if Assigned(FSaveAttachmentsDlg) then FSaveAttachmentsDlg.Free;
    //if Assigned(FIMAPConsole) then FIMAPConsole.Free;

    // Free other resources
    if Assigned(backupDB) then backupDB.Free;
    mCache.Free;
    srchKeys.Free;
    requestMgr.Free;
    mboxTree.Free;
    mMsgUIDs.Free;
    sortArrows.Free;
    mMessageLists.Free;
    TreeList1.Clear;
    Dispose(pActiveAccount);
    Dispose(pDstAccount);
    DeleteCriticalSection(treePaintCS);
    devLog.Info('Resources freed. Ready to shutdown');
end;

procedure TFMain.FormDestroy(Sender: TObject);
begin
    vcLog.Free;
    if IsDebugDefined then lg.Free;
end;

{ Repaints the tree.
  @param setImages true if image calculations should be performed }
procedure TFMain.RepaintTree(setImages: Boolean);
var CurrNode: PVirtualNode;
begin
    mInitNodeDataCnt:=0;
    if setImages then mboxTree.SetImageIndexes;
    EnterCriticalSection(treePaintCS);
    CurrNode:=TreeList1.GetFirst;
    TreeList1.ReinitNode(CurrNode,true);
    TreeList1.Invalidate;
    TreeList1.Repaint;
    LeaveCriticalSection(treePaintCS);
end;

{ Recalculates the sizes of all mailboxes and displayes the refreshed tree. }
{ @param calcImages true if image calculation should be performed }
procedure TFMain.RecalculateSizesAndDisplay(calcImages: Boolean);
begin
    try
        mboxTree.CalculateSizes;
        RepaintTree(calcImages);
    except
        // do nothing, just catch exception. Can occur on app shutdown...
    end;
end;

procedure TFMain.ComboBox1Click(Sender: TObject);
begin
    settings.Accounts.DefaultIndex:=ComboBox1.ItemIndex;
end;

procedure TFMain.PopulateMessageList(mailboxIndex: Integer; searched: Boolean);
var i:Integer; msg: TMessageInfo; MsgStatusBarString: String;
begin

    MsgStatusBarString := 'Displayed';
    if searched then MsgStatusBarString:=MsgStatusBarString+' (Search)';
    MsgStatusBarString:=MsgStatusBarString+': '+mboxTree.Items[mailboxIndex].mFullDisplayedName;
    MsgStatusBar.Panels[MSG_STATUSBAR_DISPLAYED].Text:=MsgStatusBarString;

    ListView1.BeginUpdate;
    ListView1.Items.Clear;

    for i:=0 to mMessageLists.GetMailboxLength(mailboxIndex)-1 do begin
        msg:=mMessageLists.GetMessageInfo(mailboxIndex,i);
        with ListView1.Items.Add do begin
            Caption := msg.mFrom;
            ImageIndex := msg.mImageIndex;
            SubItems.Add(msg.mTo);
            SubItems.Add(msg.mSubject);
            SubItems.Add(msg.mDateStr);
            SubItems.Add(msg.mSizeDisplay);
            SubItems.Add(msg.mSpamScore);  //@todo ovo samo ako je value
            SubItems.Add(msg.mUID);
        end;
    end;
    ListView1.EndUpdate;
    mDisplayedNodeIndex:=mailboxIndex;
    mSearchDisplayedInList:=searched;
end;

{ Add several messages to the list.
  @param mailboxIndex mailbox where messages belong
  @param first, last: Indexes in mMessageLists of the first and last messages to be added
  @param clear true if the list should be cleared }
procedure TFMain.AddMessagesToMessageList(mailboxIndex: Integer; first,last: Integer; clear: Boolean);
var i:Integer; msg: TMessageInfo;
begin
    ListView1.BeginUpdate;
    if clear then ListView1.Items.Clear;
    for i:=first to last-1 do begin
        if i<mMessageLists.GetMailboxLength(mailboxIndex) then begin
            msg:=mMessageLists.GetMessageInfo(mailboxIndex,i);
            with ListView1.Items.Add do begin
                Caption := msg.mFrom;
                ImageIndex := msg.mImageIndex;
                SubItems.Add(msg.mTo);
                SubItems.Add(msg.mSubject);
                SubItems.Add(msg.mDateStr);
                SubItems.Add(msg.mSizeDisplay);
                SubItems.Add(msg.mSpamScore);  //@todo ovo samo ako je value
                SubItems.Add(msg.mUID);
            end;
        end;
    end;
    ListView1.EndUpdate;
    if clear then begin
        mDisplayedNodeIndex:=mailboxIndex;
        mSearchDisplayedInList:=false;
    end;
end;

procedure TFMain.UpdateMessageList;
begin
    ListView1.UpdateItems(0,mMessageLists.GetMailboxLength(mDisplayedNodeIndex)-1);
end;

procedure TFMain.ClearMessageList;
begin
    ListView1.BeginUpdate;
    ListView1.Items.Clear;
    ListView1.EndUpdate;
end;

{ This procedure is called from the request manager when a size check is
  about to be invoked. }
procedure TFMain.PrepareGUIForSizeCheck;
begin
    MsgStatusBar.Panels[MSG_STATUSBAR_DISPLAYED].Text:='';
    MsgStatusBar.Panels[MSG_STATUSBAR_DESTINATION].Text:='';
    ClearMessageList;
end;

{ Dumps the whole array of nodes to the log file }
procedure TFMain.LogNodesArray(msg: String);
var str: String; i: Integer;

    function GetBoolStr(bval: boolean): String;
    begin
        if bval then Result:='YES'
        else Result:='NO'
    end;

begin
    devLog.Trace('======================================');
    devLog.Trace('STARTING DUMP '+msg);
    for i:=0 to mboxTree.Count-1 do begin
        str:='Index: '+IntToStr(mboxTree.Items[i].mAbsoluteIndex);
        str:=str+' Name: '+mboxTree.Items[i].mNodeName;
        str:=str+' Level: '+IntToStr(mboxTree.Items[i].mLevel);
        str:=str+' Parent: '+IntToStr(mboxTree.Items[i].mParentIndex);
        str:=str+' Leaf: '+GetBoolStr(mboxTree.Items[i].mIsLeaf);
        str:=str+' Expanded: '+GetBoolStr(mboxTree.Items[i].mExpanded);
        str:=str+' NumChildren: '+IntToStr(mboxTree.Items[i].mNumOfChildren);
        str:=str+' ImageIndex: '+IntToStr(mboxTree.Items[i].mImageIndex);
        str:=str+' NumMsgs: '+IntToStr(mboxTree.Items[i].mNumMessages);
        str:=str+' TotalMsgs: '+IntToStr(mboxTree.Items[i].mTotalNumMessages);
        str:=str+' Size: '+IntToStr(mboxTree.Items[i].mSize);
        str:=str+' TotalSize: '+IntToStr(mboxTree.Items[i].mTotalSize);
        str:=str+' Percent: '+IntToStr(mboxTree.Items[i].mPercentOfParent);
        str:=str+' FullDisplayedName: '+mboxTree.Items[i].mFullDisplayedName;
        str:=str+' FullMailboxName: '+mboxTree.Items[i].mFullMailboxName;
        str:=str+' NoInferiors: '+BoolToStr(mboxTree.ITems[i].mNoInferiors);
        devLog.Trace(str);
    end;
    devLog.Trace('DUMP DONE');
    devLog.Trace('======================================');
end;

procedure TFMain.SpeedButton9Click(Sender: TObject);
begin
    // test here
end;

procedure TFMain.ListView1DrawItem(Control: TWinControl;
  var ACanvas: TCanvas; Index: Integer; ARect: TRect;
  State: TOwnerDrawState; var DefaultDrawing, FullRowSelect: Boolean);
var msg: TMessageInfo;
begin
    msg:=mMessageLists.GetMessageInfo(mDisplayedNodeIndex,Index);
    if not msg.mFlags.mSeen then ACanvas.Font.Style := ACanvas.Font.Style + [fsBold]
                     else ACanvas.Font.Style := ACanvas.Font.Style - [fsBold];
    if msg.mFlags.mDeleted then ACanvas.Font.Style := ACanvas.Font.Style + [fsStrikeOut]
                     else ACanvas.Font.Style := ACanvas.Font.Style - [fsStrikeOut];
    if settings.MessageDisplay.AnsweredItalics then
        if msg.mFlags.mAnswered then ACanvas.Font.Style := ACanvas.Font.Style + [fsItalic]
                         else ACanvas.Font.Style := ACanvas.Font.Style - [fsItalic];
    if msg.mFlags.mFlagged then ACanvas.Font.Color := settings.MessageDisplay.FlaggedColor
    else if msg.mIsSpam then ACanvas.Font.Color := settings.MessageDisplay.SpamColor
    else ACanvas.Font.Color := clBlack;

    FullRowSelect := TRUE;
    DefaultDrawing := TRUE;  // let the componet draw it for us.
end;

{ Stops any running threads }
procedure TFMain.CancelAction;
begin
    // When the user cancels the action we shall terminate all threads.
    // The thread termination doesn't have to end immediatelly, so
    // we set all the user interface as if the action(s) has stopped.
    // Threads will be marked as done and 'wasTerminated' and WILL SEND
    // a message for handling the end of an operation to the main form.
    // (So, HandleXXX methods will eventually be invoked)

    //if threadMgr.ActiveLoginThreadCounter>0 then VCLAdaptLoginThreadStopped;  //@todo login
    if threadMgr.ActiveCheckerThreadCounter>0 then VCLAdaptCheckerThreadStopped;
    if threadMgr.ActiveIMAPWorkerCounter>0 then VCLAdaptIMAPWorkerStopped;
    if threadMgr.ActiveConverterThreadCounter>0 then VCLAdaptConverterThreadStopped;

    // Tell the threads they should terminate
    devLog.Trace('Terminating all threads');
    threadMgr.terminateAllThreads;   // This decrements the active thread count
    devLog.Trace('All threads terminated');
end;

procedure TFMain.InvokeSortBySize;
begin
    FListNodes:=TFListNodes.Create(Application);
    FListNodes.Caption:=GetCaptionForListNodes;
    FListNodes.Top:=settings.Positions.LDTop;
    FListNodes.Left:=settings.Positions.LDLeft;
    FListNodes.Width:=settings.Positions.LDWidth;
    FListNodes.Height:=settings.Positions.LDHeight;
    if settings.Positions.LDMaximized then FListNodes.WindowState:=wsMaximized
    else FListNodes.WindowState:=wsNormal;
    FListNodes.ShowModal;
    FListNodes.Free;
end;


procedure TFMain.SplitHorz;
begin
    TreeList1.Align:=alTop;
    Splitter1.Align:=alTop;
    mSplitHorz:=true;
end;

procedure TFMain.SplitVert;
begin
    Splitter1.Align:=alLeft;
    TreeList1.Align:=alLeft;
    mSplitHorz:=false;
end;

procedure TFMain.ComboBox1DrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
begin
    try
        if odSelected in State then
            ComboBox1.Canvas.Brush.Color := clTeal
        else
            ComboBox1.Canvas.Brush.Color := clWindow;

        ComboBox1.Canvas.FillRect(Rect);

        if settings.Accounts.getAccount(Index).SSL then
            ImageList2.Draw(ComboBox1.Canvas, Rect.Left+2, Rect.Top+1, 0, true)
        else
            ImageList2.Draw(ComboBox1.Canvas, Rect.Left+2, Rect.Top+1, 1, true);

        Rect.Left := Rect.Left + 20; {move over some}
        DrawText(ComboBox1.Canvas.Handle,
              PChar(ComboBox1.Items[Index]),
              -1,
              Rect,
              DT_SINGLELINE or DT_VCENTER or DT_LEFT);
    except
        // Just catch exception if an account is not found (e.g. was removed etc)
        on Exception do ;
    end;
end;

procedure TFMain.ShowQuota(requestPtr: PRequest);
begin
    FQuotaDlg := TFQuotaDlg.Create(FMain);
    FQuotaDlg.SetValues(requestPtr^.accountInfoPtr^.Name, requestPtr^.data.quotaInfo.current, requestPtr^.data.quotaInfo.maximum);
    FQuotaDlg.ShowModal;
    FQuotaDlg.Free;
end;

{ Shows msg message on the panelIndex status bar }
procedure TFMain.ShowMessageOnStatusBar(msg: String; panelIndex: Integer);
begin
    StatusBar1.Panels[panelIndex].Text:=msg;
end;

procedure TFMain.ListView1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var listItem: TListItem; p: TPoint; destMbox: String;
begin
    if Button=mbRight then begin
        listItem := ListView1.GetItemAt(X,Y);
        if listItem <> nil then begin
            listItem.Selected:=true;
            p.x := X; p.y:= Y;
            p := ListView1.ClientToScreen(p);
            // Set text of copy items
            if FDestinationChooserDlg.HasData then begin
                destMbox:=FDestinationChooserDlg.GetDestinationString;
                CopyToDestinationMenuItem.Enabled:=true;
                CopyDeleteToDestinationMenuItem.Enabled:=true;
                MoveToDestinationMenuItem.Enabled:=true;
                CopyToDestinationMenuItem.Caption:='To ['+destMbox+']';
                CopyDeleteToDestinationMenuItem.Caption:='To ['+destMbox+']';
                MoveToDestinationMenuItem.Caption:='To ['+destMbox+']';
            end
            else begin
                CopyToDestinationMenuItem.Enabled:=false;
                CopyDeleteToDestinationMenuItem.Enabled:=false;
                MoveToDestinationMenuItem.Enabled:=false;
            end;
            PopupMenu2.Popup(p.X,p.Y);
        end;
    end;
end;

{ Sets the GIFImage (in)visible and starts/stops its animation }
procedure TFMain.GIFAnimate(start: Boolean);
begin
    GIFImage1.Visible:=start;
    GIFImage1.Animate:=start;
end;

procedure TFMain.VCLAdaptIMAPWorkerRunning;
begin
    ACancelAction.Enabled:=true;
    GIFAnimate(true);
end;

procedure TFMain.VCLAdaptIMAPWorkerStopped;
begin
    ACancelAction.Enabled:=threadMgr.areAnyThreadsRunning;
    GIFAnimate(false);
end;

procedure TFMain.VCLAdaptCheckerThreadRunning;
begin
    ProgressBar1.Visible:=true;
    GIFAnimate(true);
    ACancelAction.Enabled:=true;
    ACheckSize.Enabled:=false;
end;

procedure TFMain.VCLAdaptCheckerThreadStopped;
begin
    ProgressBar1.Visible:=false;
    GIFAnimate(false);
    ACancelAction.Enabled:=threadMgr.areAnyThreadsRunning;
    ACheckSize.Enabled:=true;
end;

procedure TFMain.VCLAdaptLoginThreadRunning;
begin
    ACancelAction.Enabled:=true;
    GIFAnimate(true);
end;

procedure TFMain.VCLAdaptLoginThreadStopped;
begin
    ACancelAction.Enabled:=threadMgr.areAnyThreadsRunning;
    // ACancelAction.Enabled:=false;
    GIFAnimate(false);
end;

procedure TFMain.VCLAdaptConverterThreadRunning;
begin
    ProgressBar1.Visible:=true;
    ACancelAction.Enabled:=true;
end;

procedure TFMain.VCLAdaptConverterThreadStopped;
begin
    ProgressBar1.Visible:=false;
    ACancelAction.Enabled:=threadMgr.areAnyThreadsRunning;
end;

procedure TFMain.ShowAppBusyMessage;
begin
    MessageDlg('An operation is being performed. Please stop it first, or wait for it to end',mtInformation,[mbOK],0);
end;

{ Private method that will create the request object and
  populate the general request fields.
}
function TFMain.CreateRequestAndSetMainParams(threadType: TThreadType; requestType: TRequestType): PRequest;
var requestPtr: PRequest;
begin
    // SetActiveAccount;
    New(requestPtr); // Will be freed by the request manager when the request completes...
    requestPtr^ := TRequest.Create(threadType);
    requestPtr^.requestType:=requestType;
    requestPtr^.requestComponent:=rcMain;
    requestPtr^.accountInfoPtr:=pActiveAccount;
    Result:=requestPtr;
end;

procedure TFMain.CheckSize;
var requestPtr: PRequest;
begin
    if SetActiveAccount then begin
        TreeList1.SetFocus;  // remove focus from the account box
        requestPtr := CreateRequestAndSetMainParams(thtChecker,rtCheckSize);
        requestPtr^.checkerOperation:=chOpCheckSize;
        requestMgr.InvokeRequest(requestPtr);
    end
    else begin
        MessageDlg('Can''t perform check. None of the accounts are selected!',mtWarning,[mbOK],0);
    end;
end;

{ This operation will authenticate the user and retrieve the mailbox hierarchy (no size check though) }
procedure TFMain.Login;
var requestPtr: PRequest;
begin
    if SetActiveAccount then begin
        requestPtr := CreateRequestAndSetMainParams(thtChecker,rtLogin);
        requestPtr^.checkerOperation:=chOpHierarchyOnly;
        requestMgr.InvokeRequest(requestPtr);
    end
    else begin
        MessageDlg('Can''t login until you select an account!',mtWarning,[mbOK],0);
    end;
end;

procedure TFMain.Logout;
var requestPtr: PRequest; performLogout: Boolean;
begin
    performLogout:=true;
    if SetActiveAccount then begin
        if requestMgr.IsAccountActive(pActiveAccount) then begin
            if MessageDlg('Some requests for this account are still running. Logout anyway?',mtConfirmation,mbYesNoCancel,0)<>mrYes then
                performLogout:=false;
        end;
        if performLogout then begin
            // The logout request will terminate any active requests for this account...
            requestPtr := CreateRequestAndSetMainParams(thtNone,rtLogout);
            CAncelAction; //@todo this should be done through the requestMGr for this account only
            requestMgr.InvokeRequest(requestPtr);
            ClearTreeList;
            ClearMessageList;
        end;
    end
    else MessageDlg('No account to logout from!',mtWarning,[mbOK],0);
end;

procedure TFMain.PerformIMAPOperation(operation: TIMAPOperation; mailboxIndex: Integer);
var requestPtr: PRequest;
begin
    requestPtr := CreateRequestAndSetMainParams(thtIMAPWorker,rtGeneralIMAP);
    // Set required data
    requestPtr^.imapOperation:=operation;
    requestPtr^.data.mailboxIndex:=mailboxIndex;
    // Invoke request
    requestMgr.InvokeRequest(requestPtr);
end;

procedure TFMain.PerformMessageIMAPOperation(operation: TIMAPOperation; mailboxIndex: Integer; msgUIDs: TStringList);
var requestPtr: PRequest;
begin
    requestPtr := CreateRequestAndSetMainParams(thtIMAPWorker,rtMessageIMAP);
    requestPtr^.imapOperation:=operation;
    requestPtr^.data.mailboxIndex:=mailboxIndex;
    requestPtr^.SetMsgUIDs(msgUIDs);
    requestMgr.InvokeRequest(requestPtr);
end;

procedure TFMain.CheckQuota;
var requestPtr: PRequest;
begin
    if SetActiveAccount then begin
        requestPtr := CreateRequestAndSetMainParams(thtIMAPWorker,rtGetQuota);
        requestPtr^.imapOperation:=iopGetQuota;
        requestMgr.InvokeRequest(requestPtr);
    end
    else begin
        MessageDlg('Can''t perform check. Please select an account!',mtWarning,[mbOK],0);
    end;
end;

procedure TFMain.PerformSearchIMAPOperation(operation: TIMAPOperation; mailboxIndex: Integer; searchString: String);
var requestPtr: PRequest;
begin
    requestPtr := CreateRequestAndSetMainParams(thtIMAPWorker,rtSearch);
    requestPtr^.imapOperation:=operation;
    requestPtr^.data.mailboxIndex:=mailboxIndex;
    requestPtr^.data.searchString := searchString;
    requestMgr.InvokeRequest(requestPtr);
end;

procedure TFMain.PerformCopyIMAPOperation(operation: TIMAPOperation; mailboxIndex: Integer; destMailboxIndex: Integer; msgUIDs: TStringList);
var requestPtr: PRequest;
begin
    requestPtr := CreateRequestAndSetMainParams(thtIMAPWorker,rtCopy);
    requestPtr^.imapOperation:=operation;
    requestPtr^.data.mailboxIndex := mailboxIndex;
    requestPtr^.data.destMailboxIndex := destMailboxIndex;
    requestPtr^.SetMsgUIDs(msgUIDs);
    requestMgr.InvokeRequest(requestPtr);
end;

procedure TFMain.PerformRemoteCopyIMAPOperation(mailboxIndex: Integer; msgUIDs: TStringList; destFullMailboxName: String; copyOperation: TCopyOperation);
var requestPtrUp, requestPtrDown: PRequest;
begin
    // First create the master request (uploader)
    requestPtrUp := CreateRequestAndSetMainParams(thtIMAPWorker,rtUploadEMLsToRemoteServer);
    requestPtrUp^.imapOperation:=iopUploadForRemoteCopy;
    requestPtrUp^.accountInfoPtr:=FDestinationChooserDlg.DestinationAccountPtr; // override active accountinfo pointer
    requestPtrUp^.data.fullMailboxName := destFullMailboxName;
    requestPtrUp^.data.copyOperation := copyOperation;
    requestPtrUp^.data.mailboxIndex:=mailboxIndex;  // Need this for undeletion
    requestPtrUp^.SetMsgUIDs(msgUIDs);
    requestMgr.InvokeRequest(requestPtrUp);
    // Now invoke the downloader
    requestPtrDown := CreateRequestAndSetMainParams(thtIMAPWorker,rtDownloadForRemoteCopy);
    requestPtrDown^.imapOperation:=iopDownloadForRemoteCopy;
    requestPtrDown^.data.mailboxIndex:=mailboxIndex;
    requestPtrDown^.data.masterRequestId:=requestPtrUp^.requestId;
    requestPtrDown^.SetMsgUIDs(msgUIDs);
    requestPtrDown^.data.copyOperation := copyOperation;
    requestPtrDown^.data.filename:=dataDir+'\temp';
    requestMgr.InvokeRequest(requestPtrDown);
end;

procedure TFMain.PerformRenameIMAPOperation(mailboxIndex: Integer; newNodeName, newFullDisplayedName, newFullMailboxName: String);
var requestPtr: PRequest;
begin
    SaveExpandedState;
    requestPtr := CreateRequestAndSetMainParams(thtIMAPWorker,rtRename);
    requestPtr^.imapOperation:=iopRenameMbox;
    requestPtr^.data.mailboxIndex := mailboxIndex;
    requestPtr^.data.newNodeName := newNodeName;
    requestPtr^.data.newFullDisplayedName := newFullDisplayedName;
    requestPtr^.data.newFullMailboxName := newFullMailboxName;
    requestMgr.InvokeRequest(requestPtr);
end;


{ @param mailboxIndex Index of the displayed mailbox. This method
   can only be called from the list (not from the tree) }
procedure TFMain.PerformSaveHeaders(displayedMailboxIndex: Integer; msgUIDs: TStringList);
var requestPtr: PRequest;
begin
    SaveDialog1.Filter := 'Text files (*.txt)|*.TXT';
    if SaveDialog1.Execute then begin
        requestPtr := CreateRequestAndSetMainParams(thtIMAPWorker,rtSaveHeaders);
        requestPtr^.imapOperation:=iopSaveMessageHeaders;
        requestPtr^.data.displayedMailboxIndex := displayedMailboxIndex;
        requestPtr^.SetMsgUIDs(msgUIDs);
        requestPtr^.data.filename:=SaveDialog1.Filename;
        requestMgr.InvokeRequest(requestPtr);
    end;
end;

{ Gets the body structure string of the message. The string is then
  set into the MessagePeeker and the dialog is open. }
procedure TFMain.PerformMessagePrepare(mailboxIndex: Integer);
var requestPtr: PRequest; msgInfo: TMessageInfo;
begin
    requestPtr := CreateRequestAndSetMainParams(thtIMAPWorker,rtPrepareForMessageViewing);
    requestPtr^.imapOperation:=iopGetBodyStructure;
    requestPtr^.data.mailboxIndex := mailboxIndex;
    requestPtr^.data.displayedMailboxIndex:=mDisplayedNodeIndex;
    msgInfo:=mMessageLists.GetMessageInfo(mDisplayedNodeIndex,ListView1.Selected.Index);
    CopyMessageInfo(msgInfo,requestPtr^.data.msgInfo);
    requestMgr.InvokeRequest(requestPtr);
end;

{ Performs retrieval of multiple (not necessarily all) messages in the mailbox }
procedure TFMain.PerformMultipleFetch(operation: TIMAPOperation; mailboxIndex: Integer; msgUIDs: TStringList);
var requestPtr: PRequest;
begin
    requestPtr := CreateRequestAndSetMainParams(thtIMAPWorker,rtFetchMultipleMsgs);
    requestPtr^.imapOperation:=operation;
    requestPtr^.data.mailboxIndex := mailboxIndex;
    requestPtr^.SetMsgUIDs(msgUIDs);
    requestMgr.InvokeRequest(requestPtr);
end;

{ Retrieves a TStringList of message UIDs (Strings) of selected items in the ListView }
function TFMain.GetSelectedMsgs: TStringList;
var msg: TMessageInfo; listIndex, numSelected: Integer;
begin
    numSelected:=ListView1.SelCount;
    mMsgUIDs.Clear;
    listIndex:=ListView1.Selected.Index;
    // List displays full list of messages in mailbox
    while (mMsgUIDs.Count < numSelected) do begin
        if ListView1.Items[listIndex].Selected then begin
            msg:=mMessageLists.GetMessageInfo(mDisplayedNodeIndex,listIndex);
            mMsgUIDs.Add(msg.mUID);
        end;
        Inc(listIndex);
    end;
    Result:=mMsgUIDs;
end;

{ Gets a list of strings, each describing one of the messages selected in the message list }
procedure TFMain.GetSelectedMsgsInfos(var msgInfoTitles,msgInfoFroms,msgInfoDates: TStringList);
var msg: TMessageInfo; listIndex, numSelected: Integer; formattedDate: String;
begin
    numSelected:=ListView1.SelCount;
    msgInfoTitles.Clear;
    msgInfoFroms.Clear;
    msgInfoDates.Clear;
    listIndex:=ListView1.Selected.Index;
    // List displays full list of messages in mailbox
    while (msgInfoTitles.Count < numSelected) do begin
        if ListView1.Items[listIndex].Selected then begin
            msg:=mMessageLists.GetMessageInfo(mDisplayedNodeIndex,listIndex);
            msgInfoTitles.Add('From: '+msg.mFrom+', Date: '+msg.mDateStr+', Subject: '+msg.mSubject);
            msgInfoFroms.Add(msg.mFrom);
            try
                DateTimeToString(formattedDate,'yyyymmdd_hhmmss',msg.mDate);
            except
                formattedDate:='Invalid_Date';
            end;
            msgInfoDates.Add(formattedDate);
        end;
        Inc(listIndex);
    end;
end;

procedure TFMain.SpeedButton1Click(Sender: TObject);
begin
    PerformSearch(iopSearch);
end;

procedure TFMain.SpeedButton4Click(Sender: TObject);
begin
   PerformSearch(iopSearchReverse);
end;

procedure TFMain.PerformSearch(operation: TIMAPOperation);
begin
    if mDisplayedNodeIndex>-1 then begin
        if Length(EditSearchString.Text)>0 then begin
            PerformSearchIMAPOperation(operation,mDisplayedNodeIndex,EditSearchString.Text);
        end
        else MessageDlg('Search string not specified!',mtInformation,[mbOK],0);
    end
    else
        InvokeGlobalSearchDlg(EditSearchString.Text);
        // MessageDlg('This search is mailbox related and there is no mailbox displayed. Please select a mailbox or use the Global Search functionality which can be invoked from the Account menu.',mtInformation,[mbOK],0);
end;

procedure TFMain.MarkSeen1Click(Sender: TObject);
begin
    PerformMessageIMAPOperation(iopSeenMsg,mDisplayedNodeIndex,GetSelectedMsgs);
end;

procedure TFMain.MarkUnseen1Click(Sender: TObject);
begin
    PerformMessageIMAPOperation(iopUnseenMsg,mDisplayedNodeIndex,GetSelectedMsgs);
end;

procedure TFMain.SpeedButton3Click(Sender: TObject);
begin
    CancelSearch;
end;

procedure TFMain.CancelSearch;
var ind: Integer;
begin
    if mMessageLists.HasDataForRecover then begin
        ind:=mMessageLists.RecoverOriginalSearched;
        if (ind <> -1) then PopulateMessageList(ind,false);
    end
    else begin
        // Need to call recover, to properly set all values
        ind:=mMessageLists.RecoverOriginalSearched;
        if MessageDlg('The full list of messages doesn''t exist on the client. Do you want to retreive the full list of messages for this mailbox?',
            mtConfirmation,[mbYes,mbNo],0) = mrYes then begin
            // Retreive full list of messages
            PerformIMAPOperation(iopGetMboxEnvelopes,ind);
        end
        else begin
            // Don't retreive list of messages, just display the empty mailbox
            if (ind <> -1) then PopulateMessageList(ind,false);
        end;
    end;
end;

{ dmBoxIndex is the absolute node index }
procedure TFMain.SetDestinationMailbox(dmBoxIndex: Integer);
var mboxName: String; index: Integer;
begin
    mLocalDestinationMailboxIndex:=0;
    MsgStatusBar.Panels[MSG_STATUSBAR_DESTINATION].Text:='';
    if (dmBoxIndex>-1) and (dmBoxIndex<mboxTree.Count) then begin
        mboxName:=FDestinationChooserDlg.GetFullDestinationFolderName;
        index:=GetAbsoluteMboxIndex(mboxName);
        if index <> -1 then begin
            mLocalDestinationMailboxIndex:=index;
        end;
        // MsgStatusBar.Panels[MSG_STATUSBAR_DESTINATION].Text:='Destination Mailbox: '+FDestinationChooserDlg.GetDestinationString;
    end;
end;

{ Recalculates the destination mbox index. Needed after a delete, create or rename }
{ Just changes the index, doesn't touch the name, except if the mailbox is not found }
procedure TFMain.ResetDestinationMailboxIndex;
var extrName: String; index: Integer;
begin
    try
        extrName:=FDestinationChooserDlg.GetFullDestinationFolderName;
    except
        extrName:='';
    end;

    if extrName<>'' then begin
        index:=GetAbsoluteMboxIndex(extrName);
        if index <> -1 then begin
            mLocalDestinationMailboxIndex:=index;
        end
        else begin
            mLocalDestinationMailboxIndex:=0;
            MsgStatusBar.Panels[MSG_STATUSBAR_DESTINATION].Text:='';
        end;
    end;
end;

procedure TFMain.SetInitialDestinationMailbox;
begin
    if mboxTree.Count > 1 then begin
        FDestinationChooserDlg.InitializeDestination(0);
        SetDestinationMailbox(1);
    end;
end;

procedure TFMain.PerformCopy(operation: TIMAPOperation; copyOperation: TCopyOperation);
var copy: Boolean;
begin
    copy:=true;
    if FDestinationChooserDlg.HasData then begin
        // Check if copying is local or remote
        if FDestinationChooserDlg.DestinationAccountPtr^.Name = pActiveAccount^.Name then begin
            // Local copying
            //dbg('*** mLocalDestinationMailboxIndex='+IntToStr(mLocalDestinationMailboxIndex));
            //dbg('*** mDisplayedNodeIndex='+IntToStr(mDisplayedNodeIndex));
            if mLocalDestinationMailboxIndex = mDisplayedNodeIndex then begin
                if MessageDlg('Message(s) will be copied to the same folder! Proceed anyway?',
                    mtConfirmation, [mbYes, mbNo], 0) <> mrYes then copy:=false;
            end;
            if operation = iopCopyDelExpunge then begin
                if MessageDlg('This will move selected message(s) AND expunge all deleted messages in the mailbox. Proceed?',
                    mtConfirmation, [mbYes, mbNo], 0) <> mrYes then copy:=false;
            end;
            if copy then
                PerformCopyIMAPOperation(operation,mDisplayedNodeIndex,mLocalDestinationMailboxIndex,GetSelectedMsgs);
        end
        else begin
            if copyOperation=tcoCopyDeleteExpunge then begin
                MessageDlg('For your own safety, it is currently not possible to invoke a copy operation to a remote server with expunging the original mailbox.'+
                           'Please use one of the other two copy methods and expunge the original folders manually once you are sure the messages have been copied successfully.',mtInformation,[mbOK],0);
            end
            else begin
                // Copying between servers
                if MessageDlg('You have selected to copy messages to a mailbox on a remote server ['+FDestinationChooserDlg.GetDestinationString+']. Proceed?',mtConfirmation,[mbYes,mbCancel],0) = mrYes then begin
                    PerformRemoteCopyIMAPOperation(mDisplayedNodeIndex,GetSelectedMsgs,FDestinationChooserDlg.GetFullDestinationFolderName,copyOperation);
                end;
            end;
        end;
    end
    else MessageDlg('Destination mailbox is not specified!',mtInformation,[mbOK],0);
end;

{ Retrieves the index of the specified mailbox in mNodesArray. -1 if not found }
function TFMain.GetAbsoluteMboxIndex(FullMailboxName: String): Integer;
var i: Integer; found: Boolean;
begin
    i:=1;  // Skipping account
    found:=false;
    while (not found) and (i<mboxTree.Count) do begin
        if mboxTree.Items[i].mFullDisplayedName = FullMailboxName then found:=true
        else Inc(i);
    end;
    if found then Result:=i
    else Result:=-1;
end;

procedure TFMain.PeekbothHeaderandBody1Click(Sender: TObject);
begin
    PeekMessage;
end;

{ Handles messages sent by the request manager when a request is over ( WM_REQUEST_OVER )}
procedure TFMain.HandleRequestOver(var Message: TMessage);
var i,requestID, success: Integer; request: TRequest; skipFinally: Boolean;
begin
    devLog.Trace('TFMain.HandleRequestOver');
    requestId:=Message.WParam;
    skipFinally:=false;
    try
        devLog.Trace('Request over: '+IntToStr(requestId));
        //@todo any additional processing goes here. Get the request by id and extract the request type if needed...

        requestId:=Message.WParam;
        request := requestMgr.GetRequest(requestId);
        success:=Message.LParam;
        if request.requestType = rtDebugServer then begin
            FTextViewer:=TFTextViewer.Create(Application);
            FTextViewer.Memo1.WordWrap:=true;
            FTextViewer.Memo1.ScrollBars:=ssVertical;
            for i:=0 to request.data.msgContent.Count-1 do FTextViewer.Memo1.Lines.Add(request.data.msgContent[i]);
            FTextViewer.PageControl1.ActivePageIndex:=TEXT_PAGE_IDX;
            FTextViewer.SetCaption('Server Parameters and Debug Info');
            FTextViewer.Show;
        end
        else if request.requestType = rtAccountBackupCommandLine then begin
            // Log and shutdown
            backupLog.Info('===== Backup performed. Shutting down IMAPSize =====');
            CommandLineBackupModeCloseResources;
            Application.Terminate;
            skipFinally:=true;
        end
        // Enable message view refresh on attachment deletion
        // Below method is slow when caching not enabled...
        else if request.requestType = rtMultipleQSDeletion then begin
            if settings.MiscSettings.EnableCaching then ReloadFromServer;
        end;
    finally
        if not skipFinally then begin
            requestMgr.RemoveRequest(requestId,false);
            devLog.Trace('TFMain.HandleRequestOver - request removed ID='+IntToStr(requestId));
        end;
    end;
end;

{ Handles message WM_SHOW_QUOTA }
procedure TFMain.HandleShowQuota(var Message: TMessage);
var requestPtr: PRequest;
begin
    devLog.Trace('TFMain.HandleShowQuota');
    requestPtr := PRequest(Message.WParam);
    ShowQuota(requestPtr);
end;

{ Handles message WM_HIERARCHY_CHANGE.
  This is a signal that a mailbox has been created, deleted, renamed... }
procedure TFMain.HandleHierarchyChange(var Message: TMessage);
begin
    mboxTree.CalculateSizes;
    mboxTree.SetImageIndexes;
    PopulateTree;
    ResetDestinationMailboxIndex;
end;

{ Handles message WM_UPDATE_TREE_SIZE_CALC_NO_IMG }
{ (Re)calculates sizes, doesn't set images, updates the tree }
procedure TFMain.HandleUpdateTreeWithSizeCalcNoImg(var Message: TMessage);
begin
    RecalculateSizesAndDisplay(false);
end;

{ Handles message WM_UPDATE_TREE_SIZE_CALC }
{ (Re)calculates sizes, DOES set images, updates the tree }
procedure TFMain.HandleUpdateTreeWithSizeCalc(var Message: TMessage);
begin
    RecalculateSizesAndDisplay(true);
end;

{ Handles message WM_UPDATE_MESSAGE_LIST
  Updates the message list with any new messages }
procedure TFMain.HandleUpdateMessageList(var Message: TMessage);
begin
    //dbg('*** TFMain.HandleUpdateMessageList');
    // @TODO Currently the information is stored in a single record in mMessageLists.
    // This will be implemented as a FIFO queue
    AddMessagesToMessageList(mMessageLists.pageDisplayInfo.mailboxIndex,mMessageLists.pageDisplayInfo.fromMessage,
                             mMessageLists.pageDisplayInfo.toMessage,mMessageLists.pageDisplayInfo.clear);
end;

{ Handles message WM_PROGRESS_BAR_UPDATE }
procedure TFMain.HandleUpdateMainProgressBar(var Message: TMessage);
var threadID, position: Integer;
begin
    // threadID:=Message.WParam;
    position := Message.LParam;
    ProgressBar1.Position := position;
    ProgressBar1.Repaint;
end;

{ message WM_MAIN_PROGRESS_BAR_VISIBILITY }
{ LParam - 0: not visible, 1: visible }
procedure TFMain.HandleProgressBarVisibility(var Message: TMessage);
var visible: Integer;
begin
    visible := Message.LParam;
    if visible=1 then ProgressBar1.Visible:=true
    else ProgressBar1.Visible:=false;
    ProgressBar1.Repaint;
end;


// TreeView popup menu click events
procedure TFMain.RetrieveMessagesinthisfolder1Click(Sender: TObject); begin RetrieveMailboxMessages; end;
procedure TFMain.MarkallMessagesDeleted1Click(Sender: TObject); begin MarkAllMessagesDeleted; end;
procedure TFMain.UndeleteAllMessages1Click(Sender: TObject); begin MarkAllMessagesUndeleted; end;
procedure TFMain.ExpungeMailbox1Click(Sender: TObject); begin ExpungeSelectedMailbox; end;
procedure TFMain.DeleteAllMessagesPermanently1Click(Sender: TObject); begin DeleteAndExpungeSelectedMailbox; end;
procedure TFMain.Createmailbox1Click(Sender: TObject); begin CreateMailbox; end;
procedure TFMain.CreateSubMailbox1Click(Sender: TObject); begin CreateSubmailbox; end;
procedure TFMain.RenameMailbox1Click(Sender: TObject); begin RenameMailbox; end;
procedure TFMain.DeleteMailbox1Click(Sender: TObject); begin DeleteMailbox; end;
procedure TFMain.Expandthisnode1Click(Sender: TObject); begin FullExpandNode; end;
procedure TFMain.Collapsethisnode1Click(Sender: TObject); begin FullCollapseNode; end;
procedure TFMain.Listarticlesofthisnodessortedbysize1Click(Sender: TObject); begin ListSubMailboxesBySize; end;
procedure TFMain.Tounixmailboxmbox1Click(Sender: TObject); begin DownloadToUnixMbox; end;
procedure TFMain.Toemlfiles1Click(Sender: TObject); begin DownloadToEMLFiles; end;
procedure TFMain.FrommboxUnixmailbox1Click(Sender: TObject); begin PerformUploadMbox; end;
procedure TFMain.Fromemlfiles1Click(Sender: TObject); begin PerformUploadEMLs; end;


procedure TFMain.DownloadToUnixMbox;
var dummy: TStringList;
begin
    dummy:=TStringList.Create;
    PerformDownloadMbox(mboxTree.Items[mSelectedNodeIndex].mFullMailboxName,dummy);
    dummy.Free;
end;

procedure TFMain.DownloadToEMLFiles;
var dummy: TStringList;
begin
    dummy:=TStringList.Create;
    PerformDownloadEMLs(mboxTree.Items[mSelectedNodeIndex].mFullMailboxName,dummy);
    dummy.Free;
end;

procedure TFMain.SetFlag1Click(Sender: TObject); begin DeleteFlagSet; end;
procedure TFMain.RemoveFlag1Click(Sender: TObject); begin DeleteFlagReset; end;
procedure TFMain.SetFlag2Click(Sender: TObject); begin SeenFlagSet; end;
procedure TFMain.RemoveFlag2Click(Sender: TObject); begin SeenFlagReset; end;
procedure TFMain.SetFlag3Click(Sender: TObject); begin FlaggedFlagSet; end;
procedure TFMain.RemoveFlag3Click(Sender: TObject); begin FlaggedFlagReset; end;
procedure TFMain.Answered1Click(Sender: TObject); begin AnsweredFlagSet; end;
procedure TFMain.Answered2Click(Sender: TObject); begin AnsweredFlagReset; end;
procedure TFMain.DeleteandExpunge1Click(Sender: TObject); begin DeleteSelectedMsgsAndExpunge; end;
procedure TFMain.ExpungeMailbox2Click(Sender: TObject); begin ExpungeThisMailbox; end;
procedure TFMain.CopyToDestinationMenuItemClick(Sender: TObject); begin CopyMessages; end;
procedure TFMain.CopyDeleteToDestinationMenuItemClick(Sender: TObject); begin CopyDeleteMessages; end;
procedure TFMain.MoveToDestinationMenuItemClick(Sender: TObject); begin CopyDeleteExpungeMessages; end;
procedure TFMain.CopyToChooseDestinationMenuItemClick(Sender: TObject);
    begin if ShowDestinationMailboxDlg then CopyMessages; end;
procedure TFMain.CopyDeleteChooseDestinationMenuItemClick(Sender: TObject);
    begin if ShowDestinationMailboxDlg then CopyDeleteMessages; end;
procedure TFMain.MoveChooseDestinationMenuItemClick(Sender: TObject);
    begin if ShowDestinationMailboxDlg then CopyDeleteExpungeMessages; end;
procedure TFMain.Reload1Click(Sender: TObject); begin ReloadFromServer; end;
procedure TFMain.SaveMessageHeaders1Click(Sender: TObject); begin SaveMessageHeaders; end;
procedure TFMain.DeleteAttachments1Click(Sender: TObject); begin QSDeleteAttachments; end;
procedure TFMain.SaveAttachmentsLocally1Click(Sender: TObject); begin InvokeSaveAttachments; end;


procedure TFMain.ToUnixMailboxmbox2Click(Sender: TObject);
begin
    PerformDownloadMbox(mboxTree.Items[mDisplayedNodeIndex].mFullMailboxName,GetSelectedMsgs);
end;

procedure TFMain.Toemlfiles2Click(Sender: TObject);
begin
    PerformDownloadEMLs(mboxTree.Items[mDisplayedNodeIndex].mFullMailboxName,GetSelectedMsgs);
end;

procedure TFMain.InvokeAdvSearchDlg;
begin
    if mDisplayedNodeIndex>-1 then begin
        // Once we load the dialog it keeps loaded
        // Will be freed on app exit if assigned
        if not Assigned(FAdvSearchDlg) then FAdvSearchDlg := TFAdvSearchDlg.Create(Self);
        if FAdvSearchDlg.ShowModal = mrOK then begin
            PerformSearchIMAPOperation(iopSearchAdvanced,mDisplayedNodeIndex,FAdvSearchDlg.SearchString);
        end;
    end
    else
        InvokeGlobalSearchDlg(EditSearchString.Text);
        // MessageDlg('This search is mailbox related and there is no mailbox displayed. Please select a mailbox or use the Global Search functionality which can be invoked from the Account menu.',mtInformation,[mbOK],0);
end;

procedure TFMain.InvokeGlobalSearchDlg(searchStr: String);
begin
    if SetActiveAccount then begin
        if not Assigned(FGlobalSearch) then begin
            FGlobalSearch := TFGlobalSearch.Create(FMain);
        end;
        FGlobalSearch.EditSearchString.Text:=searchStr;
        // Populate the mailbox three in the search dialog.
        // This will either be done from the mbox if the information is readilly available,
        // or the list will be retrieved from the server...
        // Check the method for more information on possible scenarios
        FGlobalSearch.PrepareMailboxList;
        // In any case display the dialog...
        FGlobalSearch.PageControl1.ActivePageIndex:=0;
        FGlobalSearch.Show;
    end
    else begin
        MessageDlg('Cannot perform a global search. Please select an account first!',mtWarning,[mbOk],0);
    end;
end;

procedure TFMain.InvokeFolderSubsDlg;
begin
    if SetActiveAccount then begin
        FFolderSubDlg := TFFolderSubDlg.Create(FMain);
        FFolderSubDlg.PrepareMailboxList;
        FFolderSubDlg.ShowModal;
        FFolderSubDlg.Free;
    end
    else begin
        MessageDlg('Cannot invoke the Folder Subscription dialog. Please select an account first!',mtWarning,[mbOk],0);
    end;
end;

procedure TFMain.InvokeSaveAttachments;
var msgInfoTitles,msgInfoFroms,msgInfoDates: TStringList; requestPtr: PRequest;
begin
    if Assigned(ListView1.Selected) then begin
        // PerformDeleteAttachments(mDisplayedNodeIndex, GetSelectedMsgs);
        if not Assigned(FSaveAttachmentsDlg) then begin
            FSaveAttachmentsDlg := TFSaveAttachmentsDlg.Create(FMain);
        end;
        FSaveAttachmentsDlg := TFSaveAttachmentsDlg.Create(FMain);
        // Save message descriptions
        msgInfoTitles:=TStringList.Create;
        msgInfoFroms:=TStringList.Create;
        msgInfoDates:=TStringList.Create;
        GetSelectedMsgsInfos(msgInfoTitles,msgInfoFroms,msgInfoDates);
        FSaveAttachmentsDlg.SetMsgTitles(msgInfoTitles);
        FSaveAttachmentsDlg.SetMsgFroms(msgInfoFroms);
        FSaveAttachmentsDlg.SetMsgDates(msgInfoDates);
        msgInfoTitles.Free;
        msgInfoFroms.Free;
        msgInfoDates.Free;
        FSaveAttachmentsDlg.SetParams(mboxTree.Items[mDisplayedNodeIndex].mFullMailboxName);
        GetSelectedMsgs;     // will set mMsgUIDs
        FSaveAttachmentsDlg.PrepareList(mDisplayedNodeIndex, mMsgUIDs);
        if FSaveAttachmentsDlg.ShowModal = mrOK then begin
            New(requestPtr);
            requestPtr^ := TRequest.Create(thtIMAPWorker);
            requestPtr^.requestType:=rtSaveAttachments;
            requestPtr^.requestComponent:=rcMain;
            requestPtr^.imapOperation:=iopSaveAttachments;
            requestPtr^.accountInfoPtr:=pActiveAccount;
            FSaveAttachmentsDlg.CreateAttSavingRequest(requestPtr);
            requestMgr.InvokeRequest(requestPtr);
        end;
        FSaveAttachmentsDlg.Free;
    end
    else begin
        MessageDlg('Cannot invoke Attachment Saving. Please select an account first!',mtWarning,[mbOk],0);
    end;
end;

procedure TFMain.ListView1ColumnClick(Sender: TObject; Column: TListColumn);
begin
    SortInvoked(intToMsgSortField(Column.Tag));
end;

procedure TFMain.SortInvoked(NewSortField: TMsgSortField);
begin

    // Determine sort direction.
    // Note: Column.Tags have to be in sync with TMsgSortField
    if ((mMsgSortField <> NewSortField) or (mMsgSortField = msfNone)) then begin
        mMsgSortDir := msdAsc;
    end
    else begin
        // if sort was on same field change direction
        if mMsgSortDir = msdDsc then mMsgSortDir:=msdAsc
        else mMsgSortDir:=msdDsc;
    end;

    // Set sorting field
    mMsgSortField := NewSortField;

    // If a mailbox is selected and displayed, resort it
    if (mDisplayedNodeIndex > -1) then begin
        mMessageLists.EnterCS;
        mMessageLists.SortMsgsInMailbox(mDisplayedNodeIndex,mMsgSortField,mMsgSortDir);
        mMessageLists.LeaveCS;
        PopulateMessageList(mDisplayedNodeIndex,mSearchDisplayedInList);
    end;

    // NOTE!!! Za ovo dole sam morao da prebacim tu fju u EnhListView iz protected u public
    // Ona je potrebna jer se nekako ocajno ponasao... Sad je OK.
    ListView1.InvalidateColumnHeader(mLastColumnClicked);
    ListView1.InvalidateColumnHeader(ord(NewSortField));
    mLastColumnClicked := ord(NewSortField);

end;

procedure TFMain.ListView1DrawHeader(Control: TWinControl;
  var ACanvas: TCanvas; Index: Integer; var ARect: TRect;
  Selected: Boolean; var DefaultDrawing: Boolean);
var Offset: integer;
begin
    // Draw the arrow in the header
    if not selected then begin
        if mMsgSortField<>msfNone then begin
            if ord(mMsgSortField)=Index then begin
                if mMsgSortDir = msdAsc then begin
                    Offset := (ARect.Bottom - ARect.Top - sortArrows.UpArrow.Height) div 2;
                    ACanvas.Draw(ARect.Left + 4, ARect.Top + Offset, sortArrows.UpArrow);
                    inc(ARect.Left, sortArrows.UpArrow.Width + 4);
                end
                else begin
                    Offset := (ARect.Bottom - ARect.Top - sortArrows.DownArrow.Height) div 2;
                    ACanvas.Draw(ARect.Left + 4, ARect.Top + Offset, sortArrows.DownArrow);
                    inc(ARect.Left, sortArrows.DownArrow.Width + 4);
                end;
            end;
        end;
    end;
    DefaultDrawing := TRUE;
end;

procedure TFMain.MsgStatusBarMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
    {if x< MsgStatusBar.Panels[MSG_STATUSBAR_DISPLAYED].Width+MsgStatusBar.Panels[MSG_STATUSBAR_DESTINATION].Width then begin
        ShowDestinationMailboxDlg;
    end;}
end;

{ Show dlg for changing the destination mailbox
  Returns true if a mailbox has actually been chosen (as oposed to pressing Cancel) }
function TFMain.ShowDestinationMailboxDlg: Boolean;
var index: Integer;
begin
    Result:=false;
    if FDestinationChooserDlg.ShowModal = mrOK then begin

        index := FDestinationChooserDlg.GetSelectedMboxIndex;
        if index > -1 then begin
            SetDestinationMailbox(index);
            Result:=true;
        end;
    end;
end;

procedure TFMain.ViewOptions;
var backupSettingsStr: TStrings;
begin
    // Make a backup of the settings in case user Cancels the operation
    backupSettingsStr:=TStringList.Create;
    SaveSettingsToXMLString(backupSettingsStr);

    FOptions:=TFOptions.Create(Application);
    FOptions.SetValues;
    FOptions.PageControl1.ActivePageIndex:=optionsDlgActiveTab;
    if (FOptions.ShowModal=mrOK) then begin
       //if FOptions.AccountDeleted then
       FillAccountCombo;
       if (FOptions.changeForReload) then begin
             mboxTree.FormatSizes;
             RepaintTree(true);
       end;
       if (FOptions.changeForMessageListReload) then begin
            if (mDisplayedNodeIndex<>-1) then begin
                mMessageLists.ReassignImageIndexes(mDisplayedNodeIndex);
                mMessageLists.ReformatSizeDisplay(mDisplayedNodeIndex);
                PopulateMessageList(mDisplayedNodeIndex, mSearchDisplayedInList);
            end;
       end;
       if (FOptions.logSettingsChanged) then begin
           vcLog.UpdateLoggingSettings;
       end;
       XPMenu1.Active:=settings.MiscSettings.XPMenus;
       FMsgPeeker.XPMenu1.Active:=settings.MiscSettings.XPMenus;
    end
    else begin
        // mrCancel - reload from backup
        settings.LoadFromXML('AppSettings',backupSettingsStr.text);
        SetActiveAccount;
    end;
    backupSettingsStr.Free;
    FOptions.Free;
end;

{ --------- Menu Actions --------- }

procedure TFMain.TileHorizontally1Click(Sender: TObject);
begin
    SplitHorz;
end;

procedure TFMain.TileVertically1Click(Sender: TObject);
begin
    SplitVert;
end;

procedure TFMain.About2Click(Sender: TObject);
begin
    FAbout:=TFAbout.Create(Application);
    FAbout.ShowModal;
    FAbout.Free;
end;

procedure TFMain.ComboBox1Change(Sender: TObject);
begin
    if ComboBox1.ItemIndex <> mSelectedAccountIndex then begin
        // Clear the tree and the message list
        if ListView1.Items.Count > 0 then ClearMessageList;
        ClearTreeList;
        // Disable export (will be set in the CheckerThread on successfull size calculation)
        EnableCSVExport(false);
        mSelectedAccountIndex := ComboBox1.ItemIndex;
    end;
end;

procedure TFMain.FormCSVStrings(var csvStrings: TStringList);
var i: Integer; str: String;
begin
    str:='Displayed Name, Full Mailbox Name, Number of Messages, Total Number of Messages, Size, Total Size, Percent of Parent';
    csvStrings.Add(str);
    for i:=0 to mboxTree.Count-1 do begin
        str:= mboxTree.Items[i].mFullDisplayedName + ',' +
              mboxTree.Items[i].mFullMailboxName + ',' +
              IntToStr(mboxTree.Items[i].mNumMessages) + ',' +
              IntToStr(mboxTree.Items[i].mTotalNumMessages) + ',' +
              IntToStr(mboxTree.Items[i].mSize) + ',' +
              IntToStr(mboxTree.Items[i].mTotalSize) + ',' +
              IntToStr(mboxTree.Items[i].mPercentOfParent) + '%';
        csvStrings.Add(str);
    end;
end;

procedure TFMain.EnableCSVExport(enable: Boolean);
begin
    ExportCSV.Enabled:=enable;
end;

procedure TFMain.ListView1DblClick(Sender: TObject);
begin
    if ListView1.SelCount > 0 then begin
        PerformMessagePrepare(mDisplayedNodeIndex);
    end;
end;

{ Action taken from FMain. Move these to Actions? }
procedure TFMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var msf: TMsgSortField;
begin
    if ssCtrl in Shift then begin
        // Keys for sorting parameters...
        if ((Key>=49) and (Key<=56)) then begin
            msf:=msfNone;
            case Key of
                49: msf:=msfFrom;
                50: msf:=msfTo;
                51: msf:=msfSubject;
                52: msf:=msfDate;
                53: msf:=msfSize;
                54: msf:=msfSpamScore;
                55: msf:=msfUID;
                56: msf:=msfNone;
            end;
            SortInvoked(msf);
        end;
    end;
end;

{ Actions taken when key is downed while in the search edit box }
procedure TFMain.EditSearchStringKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
    {
        RETURN: search text
        SHIFT-RETURN: display messages not containing text
        CTRL-RETURN: advanced search
        CTRL-ESC: cancel search
    }
    if (Key=VK_RETURN) then begin
        if (ssCtrl in Shift) then begin
            PerformSearch(iopSearchAdvanced)
        end
        else begin
            if (ssShift in Shift) then PerformSearch(iopSearchReverse)
            else PerformSearch(iopSearch);
        end;
    end
    else if (Key=VK_ESCAPE) then begin
        if (ssCtrl in Shift) then CancelSearch;
    end;
end;


{ --------------------------------------------}
{ ----------- TreeView Actions ---------------}
{ --------------------------------------------}

{ Action taken when key is downed in the tree list }
procedure TFMain.TreeList1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
    {
        RETURN      - Retrieve envelopes
        F2          - Delete all messages
        Ctrl+F2     - Undelete all messages
        Ctrl+E      - Expunge mailbox
        Ctrl+X      - Delete all and expunge
        Ctrl+Sh+C   - Create mailbox (root level)
        Ctrl+Sh+S   - Create Submailbox
        Ctrl+Sh+R   - Rename mailbox
        Ctrl+Sh+D   - Delete mailbox
        Alt+L       - List sorted by size
    }
    if (ssCtrl in Shift) then begin
        if (ssShift in Shift) then begin
            // Ctrl+Shift+
            if IsKeyDown(Key,'C') then CreateMailbox
            else if IsKeyDown(Key,'S') then CreateSubmailbox
            else if IsKeyDown(Key,'R') then RenameMailbox
            else if IsKeyDown(Key,'D') then DeleteMailbox;
        end
        else begin
            // Ctrl+
            if (Key=VK_F2) then MarkAllMessagesUndeleted
            else if IsKeyDown(Key,'E') then ExpungeSelectedMailbox
            else if IsKeyDown(Key,'X') then DeleteAndExpungeSelectedMailbox;
        end;
    end
    else if (ssAlt in Shift) then begin
        if IsKeyDown(Key,'L') then ListSubMailboxesBySize;
    end
    else begin
        // No Alt or Ctrl pressed
        if (Key=VK_RETURN) then RetrieveMailboxMessages
        else if (Key=VK_F2) then MarkAllMessagesDeleted;
    end;
end;

{ Retrieves the absolute index of the selected node.
  If no node is selected returns -1 }
function TFMain.GetIndexSelectedNodeTree: Integer;
var AnItem: PVirtualNode; CurrNodeRec: PNodeRecord; selIndex: Integer;
begin
    AnItem := TreeList1.GetFirstSelected;
    if AnItem<>nil then begin
        TreeList1.Selected[AnItem]:=true;
        TreeList1.FocusedNode:=AnItem;
        CurrNodeRec := PNodeRecord( TreeList1.GetNodeData(AnItem));
        selIndex:=CurrNodeRec.mAbsoluteIndex;
    end
    else begin
        selIndex:=-1;
    end;
    Result:=selIndex;
end;

{ Returns true if a node is selected in the TreeView.
  It sets mSelectedNodeIndex to the selected node index }
function TFMain.IsTreeNodeSelected: Boolean;
begin
    mSelectedNodeIndex:=GetIndexSelectedNodeTree;
    Result := (mSelectedNodeIndex <> -1);
end;

{ Retrieves all messages from the mailbox selected in the tree view }
procedure TFMain.RetrieveMailboxMessages;
begin
    if IsTreeNodeSelected then
        PerformIMAPOperation(iopGetMboxEnvelopes,mSelectedNodeIndex);
end;

{ Marks all messages in the selected mailbox as deleted }
procedure TFMain.MarkAllMessagesDeleted;
begin
    if IsTreeNodeSelected then
        PerformIMAPOperation(iopDeleteAllInMbox,mSelectedNodeIndex);
end;

{ Marks all messages in the selected mailbox as undeleted }
procedure TFMain.MarkAllMessagesUndeleted;
begin
    if IsTreeNodeSelected then
        PerformIMAPOperation(iopUndeleteAllInMbox,mSelectedNodeIndex);
end;

{ Expunges the selected mailbox }
procedure TFMain.ExpungeSelectedMailbox;
begin
    if IsTreeNodeSelected then
        PerformIMAPOperation(iopExpungeMbox,mSelectedNodeIndex);
end;

{ Deletes all messages in the selected mailbox and performs an expunge }
procedure TFMain.DeleteAndExpungeSelectedMailbox;
begin
    if IsTreeNodeSelected then begin
        if MessageDlg('This operation will PERMANENTLY delete all the messages in mailbox ['+
                       mboxTree.Items[mSelectedNodeIndex].mFullDisplayedName+
                       ']. Are you sure you want to proceed?',
                mtWarning, [mbYes, mbNo], 0) = mrYes then
            PerformIMAPOperation(iopDeleteAndExpungeAllInMbox,mSelectedNodeIndex);
    end;
end;

{ Private method - Returns an empty string if no mailbox should be created }
function TFMain.GetNewMailboxName(currentName: String) : String;
begin
    FNewMailboxDlg := TFNewMailboxDlg.Create(FMain);
    FNewMailboxDlg.SetInitialName(currentName);
    if FNewMailboxDlg.ShowModal = mrOK then begin
        Result := FNewMailboxDlg.Edit1.Text;
    end
    else Result:='';
end;

{ Creates a root mailbox }
procedure TFMain.CreateMailbox;
var newMbox: String; requestPtr: PRequest;
begin
    // Allow creation of a mailbox anytime, but only if mailboxes are displayed
    if mboxTree.Count>0 then begin
        newMbox:=GetNewMailboxName('');
        if Length(newMbox)>0 then begin
            if mNS.hasPersonalNS then
                newMbox := mNS.personalNSPrefix + newMbox;
            SaveExpandedState;

            requestPtr := CreateRequestAndSetMainParams(thtIMAPWorker,rtCreateMailbox);
            requestPtr^.imapOperation:=iopCreateMbox;
            requestPtr^.data.fullMailboxName := newMbox;  // New mbox name
            requestPtr^.data.mailboxIndex := mSelectedNodeIndex;  // Parents index
            requestMgr.InvokeRequest(requestPtr);
        end;
    end;
end;

{ Creates a mailbox as a child of the currently selected mailbox in the tree }
procedure TFMain.CreateSubmailbox;
var newMbox: String; requestPtr: PRequest;
begin
    if IsTreeNodeSelected then begin
        newMbox:=GetNewMailboxName('');
        if Length(newMbox)>0 then begin
            SaveExpandedState;
            devLog.Trace('Creating subfolder of '+mboxTree.Items[mSelectedNodeIndex].mFullMailboxName);
            if (mNS.hasPersonalNS) then begin
                if (mboxTree.Items[mSelectedNodeIndex].mFullMailboxName=mNS.personalNSPrefix) or
                   (mboxTree.Items[mSelectedNodeIndex].mFullMailboxName=mNS.personalNSPrefix+'Inbox') or
                   // mPersonalNamespacePrefix can have a dot at the end. Check
                   (mboxTree.Items[mSelectedNodeIndex].mFullMailboxName=Copy(mNS.personalNSPrefix,1,Length(mNS.personalNSPrefix)-1))
                then begin
                    // If it is a child of INBOX should add .Inbox (hack)
                    newMbox:=mNS.personalNSPrefix+'Inbox'+GetSeparator+newMbox;
                end
                else begin
                    newMbox:=mboxTree.Items[mSelectedNodeIndex].mFullMailboxName+GetSeparator+newMbox;
                end;
            end
            else begin
                newMbox:=mboxTree.Items[mSelectedNodeIndex].mFullMailboxName+GetSeparator+newMbox;
            end;

            devLog.Info('Will create new mbox: '+newMbox);

            requestPtr := CreateRequestAndSetMainParams(thtIMAPWorker,rtCreateMailbox);
            requestPtr^.imapOperation:=iopCreateMbox;
            requestPtr^.data.fullMailboxName := newMbox;
            requestPtr^.data.mailboxIndex := mSelectedNodeIndex;  // Parents index
            requestMgr.InvokeRequest(requestPtr);
        end;
    end;
end;

{ Renames the currently selected mailbox in the tree }
procedure TFMain.RenameMailbox;
var newMbox: String; newFullMailboxName, newFullDisplayedName: String; p: Integer;
begin
    if IsTreeNodeSelected then begin
        // Don't allow renaming if INBOX is selected...
        if (CompareText(mboxTree.Items[mSelectedNodeIndex].mFullMailboxName,'INBOX') <> 0) then begin
            newMbox:=GetNewMailboxName(mboxTree.Items[mSelectedNodeIndex].mNodeName);
            if Length(newMbox)>0 then begin
                p:= Pos(mSeparator,newMbox);
                if p=0 then begin
                    newFullDisplayedName := ChangeDeepestMailboxName(mboxTree.Items[mSelectedNodeIndex].mFullDisplayedName, newMbox);
                    newFullMailboxName := ChangeDeepestMailboxName(mboxTree.Items[mSelectedNodeIndex].mFullMailboxName, newMbox);
                    PerformRenameIMAPOperation(mSelectedNodeIndex, newMbox, newFullDisplayedName, newFullMailboxName);
                end
                else begin
                    MessageDlg('Please use a name without the hierarchy separator ['+mSeparator+']',mtWarning,[mbOK],0);
                end;
            end;
        end;
    end;
end;

{ Changes the last node name in the hierarchy. E.g. if the full mailbox name is
  one/two/three and the newNodeName is four then the result will be one/two/four
  mailboxName can be either mFullMailboxName or mFullDisplayedName }
function TFMain.ChangeDeepestMailboxName(mailboxName: String; newNodeName: String):String;
var newFullName: String; parts: TStringList; i: Integer;
begin
    parts := TStringList.Create;
    newFullName := '';
    try
        Split(EscapeString(mailboxName,false),mSeparator,parts);
        parts[parts.Count-1] := newNodeName;
        for i:=0 to parts.Count-1 do begin
            newFullName := newFullName + parts[i];
            if (i <> parts.Count-1) then newFullName := newFullName + GetSeparator;
        end;
    finally
        parts.Free;
        Result:=newFullName;
    end;
end;


{ Deletes the mailbox currently selected in the tree (must be a leaf) }
procedure TFMain.DeleteMailbox;
begin
    if IsTreeNodeSelected then begin
        // Only allow deletion of leaves
        if mboxTree.Items[mSelectedNodeIndex].mIsLeaf then begin
            if MessageDlg('This operation will PERMANENTLY delete the mailbox ['+
                           mboxTree.Items[mSelectedNodeIndex].mFullDisplayedName+
                           ']. Are you sure you want to proceed?',
                    mtWarning, [mbYes, mbNo], 0) = mrYes then
            begin
                SaveExpandedState;
                PerformIMAPOperation(iopDeleteMbox,mSelectedNodeIndex);
            end;
        end;
    end;
end;

{ Lists by size all mailboxes with the selected mailbox as root }
procedure TFMain.ListSubMailboxesBySize;
begin
    if IsTreeNodeSelected then begin
        FListNodes:=TFListNodes.Create(Application);
        FListNodes.Caption:=GetCaptionForListNodes;
        FListNodes.Top:=settings.Positions.LDTop;
        FListNodes.Left:=settings.Positions.LDLeft;
        FListNodes.Width:=settings.Positions.LDWidth;
        FListNodes.Height:=settings.Positions.LDHeight;
        if settings.Positions.LDMaximized then FListNodes.WindowState:=wsMaximized
        else FListNodes.WindowState:=wsNormal;
        FListNodes.SetPathFilter(mboxTree.Items[mSelectedNodeIndex].mFullDisplayedName);
        FListNodes.ShowModal;
        FListNodes.Free;
    end;
end;

procedure TFMain.PerformUploadMbox;
var requestPtr: PRequest;
begin
    ULOpenDialog.Title:='Select a mbox file with messages to upload';
    ULOpenDialog.InitialDir:=settings.MiscSettings.MboxDir;
    ULOpenDialog.Filter := 'Unix mailbox files (*.mbox)|*.MBOX|All files (*.*)|*.*';
    ULOpenDialog.Options:= ULOpenDialog.Options - [ofAllowMultiSelect];
    if ULOpenDialog.Execute then begin
        settings.MiscSettings.MboxDir:=ExtractFilePath(ULOpenDialog.Filename);
        //@todo Proveri velicinu fajla. Stavi upozorenje (opcije za velicinu)
        requestPtr := CreateRequestAndSetMainParams(thtIMAPWorker,rtUploadMbox);
        requestPtr^.imapOperation:=iopUploadMbox;
        requestPtr^.data.mailboxIndex:=mSelectedNodeIndex;
        requestPtr^.data.filename:=ULOpenDialog.filename;
        requestMgr.InvokeRequest(requestPtr);
    end;
end;

procedure TFMain.PerformUploadEMLs;
var requestPtr: PRequest;
begin
    ULOpenDialog.Title:='Select eml files to upload';
    ULOpenDialog.InitialDir:=settings.MiscSettings.EMLDir;
    ULOpenDialog.Filter := '*.eml files|*.EML|All files (*.*)|*.*';
    ULOpenDialog.Options:= ULOpenDialog.Options + [ofAllowMultiSelect];
    if ULOpenDialog.Execute then begin
        settings.MiscSettings.EMLDir:=ExtractFilePath(ULOpenDialog.Files[0]);
        //@todo? Proveri broj i ukupnu velicinu fajla (u opcije)
        requestPtr := CreateRequestAndSetMainParams(thtIMAPWorker,rtUploadEmls);
        requestPtr^.imapOperation:=iopUploadEMLs;
        requestPtr^.data.mailboxIndex:=mSelectedNodeIndex;
        requestPtr^.SetFilenames(ULOpenDialog.files);
        requestMgr.InvokeRequest(requestPtr);
    end;
end;

{ This can be called from several places.
  @param fullMailboxName This is the remote mailbox name of the mailbox
   we are getting messages from. The caller can set this to either the
   selected mailbox in the tree or to the displayed mailbox.
  @param msgUIDs Caller should clear this if getting messages from a
   mailbox selected in the tree, otherwise should set the list of msgUIDs }
procedure TFMain.PerformDownloadMbox(fullMailboxName: String; msgUIDs: TStringList);
var requestPtr: PRequest; messageIdsToSkip: TStringList;
begin
    // Choose file to save to
    DLSaveDialog.Title:='Specify mbox file to save to';
    DLSaveDialog.InitialDir:=settings.MiscSettings.MboxDir;
    DLSaveDialog.Filter := 'Unix mailbox files (*.mbox)|*.MBOX|All files (*.*)|*.*';
    DLSaveDialog.DefaultExt:='mbox';
    if DLSaveDialog.Execute then begin
        settings.MiscSettings.MboxDir:=ExtractFilePath(DLSaveDialog.Files[0]);
        messageIdsToSkip:=TStringList.Create;
        requestPtr := CreateRequestAndSetMainParams(thtIMAPWorker,rtDownloadMbox);
        requestPtr^.imapOperation:=iopDownloadMbox;
        requestPtr^.data.fullMailboxName:=fullMailboxName;
        requestPtr^.SetMsgUIDs(msgUIDs);
        requestPtr^.data.filename:=DLSaveDialog.filename;
        requestMgr.InvokeRequest(requestPtr);
        messageIdsToSkip.Free;
    end;
end;

procedure TFMain.PerformDownloadEMLs(fullMailboxName: String; msgUIDs: TStringList);
var requestPtr: PRequest; messageIdsToSkip: TStringList;
begin
    DirDialog1.Folder:=settings.MiscSettings.EMLDir;
    if DirDialog1.Execute then begin
        messageIdsToSkip:=TStringList.Create;
        settings.MiscSettings.EMLDir:=DirDialog1.Folder;
        requestPtr := CreateRequestAndSetMainParams(thtIMAPWorker,rtDownloadEMLs);
        requestPtr^.imapOperation:=iopDownloadEMLs;
        requestPtr^.data.fullMailboxName:=fullMailboxName;
        requestPtr^.SetMsgUIDs(msgUIDs);
        requestPtr^.data.filename:=DirDialog1.Folder;  // note: this is actually the folder name
        ProgressBar1.Position:=0;
        ProgressBar1.Visible:=true;
        requestMgr.InvokeRequest(requestPtr);
        messageIdsToSkip.Free;
    end;
end;

procedure TFMain.Eml2Mbox;
var converterOptions: TConvertOptions;
    requestPtr: PRequest;
begin
    FEml2MboxDlg := TFEml2MboxDlg.Create(Application);
    if FEml2MboxDlg.ShowModal=mrOK then begin
        requestPtr := CreateRequestAndSetMainParams(thtConverter,rtEml2Mbox);
        requestPtr^.convertOperation:=cnvOpEml2Mbox;
        requestPtr^.SetFilenames(FEml2MboxDlg.GetEmlFiles);
        requestPtr^.data.filename:=FEml2MboxDlg.GetMboxFilename;
        requestPtr^.data.convertOptions:=FEml2MboxDlg.GetConverterOptions;
        requestMgr.InvokeRequest(requestPtr);
    end;
    FEml2MboxDlg.Free;
end;

procedure TFMain.Eml2Mboxes;
var converterOptions: TConvertOptions;
    requestPtr: PRequest;
begin
    FEml2MboxesDlg := TFEml2MboxesDlg.Create(Application);
    if FEml2MboxesDlg.ShowModal=mrOK then begin
        requestPtr := CreateRequestAndSetMainParams(thtConverter,rtEml2Mboxes);
        requestPtr^.convertOperation:=cnvOpEml2Mboxes;
        requestPtr^.data.folder:=FEml2MboxesDlg.GetEmlDir;
        requestPtr^.data.filename:=FEml2MboxesDlg.GetMboxDir;
        requestPtr^.data.convertOptions:=FEml2MboxesDlg.GetConverterOptions;
        requestMgr.InvokeRequest(requestPtr);
    end;
    FEml2MboxesDlg.Free;
end;

procedure TFMain.Mbox2Eml;
var requestPtr: PRequest;
begin
    FMbox2EmlDlg := TFMbox2EmlDlg.Create(Application);
    if FMbox2EmlDlg.ShowModal=mrOK then begin
        requestPtr := CreateRequestAndSetMainParams(thtConverter,rtMbox2Eml);
        requestPtr^.convertOperation:=cnvOpMbox2Eml;
        requestPtr^.SetFilenames(FMbox2EmlDlg.GetMboxFilenames);
        requestPtr^.SetFolders(FMbox2EmlDlg.GetEmlDirs);
        requestPtr^.data.convertOptions:=GetDefaultConversionOptions;
        requestMgr.InvokeRequest(requestPtr);
    end;
    FMbox2EmlDlg.Free;
end;

{ Invokes quick and smart attachment deletion on selected messages }
procedure TFMain.PerformDeleteAttachments(mailboxIndex: Integer; msgUIDs: TStringList);
var requestPtr: PRequest;
begin
    requestPtr := CreateRequestAndSetMainParams(thtIMAPWorker,rtMultipleQSDeletion);
    requestPtr^.imapOperation:=iopDeleteAttachments;
    requestPtr^.data.mailboxIndex:=mSelectedNodeIndex;
    requestPtr^.SetMsgUIDs(msgUIDs);
    requestMgr.InvokeRequest(requestPtr);
end;

{ Loads the folder hierarchy into the tree view - no size check }
procedure TFMain.PerformLoadFolderHierarchy;
var requestPtr: PRequest;
begin
    if SetActiveAccount then begin
        requestPtr := CreateRequestAndSetMainParams(thtChecker,rtLogin);
        requestPtr^.checkerOperation:=chOpHierarchyOnly;
        requestMgr.InvokeRequest(requestPtr);
    end;
end;

{ Fully expands the selected node (if any) }
procedure TFMain.FullExpandNode;
var AnItem: PVirtualNode;
begin
    AnItem := TreeList1.GetFirstSelected;
    mSelectedNode := AnItem;
    if mSelectedNode<>nil then
        TreeList1.FullExpand(mSelectedNode);
end;

{ Fully collapses the selected node (if any) }
procedure TFMain.FullCollapseNode;
var AnItem: PVirtualNode;
begin
    AnItem := TreeList1.GetFirstSelected;
    mSelectedNode := AnItem;
    if mSelectedNode<>nil then
        TreeList1.FullCollapse(mSelectedNode);
end;

{ Returns mSeparator, except if the separator is '\' }
function TFMain.GetSeparator: String;
begin
    if mSeparator<>'\' then Result:=mSeparator
    else Result:='\\';
end;

{ (D)escapes a string containing \ characters
  @todo might need to extend for escaping other characters
  @param s The string to (d)escape
  @param escape True if the string should be escaped }
function TFMain.EscapeString(s: String; escape: Boolean): String;
begin
    if escape then Result:=StringReplace(s,'\','\\',[rfReplaceAll])
    else Result:=StringReplace(s,'\\','\',[rfReplaceAll])
end;

{ --------------------------------------------}
{ ----------- ListView Actions ---------------}
{ --------------------------------------------}

{ Action taken when a key is downed on the list view }
procedure TFMain.ListView1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
    if (ssCtrl in Shift) then begin
        // Ctrl+
        if IsKeyDown(Key,'E') then ExpungeThisMailbox
        else if IsKeyDown(Key,'X') then DeleteSelectedMsgsAndExpunge
        else if IsKeyDown(Key,'C') then CopyMessages
        else if IsKeyDown(Key,'K') then CopyDeleteMessages
        else if IsKeyDown(Key,'M') then CopyDeleteExpungeMessages
        else if IsKeyDown(Key,'R') then ReloadFromServer
        else if IsKeyDown(Key,'H') then SaveMessageHeaders
        else if (Key=VK_F2) then DeleteFlagReset
        else if (Key=VK_F3) then SeenFlagReset
        else if (Key=VK_F4) then FlaggedFlagReset
        else if (Key=VK_F5) then AnsweredFlagReset
        else if (Key=VK_DELETE) then QSDeleteAttachments;
    end
    else begin
        // No Alt or Ctrl pressed
        if (Key=VK_RETURN) then PeekMessage
        else if (Key=VK_F2) then DeleteFlagSet
        else if (Key=VK_F3) then SeenFlagSet
        else if (Key=VK_F4) then FlaggedFlagSet
        else if (Key=VK_F5) then AnsweredFlagSet;
    end;
end;

{ Invokes message peeking (both header and body) }
procedure TFMain.PeekMessage;
begin
    if Assigned(ListView1.Selected) then
        PerformMessagePrepare(mDisplayedNodeIndex);
end;

{ Invokes setting of the deletion flag }
procedure TFMain.DeleteFlagSet;
begin
    if Assigned(ListView1.Selected) then
        PerformMessageIMAPOperation(iopDeleteMsg,mDisplayedNodeIndex,GetSelectedMsgs);
end;

{ Invokes resetting of the deletion flag }
procedure TFMain.DeleteFlagReset;
begin
    if Assigned(ListView1.Selected) then
        PerformMessageIMAPOperation(iopUndeleteMsg,mDisplayedNodeIndex,GetSelectedMsgs);
end;

{ Invokes setting of the seen flag }
procedure TFMain.SeenFlagSet;
begin
    if Assigned(ListView1.Selected) then
        PerformMessageIMAPOperation(iopSeenMsg,mDisplayedNodeIndex,GetSelectedMsgs);
end;

{ Invokes resetting of the seen flag }
procedure TFMain.SeenFlagReset;
begin
    if Assigned(ListView1.Selected) then
        PerformMessageIMAPOperation(iopUnseenMsg,mDisplayedNodeIndex,GetSelectedMsgs);
end;

{ Invokes setting of the flagged flag }
procedure TFMain.FlaggedFlagSet;
begin
    if Assigned(ListView1.Selected) then
        PerformMessageIMAPOperation(iopFlagMsg,mDisplayedNodeIndex,GetSelectedMsgs);
end;

{ Invokes resetting of the flagged flag }
procedure TFMain.FlaggedFlagReset;
begin
    if Assigned(ListView1.Selected) then
        PerformMessageIMAPOperation(iopUnflagMsg,mDisplayedNodeIndex,GetSelectedMsgs);
end;

procedure TFMain.AnsweredFlagSet;
begin
    if Assigned(ListView1.Selected) then
        PerformMessageIMAPOperation(iopAnsweredMsg,mDisplayedNodeIndex,GetSelectedMsgs);
end;

procedure TFMain.AnsweredFlagReset;
begin
    if Assigned(ListView1.Selected) then
        PerformMessageIMAPOperation(iopUnansweredMsg,mDisplayedNodeIndex,GetSelectedMsgs);
end;

{ Marks selected messages as deleted and performs an expunge }
procedure TFMain.DeleteSelectedMsgsAndExpunge;
begin
    if Assigned(ListView1.Selected) then
        if MessageDlg('This will mark selected messages as deleted AND expunge the mailbox. Proceed?',mtConfirmation,mbYesNoCancel,0) = mrYes then
            PerformMessageIMAPOperation(iopDeleteExpungeMsg,mDisplayedNodeIndex,GetSelectedMsgs);
end;

{ Expunges the current mailbox }
procedure TFMain.ExpungeThisMailbox;
begin
    if mDisplayedNodeIndex > -1 then
        PerformIMAPOperation(iopExpungeMbox,mDisplayedNodeIndex);
end;

{ Copies selected messages to the destination mailbox }
procedure TFMain.CopyMessages;
begin
    if Assigned(ListView1.Selected) then
        PerformCopy(iopCopy,tcoCopyOnly);
end;

{ Copies selected messages to the destination mailbox and marks original messages deleted }
procedure TFMain.CopyDeleteMessages;
begin
    if Assigned(ListView1.Selected) then
        PerformCopy(iopCopyDelete,tcoCopyMarkDeleted);
end;

{ Copies selected messages to the destination mailbox, marks messages deleted and performs an Expunge}
procedure TFMain.CopyDeleteExpungeMessages;
begin
    if Assigned(ListView1.Selected) then
        PerformCopy(iopCopyDelExpunge,tcoCopyDeleteExpunge);
end;

{ Reloads the displayed mailbox from the server }
procedure TFMain.ReloadFromServer;
begin
    if mDisplayedNodeIndex > -1 then
        PerformIMAPOperation(iopGetMboxEnvelopes,mDisplayedNodeIndex);
end;

procedure TFMain.SaveMessageHeaders;
begin
    if Assigned(ListView1.Selected) then
        PerformSaveHeaders(mDisplayedNodeIndex, GetSelectedMsgs);
end;

{ --------------------------------------------}
{ ----------- Main menu Actions --------------}
{ --------------------------------------------}

{ Exports the size report to a csv file }
procedure TFMain.ExportToCSV;
var csvStrings: TStringList;
begin
    SaveDialog1.Filter := 'CSV files (*.csv)|*.CSV';
    if SaveDialog1.Execute then begin
        csvStrings := TStringList.Create;
        FormCSVStrings(csvStrings);
        csvStrings.SaveToFile(SaveDialog1.FileName);
        MessageDlg('Account size information exported...',mtInformation,[mbOk],0);
    end;
end;


procedure TFMain.AExportToCSVExecute(Sender: TObject);
begin
    ExportToCSV;
end;

procedure TFMain.AExitExecute(Sender: TObject);
begin
    Close;
end;

procedure TFMain.ACheckSizeExecute(Sender: TObject);
begin
    CheckSize;
end;

procedure TFMain.ACheckQuotaExecute(Sender: TObject);
begin
    CheckQuota;
end;

procedure TFMain.AAccountBackupExecute(Sender: TObject);
begin
    AccountBackup;
end;

procedure TFMain.ALoginExecute(Sender: TObject);
begin
    Login;
end;

procedure TFMain.ALogoutExecute(Sender: TObject);
begin
    Logout;
end;

procedure TFMain.AListSortedExecute(Sender: TObject);
begin
    InvokeSortBySize;
end;

procedure TFMain.AGlobalSearchExecute(Sender: TObject);
begin
    InvokeGlobalSearchDlg('');
end;

procedure TFMain.AAdvancedSearchExecute(Sender: TObject);
begin
    InvokeAdvSearchDlg;
end;

procedure TFMain.AHelpExecute(Sender: TObject);
begin
    Application.HelpCommand(HELP_CONTENTS,0);
end;

procedure TFMain.ACancelActionExecute(Sender: TObject);
begin
    CancelAction;
end;

procedure TFMain.AShowDestinationDialogExecute(Sender: TObject);
begin
    ShowDestinationMailboxDlg;
end;

procedure TFMain.AFocusAccountExecute(Sender: TObject);
begin
    ComboBox1.SetFocus;
end;

procedure TFMain.AFocusSearchExecute(Sender: TObject);
begin
    EditSearchString.SetFocus;
end;

procedure TFMain.AFocusTreeExecute(Sender: TObject);
begin
    TreeList1.SetFocus;
end;

procedure TFMain.AFocusListExecute(Sender: TObject);
begin
    ListView1.SetFocus;
end;

procedure TFMain.AOptionsExecute(Sender: TObject);
begin
    ViewOptions;
end;

procedure TFMain.eml2mbox1Click(Sender: TObject);
begin
    Eml2Mbox;
end;

procedure TFMain.eml2mboxes1Click(Sender: TObject);
begin
    Eml2Mboxes;
end;

procedure TFMain.mbox2eml1Click(Sender: TObject);
begin
    Mbox2Eml;
end;

procedure TFMain.ClearAccountCache1Click(Sender: TObject);
begin
    if ComboBox1.ItemIndex>-1 then begin
        if MessageDlg('Are you sure you want to delete the cache for account ['+ComboBox1.Items[ComboBox1.ItemIndex]+']?',mtConfirmation,[mbYes,mbNo],0) = mrYes then begin
            if not mCache.ClearAccountCache(ComboBox1.Items[ComboBox1.ItemIndex]) then
                MessageDlg('The account cache could not be deleted!',mtError,[mbOk],0);
        end;
    end
    else MessageDlg('No accounts selected',mtInformation,[mbOK],0);

end;

{ Invokes the dialog for creating a new account }
procedure TFMain.New1Click(Sender: TObject);
var account: TAccountInfo;
begin
    FAddAccountDlg := TFAddAccountDlg.Create(Application);
    if FAddAccountDlg.ShowModal = mrOK then begin
        account:=TAccountInfo.Create;
        FAddAccountDlg.getNewAccount(account);
        settings.Accounts.addAccount(account);
        account.Free;
        FillAccountCombo;
    end;
    FAddAccountDlg.Free;
end;

procedure TFMain.ChangeDestinationMailboxMboxMenuClick(Sender: TObject);
begin
    ShowDestinationMailboxDlg;
end;

procedure TFMain.IMAPConsole1Click(Sender: TObject);
begin
    //if not Assigned(FIMAPConsole) then FIMAPConsole := TFIMAPConsole.Create(Self);
    //FIMAPConsole.Show;
end;

procedure TFMain.Activity1Click(Sender: TObject);
begin
    FActivityDlg.Show;
end;

procedure TFMain.FAQ1Click(Sender: TObject);
begin
    ShellExecute(Handle, 'open', PChar('http://www.broobles.com/imapsize/faq.php'), nil, nil, SW_SHOW);
end;

procedure TFMain.News1Click(Sender: TObject);
begin
    ShellExecute(Handle, 'open', PChar('http://www.broobles.com/imapsize/news.php'), nil, nil, SW_SHOW);
end;

procedure TFMain.TreeList1DblClick(Sender: TObject);
var AnItem: PVirtualNode; p: TPoint; CurrNodeRec: PNodeRecord;
begin
    AnItem := TreeList1.GetNodeAt(TreeLastClick.X,TreeLastClick.Y);
    if AnItem<>nil then begin
        TreeList1.Selected[AnItem]:=true;
        TreeList1.FocusedNode:=AnItem;
        CurrNodeRec := PNodeRecord( TreeList1.GetNodeData(AnItem));
        mSelectedNodeIndex:=CurrNodeRec.mAbsoluteIndex;
        if (not mboxTree.Items[mSelectedNodeIndex].mVirtual) then begin
            mSelectedNode:=AnItem;
            RetrieveMailboxMessages;
        end;
    end;
end;

{ Method that invokes quick & smart attachment deletion on multiple messages }
procedure TFMain.QSDeleteAttachments;
begin
    if Assigned(ListView1.Selected) then
        if MessageDlg('This will delete attachments from the selected messages. Do you want to continue?',mtConfirmation,mbYesNoCancel,0)=mrYes then
            PerformDeleteAttachments(mDisplayedNodeIndex, GetSelectedMsgs);
end;

procedure TFMain.KeyboardShortcuts1Click(Sender: TObject);
begin
    FTextViewer:=TFTextViewer.Create(Application);
    FTextViewer.Memo1.Lines.LoadFromFile(ExtractFileDir(Application.ExeName)+'\keyboard.txt');
    FTextViewer.PageControl1.ActivePageIndex:=TEXT_PAGE_IDX;
    FTextViewer.Show;
end;

procedure TFMain.FolderSubscriptions1Click(Sender: TObject);
begin
    InvokeFolderSubsDlg;
end;

procedure TFMain.ServerParameters1Click(Sender: TObject);
var requestPtr: PRequest;
begin
    if SetActiveAccount then begin
        requestPtr := CreateRequestAndSetMainParams(thtIMAPWorker,rtDebugServer);
        requestPtr^.imapOperation:=iopDebugServer;
        requestMgr.InvokeRequest(requestPtr);
    end
    else begin
        MessageDlg('Please select the account whose IMAP server you want to check!',mtWarning,[mbOK],0);
    end;
end;

procedure TFMain.ReplicateFolderHierarchy1Click(Sender: TObject);
begin
    if SetActiveAccount then begin
        FFolderHierarchyReplicatorDlg := TFFolderHierarchyReplicatorDlg.Create(FMain);
        FFolderHierarchyReplicatorDlg.SetFolder(dataDir+'\backup\'+pActiveAccount^.Name);
        FFolderHierarchyReplicatorDlg.ShowModal;
        FFolderHierarchyReplicatorDlg.Free;
    end
    else
        MessageDlg('You have to select an account you want to replicate!',mtInformation,[mbOk],0);
end;

procedure TFMain.AccountBackup;
var folders, messageIdsToSkip: TStringList; i: Integer; requestPtr:
    PRequest; backupDir: String;
    folderSeparator: Char; dummy: TStringList;
    mboxFilename, emlFolder: String;
begin
    if not backupRunning then begin
        if SetActiveAccount then begin
            if settings.MiscSettings.BackupDir='' then begin
                settings.MiscSettings.BackupDir:=IncludeTrailingBackslash(dataDir)+'backup';
            end;
            backupDir:=IncludeTrailingBackslash(settings.MiscSettings.BackupDir)+pActiveAccount^.Name;
            FAccountBackupDlg:=TFAccountBackupDlg.Create(FMain);
            FAccountBackupDlg.SetBackupFolder(backupDir);
            FAccountBackupDlg.PrepareMailboxList;
            if FAccountBackupDlg.ShowModal = mrOK then begin
                folders:=TStringList.Create;
                try
                    FAccountBackupDlg.GetFoldersToBackup(folders);
                    folderSeparator:=pActiveAccount.FolderSeparator;
                    FAccountBackupDlg.Free;
                    // Create folders
                    try
                        InitializeBackupDB;
                        CreateLocalFolders(folders, backupDir, folderSeparator);
                        requestPtr := CreateRequestAndSetMainParams(thtIMAPWorker,rtAccountBackup);
                        requestPtr^.imapOperation:=iopAccountBackup;
                        requestPtr^.setFolders(folders);
                        requestPtr^.data.filename:=backupDir;
                        requestPtr^.data.folderSeparator:=folderSeparator;
                        requestMgr.InvokeRequest(requestPtr);
                    except
                        on E:Exception do
                            MessageDlg('Backup failed: '+E.Message+'. Please check your permissions.',mtError,[mbOk],0);
                    end;
                finally
                    folders.Free;
                end;
            end;
        end
        else
            MessageDlg('You have to select an account you want to backup!',mtInformation,[mbOk],0);
    end
    else begin
        MessageDlg('Another backup is currently active. Please wait until it completes.',mtWarning,[mbOk],0);
    end;
end;


procedure TFMain.InvokeCommandLineBackup;
var folders: TStringList; folderSeparator: Char; requestPtr: PRequest; backupDir: String;
begin
    folders:=TStringList.Create;
    try
        folders.Text:=pActiveAccount.BackupFolders;
        folderSeparator:=pActiveAccount.FolderSeparator;
        if settings.MiscSettings.BackupDir='' then begin
            settings.MiscSettings.BackupDir:=IncludeTrailingBackslash(dataDir)+'backup\';
        end;
        backupDir:=IncludeTrailingBackslash(settings.MiscSettings.BackupDir)+pActiveAccount^.Name;

        // Create folders
        try
            InitializeBackupDB;
            CreateLocalFolders(folders, backupDir, folderSeparator);  // Can raise exception
            requestPtr := CreateRequestAndSetMainParams(thtIMAPWorker,rtAccountBackupCommandLine);
            requestPtr^.imapOperation:=iopAccountBackup;
            requestPtr^.setFolders(folders);
            requestPtr^.data.filename:=backupDir;
            requestPtr^.data.folderSeparator:=folderSeparator;
            requestPtr^.commandLine:=true;
            requestMgr.InvokeRequest(requestPtr);
        except
            on E:Exception do
                backupLog.Error('Backup failed: '+E.Message+'. Check your permissions');
        end;
    finally
        folders.Free;
    end;
end;

{ Call this method to terminate the backup specifying the error message (reason for termination) }
procedure TFMain.TerminateBackup(errorMsg: String);
begin
    backupLog.Error(errorMsg);
    backupLog.Info('===== Backup has NOT been performed. Closing IMAPSize =====');
    CommandLineBackupModeCloseResources;
    Application.Terminate;
end;

{ Clean shutdown from backup mode (console) }
procedure TFMain.CommandLineBackupModeCloseResources;
var appSettingsStr: TStrings;
begin
    devLog.Trace('Freeing resources');

    // Free other resources
    if Assigned(backupDB) then backupDB.Free;
    requestMgr.Free;
    Dispose(pActiveAccount);
    devLog.Info('Resources freed. Ready to shutdown');

    // Finally free loggers
    vcLog.Free;
    if IsDebugDefined then lg.Free;
end;

procedure TFMain.RestoreBackup1Click(Sender: TObject);
var folders: TListOfBackupFolderInfos;
    srcAccount, dstAccount: String;
    requestPtr: PRequest;
    folderSeparator: Char;

    procedure NoBackupsFoundMsg;
    begin
        MessageDlg('No backups found! You need to perform a backup before restoring.',mtInformation,[mbOK],0);
    end;
begin
    if SetActiveAccount then begin
        if settings.MiscSettings.BackupExists then begin
            try
                InitializeBackupDB;
                FRestoreBackupDlg:=TFRestoreBackupDlg.Create(Application);
                if FRestoreBackupDlg.SetBackedUpAccounts(ComboBox1.Text) then begin
                    if FRestoreBackupDlg.ShowModal = mrOK then begin
                        srcAccount:=FRestoreBackupDlg.GetSourceAccount;
                        dstAccount:=FRestoreBackupDlg.GetDestinationAccount;
                        FRestoreBackupDlg.GetFoldersToBackup(folders);

                        New(requestPtr); // Will be freed by the request manager when the request completes...
                        requestPtr^ := TRequest.Create(thtIMAPWorker);
                        requestPtr^.requestType:=rtRestoreBackup;
                        requestPtr^.requestComponent:=rcMain;
                        pDstAccount^:=settings.Accounts.getAccount(dstAccount);
                        requestPtr^.accountInfoPtr:=pDstAccount;
                        requestPtr^.imapOperation:=iopRestoreBackup;
                        requestPtr^.SetBackupFolders(folders);
                        requestPtr^.data.restoreSameAccount := (srcAccount = dstAccount);
                        requestPtr^.data.folderSeparator:=settings.Accounts.getAccount(srcAccount).FolderSeparator;
                        requestPtr^.data.accountName:=srcAccount;
                        requestMgr.InvokeRequest(requestPtr);

                        SetLength(folders,0);
                    end;
                end
                else NoBackupsFoundMsg;
            finally
                FRestoreBackupDlg.Free;
            end;
        end
        else NoBackupsFoundMsg;
    end
    else NoBackupsFoundMsg;
end;

procedure TFMain.InitializeBackupDB;
var errorMsg: String;
begin
    if not Assigned(backupDB) then begin
        try
            devLog.Info('Initializing backup database in ['+settings.MiscSettings.BackupDir+']');
            backupDB:=TBackupDB.Create(settings.MiscSettings.BackupDir);
            settings.MiscSettings.BackupExists:=true;
        except
            on E:Exception do begin
                errorMsg:= 'Error initializing backups: ' + E.Message + ', check permissions for the backup folder [' + settings.MiscSettings.BackupDir + ']' ;
                if consoleMode then backupLog.Error(errorMsg)
                else MessageDlg(errorMsg,mtError,[mbOk],0);
            end;
        end;
    end;
end;

procedure TFMain.mboxInspector1Click(Sender: TObject);
begin
    FMboxInspectorDlg := TFMboxInspectorDlg.Create(Application);
    FMboxInspectorDlg.ShowModal;
    FMboxInspectorDlg.Free;
end;

procedure TFMain.UpgradeBackups1Click(Sender: TObject);
begin
    FUpgradeBackupsDlg := TFUpgradeBackupsDlg.Create(Application);
    FUpgradeBackupsDlg.ShowModal;
    FUpgradeBackupsDlg.Free;
end;

end.
