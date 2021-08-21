{==============================================================================|
| Project : IMAPSize                                                           |
|==============================================================================|
| $Id: AppSettings.pas,v 1.13 2004/03/31 23:27:39 Ivan Exp $
|==============================================================================|
| Copyright 2003-2004 Ivan Vecanski                                            |
| =============================================================================}

unit AppSettings;

interface

uses classes, QuickRTTI, Accounts, SpamHandles, graphics, MessageLists, SavedSearches;

type

    TPositions = class(TPersistent)
    private
        fTop: Integer;
        fLeft: Integer;
        fWidth: Integer;
        fHeight: Integer;
        fMaximized: Boolean;
        fLDTop: Integer;
        fLDLeft: Integer;
        fLDWidth: Integer;
        fLDHeight: Integer;
        fLDMaximized: Boolean;
        fSplitHorz: Boolean;
        fSplitPos: Integer;
        fGSTop: Integer;
        fGSLeft: Integer;
        constructor Create;
    published
        property Top: Integer read fTop write fTop;
        property Left: Integer read fLeft write fLeft;
        property Width: Integer read fWidth write fWidth;
        property Height: Integer read fHeight write fHeight;
        property Maximized: Boolean read fMaximized write fMaximized;
        property LDTop: Integer read fLDTop write fLDTop;
        property LDLeft: Integer read fLDLeft write fLDLeft;
        property LDWidth: Integer read fLDWidth write fLDWidth;
        property LDHeight: Integer read fLDHeight write fLDHeight;
        property LDMaximized: Boolean read fLDMaximized write fLDMaximized;
        property SplitHorz: Boolean read fSplitHorz write fSplitHorz;
        property SplitPos: Integer read fSplitPos write fSplitPos;
        property GSTop: Integer read fGSTop write fGSTop;
        property GSLeft: Integer read fGSLeft write fGSLeft;
    end;

    TThresholds = class(TPersistent)
    private
        fLower: Integer;
        fUpper: Integer;
        fAllowLowNumChange: Boolean;
        fAllowLowNumThr: Integer;
        constructor Create;
    published
        property Lower: Integer read fLower write fLower;
        property Upper: Integer read fUpper write fUpper;
        property AllowLowNumChange: Boolean read fAllowLowNumChange write fAllowLowNumChange;
        property AllowLowNumThr: Integer read fAllowLowNumThr write fAllowLowNumThr;
    end;

    TMessageDisplay = class(TPersistent)
    private
        fLower: Integer;
        fUpper: Integer;
        fSizeFormat: Integer;
        fFlaggedColor: Integer;
        fSpamColor: Integer;
        fAddressStructure: Integer; // 0: name+address (if applicable), 1: name only (address if not available) 2: address only
        fAnsweredItalics: Boolean;
        constructor Create;
    published
        property Lower: Integer read fLower write fLower;
        property Upper: Integer read fUpper write fUpper;
        property SizeFormat: Integer read fSizeFormat write fSizeFormat;
        property FlaggedColor: Integer read fFlaggedColor write fFlaggedColor;
        property SpamColor: Integer read fSpamColor write fSpamColor;
        property AddressStructure: Integer read fAddressStructure write fAddressStructure;
        property AnsweredItalics: Boolean read fAnsweredItalics write fAnsweredItalics;
    end;

    TIMAPOpSettings = class(TPersistent)
    private
        fLimitTextToFetch: Boolean;
        fTextToFetchLength: Integer; // Number of chars of the message body text to fetch
        fEnableHeaderEditing: Boolean;
        fQoutaAlertThreshold: Integer;
        fServerTimeout: Integer;
        fMsgDelAddFilenamesToBody: Boolean;
        constructor Create;
    published
        property LimitTextToFetch: Boolean read fLimitTextToFetch write fLimitTextToFetch;
        property TextToFetchLength: Integer read fTextToFetchLength write fTextToFetchLength;
        property EnableHeaderEditing: Boolean read fEnableHeaderEditing write fEnableHeaderEditing;
        property QoutaAlertThreshold: Integer read fQoutaAlertThreshold write fQoutaAlertThreshold;
        property ServerTimeout: Integer read fServerTimeout write fServerTimeout;
        property MsgDelAddFilenamesToBody: Boolean read fMsgDelAddFilenamesToBody write fMsgDelAddFilenamesToBody;
    end;

    TTreeDisplay = class(TPersistent)
    private
        fColumn0Width: Integer;
        fColumn1Width: Integer;
        fColumn2Width: Integer;
        fColumn3Width: Integer;
        fColumn4Width: Integer;
        fColumn5Width: Integer;
        fSizeFormat: Integer;
        fUpdateAsSizeCheck: Boolean;  // true if updates are performed on every mbox when checking size
        constructor Create;
    published
        property Column0Width: Integer read fColumn0Width write fColumn0Width;
        property Column1Width: Integer read fColumn1Width write fColumn1Width;
        property Column2Width: Integer read fColumn2Width write fColumn2Width;
        property Column3Width: Integer read fColumn3Width write fColumn3Width;
        property Column4Width: Integer read fColumn4Width write fColumn4Width;
        property Column5Width: Integer read fColumn5Width write fColumn5Width;
        property SizeFormat: Integer read fSizeFormat write fSizeFormat;
        property UpdateAsSizeCheck: Boolean read fUpdateAsSizeCheck write fUpdateAsSizeCheck;
    end;

    TListDisplay = class(TPersistent)
    private
        fColumn0Width: Integer;
        fColumn1Width: Integer;
        fColumn2Width: Integer;
        fColumn3Width: Integer;
        fColumn4Width: Integer;
        fColumn5Width: Integer;
        fColumn6Width: Integer;
        fMsgSortField: Integer;
        fMsgSortDir: Integer;
        constructor Create;
    published
        property Column0Width: Integer read fColumn0Width write fColumn0Width;
        property Column1Width: Integer read fColumn1Width write fColumn1Width;
        property Column2Width: Integer read fColumn2Width write fColumn2Width;
        property Column3Width: Integer read fColumn3Width write fColumn3Width;
        property Column4Width: Integer read fColumn4Width write fColumn4Width;
        property Column5Width: Integer read fColumn5Width write fColumn5Width;
        property Column6Width: Integer read fColumn6Width write fColumn6Width;
        property MsgSortField: Integer read fMsgSortField write fMsgSortField;
        property MsgSortDir: Integer read fMsgSortDir write fMsgSortDir;
    end;

    TMessagePeeker = class(TPersistent)
    private
        fTop: Integer;
        fLeft: Integer;
        fWidth: Integer;
        fHeight: Integer;
        fMaximized: Boolean;
        //fSplitPos: Integer;
        fFontName: String;
        fFontSize: Integer;
        //fPartsSplitPos: Integer;
        fPartsAdvanced: Boolean;
        fPartsViewNameWidth: Integer;
        fPartsViewSizeWidth: Integer;
        fAutoCloseMsgPeeker: Boolean;
        //fShowHeader: Boolean;
        constructor Create;
    published
        property Top: Integer read fTop write fTop;
        property Left: Integer read fLeft write fLeft;
        property Width: Integer read fWidth write fWidth;
        property Height: Integer read fHeight write fHeight;
        property Maximized: Boolean read fMaximized write fMaximized;
        //property SplitPos: Integer read fSplitPos write fSplitPos;
        property FontName: String read fFontName write fFontName;
        property FontSize: Integer read fFontSize write fFontSize;
        //property PartsSplitPos: Integer read fPartsSplitPos write fPartsSplitPos;
        property PartsAdvanced: Boolean read fPartsAdvanced write fPartsAdvanced;
        property PartsViewNameWidth: Integer read fPartsViewNameWidth write fPartsViewNameWidth;
        property PartsViewSizeWidth: Integer read fPartsViewSizeWidth write fPartsViewSizeWidth;
        property AutoCloseMsgPeeker: Boolean read fAutoCloseMsgPeeker write fAutoCloseMsgPeeker;
        //property ShowHeader: Boolean read fShowHeader write fShowHeader;
    end;

    TMiscSettings = class(TPersistent)
    private
        fDebugLog: Boolean;
        fEnableCaching: Boolean;
        fMboxDir: String;
        fEMLDir: String;
        fSaveAttachmentDir: String;
        fXPMenus: Boolean;
        // fBackupMethod: Integer;  // 0 - eml, 1 - mbox
        fBackupDir: String;
        fBackupExists: Boolean;  // internal setting, is set to 1 when the user creates the first backup
        fBackupFilenameFormat: String;  // The format of filenames
        fLoadFoldersStartup: Boolean;
        constructor Create;
    published
        property DebugLog: Boolean read fDebugLog write fDebugLog;
        property EnableCaching: Boolean read fEnableCaching write fEnableCaching;
        property MboxDir: String read fMboxDir write fMboxDir;
        property EMLDir: String read fEMLDir write fEMLDir;
        property SaveAttachmentDir: String read fSaveAttachmentDir write fSaveAttachmentDir;
        property XPMenus: Boolean read fXPMenus write fXPMenus;
        property BackupDir: String read fBackupDir write fBackupDir;
        property BackupFilenameFormat: String read fBackupFilenameFormat write fBackupFilenameFormat;
        property BackupExists: Boolean read fBackupExists write fBackupExists;
        property LoadFoldersStartup: Boolean read fLoadFoldersStartup write fLoadFoldersStartup;
    end;

    TLogSettings = class(TPersistent)
    private
        fIMAPLog: Integer;
        fAppLog: Integer;
        fMaxFileSize: Integer;
        fFilesToKeep: Integer;
        constructor Create;
    published
        property IMAPLog: Integer read fIMAPLog write fIMAPLog;
        property AppLog: Integer read fAppLog write fAppLog;
        property MaxFileSize: Integer read fMaxFileSize write fMaxFileSize;
        property FilesToKeep: Integer read fFilesToKeep write fFilesToKeep;
    end;

    TAppSettings = class(TXMLAware)
    private
        fPositions: TPositions;
        fThresholds: TThresholds;
        fMessageDisplay: TMessageDisplay;
        fIMAPOpSettings: TIMAPOpSettings;
        fTreeDisplay: TTreeDisplay;
        fListDisplay: TListDisplay;
        fMessagePeeker: TMessagePeeker;
        fMiscSettings: TMiscSettings;
        fLogSettings: TLogSettings;
        fAccounts: TAccounts;
        fSpamHandles: TSpamHandles;
        fSavedSearches: TSavedSearches;
    public
        constructor Create (RTTIEnabler:TCustomXMLRTTI);override;
        destructor  Destroy;override;
    published
        property Positions: TPositions read fPositions write fPositions;
        property Thresholds: TThresholds read fThresholds write fThresholds;
        property MessageDisplay: TMessageDisplay read fMessageDisplay write fMessageDisplay;
        property IMAPOpSettings: TIMAPOpSettings read fIMAPOpSettings write fIMAPOpSettings;
        property TreeDisplay: TTreeDisplay read fTreeDisplay write fTreeDisplay;
        property ListDisplay: TListDisplay read fListDisplay write fListDisplay;
        property MessagePeeker: TMessagePeeker read fMessagePeeker write fMessagePeeker;
        property MiscSettings: TMiscSettings read fMiscSettings write fMiscSettings;
        property LogSettings: TLogSettings read fLogSettings write fLogSettings;
        property Accounts: TAccounts read fAccounts write fAccounts;
        property SpamHandles: TSpamHandles read fSpamHandles write fSpamHandles;
        property SavedSearches: TSavedSearches read fSavedSearches write fSavedSearches;
    end;


implementation


constructor TPositions.Create;
begin
    fTop:=50;
    fLeft:=50;
    fWidth:=800;
    fHeight:=600;
    fMaximized:=false;
    fLDTop:=200;
    fLDLeft:=200;
    fLDWidth:=350;
    fLDHeight:=400;
    fLDMaximized:=false;
    fSplitHorz:=true;
    fSplitPos:=200;
    fGSTop:=250;
    fGSLeft:=150;
end;

constructor TThresholds.Create;
begin
    fLower:=10;
    fUpper:=25;
    fAllowLowNumChange:=true;
    fAllowLowNumThr:=4;
end;

// Default values for MessageDisplay
constructor TMessageDisplay.Create;
begin
    fLower:= 100;           // 100 KB
    fUpper:= 1000;          // 1000 KB
    fSizeFormat:=0;         // Byte display
    fFlaggedColor:=clGreen;
    fSpamColor:=clRed;
    fAddressStructure:=0;   // Name + address
    fAnsweredItalics:=false;
end;

constructor TIMAPOpSettings.Create;
begin
    fLimitTextToFetch:=false;
    fTextToFetchLength:=10000;
    fEnableHeaderEditing:=false;
    fQoutaAlertThreshold:=85;
    fServerTimeout:=5;
    fMsgDelAddFilenamesToBody:=true;
end;

constructor TTreeDisplay.Create;
begin
    fColumn0Width:=156;
    fColumn1Width:=30;
    fColumn2Width:=45;
    fColumn3Width:=57;
    fColumn4Width:=73;
    fColumn5Width:=57;
    fSizeFormat:=3;
    fUpdateAsSizeCheck:=true;
end;

constructor TListDisplay.Create;
begin
    fColumn0Width:=106;
    fColumn1Width:=84;
    fColumn2Width:=190;
    fColumn3Width:=148;
    fColumn4Width:=72;
    fColumn5Width:=75;
    fColumn6Width:=91;
    fMsgSortField:=msfToInt(msfNone);
    fMsgSortDir:=msdToInt(msdAsc);
end;

constructor TMessagePeeker.Create;
begin
    fTop:=150;
    fLeft:=150;
    fWidth:=600;
    fHeight:=400;
    fMaximized:=false;
    //fSplitPos:=200;
    fFontName:='Courier';
    fFontSize:=10;
    fPartsAdvanced:=false;
    //fPartsSplitPos:=400;
    fPartsViewNameWidth:=150;
    fPartsViewSizeWidth:=50;
    fAutoCloseMsgPeeker:=false;
    //fShowHeader:=true;
end;

constructor TMiscSettings.Create;
begin
    fDebugLog:=false;
    fEnableCaching:=false;
    fMboxDir:='C:\';
    fEMLDir:='C:\';
    fSaveAttachmentDir:='C:\';
    fXPMenus:=true;
    // fBackupMethod:=0;
    fBackupDir:='';
    fBackupFilenameFormat:='%SUBJECT_%CURTIME_%MSGNUM.eml';
    fBackupExists:=false;
    fLoadFoldersStartup:=false;
end;

constructor TLogSettings.Create;
begin
    fIMAPLog := 1; // Basic
    fAppLog := 0;  // Off
    fMaxFileSize := 5;
    fFilesToKeep := 3;
end;

constructor TAppSettings.Create (RTTIEnabler:TCustomXMLRTTI);
begin
    inherited Create(RTTIEnabler);
    Registerclass(TPositions);
    Registerclass(TThresholds);
    Registerclass(TMessageDisplay);
    Registerclass(TIMAPOpSettings);
    Registerclass(TTreeDisplay);
    Registerclass(TListDisplay);
    Registerclass(TMessagePeeker);
    Registerclass(TMiscSettings);
    Registerclass(TLogSettings);
    Positions:=TPositions.Create;
    Thresholds:=TThresholds.Create;
    MessageDisplay:=TMessageDisplay.Create;
    IMAPOpSettings:=TIMAPOpSettings.Create;
    TreeDisplay:=TTreeDisplay.Create;
    ListDisplay:=TListDisplay.Create;
    MessagePeeker:=TMessagePeeker.Create;
    MiscSettings:=TMiscSettings.Create;
    LogSettings:=TLogSettings.Create;
    // Following are registered upon their creation
    Accounts:=TAccounts.Create;
    SpamHandles:=TSpamHandles.Create;
    SavedSearches:=TSavedSearches.Create;
end;

destructor  TAppSettings.Destroy;
begin
    try
        Positions.Free;
        Thresholds.Free;
        MessageDisplay.Free;
        IMAPOpSettings.Free;
        TreeDisplay.Free;
        ListDisplay.Free;
        MessagePeeker.Free;
        MiscSettings.Free;
        LogSettings.Free;
        Accounts.Free;
        SpamHandles.Free;
        SavedSearches.Free;
    finally
        inherited destroy;
    end;
end;

end.