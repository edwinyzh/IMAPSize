{

    Vectrics Logging - distributed logging for Java
    Copyright (C) 2003 -  Mike Moore

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

}

unit Logging;


{$ObjExportAll On}

// Make sure we have resource files available.

interface

uses Classes, Graphics,
    Controls, Forms, SyncObjs, PageList, ExtCtrls, SysUtils;

const
    ONE_MEG = 1000000;

type TVLevelValue = (lvInherit, lvTrace, lvDebug, lvInfo,
    lvWarn, lvError, lvFatal, lvOff);


type

TVException = class (Exception)
end;

TVFileException = class (TVException)
end;

TVAppenderThreadException = class (TVException)
end;


TVLevel = class(TObject)
public
    class function ValueToString(value: TVLevelValue): String;
    class function ToString(value: TVLevelValue): String;
    class function ToSymbol(value: TVLevelValue): String;
    class function GetLevel(value: String): TVLevelValue; overload;
end;



TVLoggingEvent = class(TObject)
private
    fTime: TDateTime;
    fLevel: TVLevelValue;
    fIndex: integer;
    fLogger: String;
    fMessage: String;
    fException: String;
    fNdc: String;
public
    constructor Create(); overload;
    constructor Create(index: integer; time: TDateTime; logger: String; level: TVLevelValue; msg: String; ndc: String); overload;
    constructor Create(other: TVLoggingEvent); overload;
    destructor Destroy(); override;


    function GetIndex(): integer;
    procedure SetIndex(index: integer);

    function GetLevel(): TVLevelValue;
    procedure SetLevel(level: TVLevelValue);

    function GetTime(): TDateTime;
    procedure SetTime(time: TDateTime);

    function GetLogger(): String;
    procedure SetLogger(logger: String);

    function GetException(): String;
    procedure SetException(exception: String);

    function GetNdc(): String;
    procedure SetNdc(ndc: String);

    function GetMessage(): String;
    procedure SetMessage(value: String);

    function Assign(other: TVLoggingEvent): TVLoggingEvent;

    procedure Copy(event: TVLoggingEvent);
published
    property Index: Integer read GetIndex write SetIndex;
    property Level: TVLevelValue read GetLevel write SetLevel;
    property Time: TDateTime read GetTime write SetTime;
    property Logger: String read GetLogger write SetLogger;
    property Ndc: String read GetNdc write SetNdc;
    property Message: String read GetMessage write SetMessage;
end;



TVFilterResult = (frDeny, frAccept);
TVFilterDecideEvent = procedure(loggingEvent: TVLoggingEvent; var decision: TVFilterResult) of object;


TVFilter = class(TComponent)
public
  constructor Create(Owner: TComponent); override;
  destructor Destroy(); override;
  function Decide(event: TVLoggingEvent): TVFilterResult; virtual;
private
  fOnDecide: TVFilterDecideEvent;
published
  property OnDecide: TVFilterDecideEvent read fOnDecide write fOnDecide;
end;



TLayout = class(TObject)
public
    function Format(event: TVLoggingEvent): String; virtual; abstract;
end;


TBasicLayout = class(TLayout)
private
public
  constructor Create;
  function format(logEvent: TVLoggingEvent): String; override;
end;

TVAppendEvent = procedure(loggingEvent: TVLoggingEvent) of object;

TVCustomAppender = class(TComponent)
protected
    procedure DoAppend(event: TVLoggingEvent); virtual; abstract;
public
    function GetAppenderName(): String; virtual; abstract;
    function GetInitialized(): boolean; virtual; abstract;
    procedure Initialize(); virtual; abstract;
    function  RequiresLayout(): boolean; virtual; abstract;
    procedure SetLevel(level: TVLevelValue); virtual; abstract;
    function GetLevel(): TVLevelValue; virtual; abstract;
    procedure SetFilter(filter: TVFilter); virtual; abstract;
    function  GetFilter(): TVFilter; virtual; abstract;
    function GetAttachToRoot(): boolean; virtual; abstract;
    procedure SetAttachToRoot(value: boolean); virtual; abstract;
    function GetAsynchronous(): boolean; virtual; abstract;
    procedure SetAsynchronous(value: boolean); virtual; abstract;
    procedure Flush(); virtual; abstract;
protected
    procedure SetOnAppend(event: TVAppendEvent); virtual; abstract;
    function GetOnAppend(): TVAppendEvent; virtual; abstract;
    procedure AppendFirstBufferedEvent(); virtual; abstract;
    function HasBufferedEvents(): boolean; virtual; abstract;
    function GetBufferedEventListCount(): Integer; virtual; abstract;
published
    property AppenderName: String read GetAppenderName;
    property Level: TVLevelValue read GetLevel  write SetLevel;
    property AttachToRoot: boolean read GetAttachToRoot write SetAttachToRoot default true;
    property OnAppend: TVAppendEvent read GetOnAppend write SetOnAppend;
    property Asynchronous: boolean read GetAsynchronous write SetAsynchronous;
end;


type TVAppenderThread = class(TThread)
private
    fAppender: TVCustomAppender;
protected
    procedure Stop();
public
    constructor Create(suspended: boolean; appender: TVCustomAppender);
    procedure Execute(); override;
end;


TVAppenderErrorEvent = procedure(appender: TVCustomAppender; errorText: String) of object;


type TVAppender = class(TVCustomAppender)
public
    constructor CreateWithName(owner: TComponent; name: String);
    constructor Create(owner: TComponent); override;
    destructor Destroy(); override;
    procedure Initialize(); override;
    function GetInitialized(): boolean; override;
    function GetAppenderName(): String; override;
    function  RequiresLayout(): boolean; override;
    procedure  SetLayout(layout: TLayout);
    function  GetLayout(): TLayout;
    function GetAttachToRoot(): boolean; override;
    procedure SetAttachToRoot(value: boolean); override;
    procedure SetLevel(level: TVLevelValue); override;
    function GetLevel(): TVLevelValue; override;
    procedure SetFilter(filter: TVFilter); override;
    function  GetFilter(): TVFilter; override;
    procedure Flush(); override;
    function GetAsynchronous(): boolean; override;
    procedure SetAsynchronous(value: boolean); override;
protected
    procedure Append(event: TVLoggingEvent); virtual; //abstract;
    procedure DoAppend(event: TVLoggingEvent); override;
    procedure OnInitialize(); virtual;
    procedure SetOnDecide(event: TVFilterDecideEvent);
    procedure SetOnAppend(event: TVAppendEvent); override;
    function GetOnAppend(): TVAppendEvent; override;
    procedure AppendFirstBufferedEvent(); override;
    function HasBufferedEvents(): boolean; override;
    function GetBufferedEventListCount(): Integer; override;
private
    fStopping : boolean;
    fOnError: TVAppenderErrorEvent;
    fOnDecide: TVFilterDecideEvent;
    fBufferedEventList: TList;
    fASynchThread : TVAppenderThread;
    fAfterInitialize: TNotifyEvent;
    fMutex : TCriticalSection;
    fLayout: TLayout;
    fAppenderName: String;
    fFilter: TVFilter;
    fLevel: TVLevelValue;
    fAttachToRoot: boolean;
    fInitialized: boolean;
    fOnAppend: TVAppendEvent;
    fAsynchronous: boolean;
    procedure _AppendFirstBufferedEvent();
published
    property AppenderName: String read GetAppenderName;
    property Level: TVLevelValue read GetLevel  write SetLevel;
    property AttachToRoot: boolean read GetAttachToRoot write SetAttachToRoot default true;
    property OnAppend: TVAppendEvent read fOnAppend write SetOnAppend;
    property OnDecide: TVFilterDecideEvent read fOnDecide write SetOnDecide;
    property AfterInitialize: TNotifyEvent read fAfterInitialize write fAfterInitialize;
    property Layout: TLayout read GetLayout write SetLayout;
    property OnError: TVAppenderErrorEvent read fOnError write fOnError;
end;



TVConsoleAppender = class(TVAppender)
protected
  procedure Append(event: TVLoggingEvent); override;
public
  constructor Create(owner: TComponent); override;
  destructor Destroy(); override;
  procedure Reset();
published
end;



TVFileAppender = class(TVAppender)
private
  fFileName: String;
  fFile : TFileStream;
  fFileMutex: TCriticalSection;
  procedure CreateTheFile();
protected
  procedure Append(event: TVLoggingEvent); override;
  function GetFile(): TFileStream;
  function GetSize(): Integer;
public
  constructor Create(owner: TComponent); override;
  destructor Destroy(); override;

  procedure SetFileName(fileName: String);
  function GetFileName(): String;
  procedure Reset();
  procedure Close();
published
  property FileName: String read GetFileName write SetFileName;
end;



TVRollingFileAppender = class(TVFileAppender)
private
    fMaxSize: Integer;          // Maximum size that the log file is allowed to reach before being rolled over to backup files.
    fMaxBackupIndex: Integer;   // Maximum number of backup files to keep around
    fAppendsSinceSizeCheck: Integer;
    function IsTimeForRollover(): Boolean;
    procedure RollOver();
protected

public
    constructor Create(Owner: TComponent); override;
    destructor Destroy(); override;
    procedure Append(event: TVLoggingEvent); override;
published
    property MaxSize: Integer read fMaxSize write fMaxSize;
    property MaxBackupIndex: Integer read fMaxBackupIndex write fMaxBackupIndex;
end;



TVLogger = class(TComponent)
  public
    constructor Create(owner: TComponent); override;
    destructor Destroy(); override;

    class function GetRootLogger(): TVLogger;
    class function GetInstance(name: String): TVLogger;
    class function GetCurrentLoggers(): TStringList;
    class procedure RemoveAppenderFromAll(appender: TVCustomAppender);

    function Exists(name: String): TVLogger;
    function GetCategory(): String;
    procedure SetCategory(name: String);
    function GetLevel(): TVLevelValue;
    procedure SetLevel(level: TVLevelValue);
    procedure AddAppender(appender: TVAppender);
    function GetEffectiveLevel(): TVLevelValue;
    function GetParent(): TVLogger;
    procedure ForceChildrenToInherit(recurse: boolean);

    function GetAppender(name: String): TVAppender;
    procedure RemoveAllAppenders();
    procedure RemoveAppender(appender: TVCustomAppender);
    procedure SendEventToAppenders(event: TVLoggingEvent);

    function GetAdditivity(): boolean;
    procedure SetAdditivity(additivity: boolean);
    procedure LogIt(level: TVLevelValue; msg: AnsiString); overload;
    procedure LogIt(level: TVLevelValue; msg: AnsiString; ex: Exception); overload;
    function IsEnabledFor(level: TVLevelValue): boolean;
    procedure Log(level: TVLevelValue; msg: String);

    procedure Trace(msg: String); overload;
    procedure Trace(msg: String; ex: Exception); overload;
    function IsTraceEnabled(): boolean;

    procedure Debug(msg: String); overload;
    procedure Debug(msg: String; ex: Exception); overload;
    function IsDebugEnabled(): boolean;

    procedure Info(msg: String); overload;
    procedure Info(msg: String; ex: Exception); overload;
    function IsInfoEnabled(): boolean;

    procedure Warn(msg: String); overload;
    procedure Warn(msg: String; ex: Exception); overload;
    function IsWarnEnabled(): boolean;

    procedure Error(msg: String); overload;
    procedure Error(msg: String; ex: Exception); overload;
    function IsErrorEnabled(): boolean;

    procedure Fatal(msg: String); overload;
    procedure Fatal(msg: String; ex: Exception); overload;
    function IsFatalEnabled(): boolean;

    function GetLoggerChildren(): TList;
  private
    fSettingLevel: boolean;
    fName: String;
    fParent: TVLogger;
    fLevel: TVLevelValue;
    fAdditive: boolean;
    fAppenderList: TStringList;
  protected
    constructor CreateBase(owner: TComponent; name: String; parent: TVLogger; level: TVLevelValue);
  published
    property Level: TVLevelValue read GetLevel write SetLevel;
    property Category: String read GetCategory write SetCategory;
end;



TVLog = class(TComponent)
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy(); override;
    function GetCategory(): String;
    procedure SetCategory(name: String);
    procedure RemoveAppenderFromAll(appender: TVCustomAppender);
    procedure SetLevel(level: TVLevelValue);
    function GetBaseLogger(): TVLogger;
    function GetLevel(): TVLevelValue;
    procedure Log(priority: TVLevelValue; msg: String);
    procedure SetAdditivity(additivity: boolean);
    procedure RemoveAppender(appender: TVAppender);
    procedure RemoveAllAppenders();
    function GetAppender(name: String): TVAppender;
    procedure AddAppender(appender: TVAppender);
    function IsEnabledFor(level: TVLevelValue): boolean;
    function GetEffectiveLevel(): TVLevelValue;
    function GetRootLogger(owner: TComponent): TVLog;
    function GetAdditivity(): boolean;
    function GetInstance(owner: TComponent; name: String): TVLog;
    function GetParent(owner: TComponent): TVLog;

    function IsTraceEnabled(): boolean;
    procedure Trace(msg: String); overload;
    procedure Trace(msg: String; ex: Exception); overload;

    function IsDebugEnabled(): boolean;
    procedure Debug(msg: String); overload;
    procedure Debug(msg: String; ex: Exception); overload;

    function IsInfoEnabled(): boolean;
    procedure Info(msg: String); overload;
    procedure Info(msg: String; ex: Exception); overload;

    function IsWarnEnabled(): boolean;
    procedure Warn(msg: String); overload;
    procedure Warn(msg: String; ex: Exception); overload;

    function IsErrorEnabled(): boolean;
    procedure Error(msg: String); overload;
    procedure Error(msg: String; ex: Exception); overload;

    function IsFatalEnabled(): boolean;
    procedure Fatal(msg: String); overload;
    procedure Fatal(msg: String; ex: Exception); overload;

  private
    fName : String;
  published
    property Category: String read GetCategory write SetCategory;
    property Level: TVLevelValue read GetLevel write SetLevel default lvInfo;
end;


TVDisplayManager = class(TComponent)
  private
    fSeverityIcons: TImageList;
    fLevelInhIcons : TImageList;
  protected
    procedure FindResources();
    function MakeBitmap(dataConst: PAnsiChar): Graphics.TBitmap;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy(); override;
    function GetSeverityIndex(severity: TVLevelValue): Integer;
    //function GetBitmap(resourceName: String): Graphics.TBitmap;
    function GetSeverityIcons(): TImageList;
    function GetLevelInhIcons(): TImageList;

    function GetPreviousButtonImage(owner: TComponent): TImage;
    function GetNextButtonImage(): TImage;

//    class function GetInstance(): TVDisplayManager;

end;



type TVTestSpeed = (tsSlow, tsMedium, tsFast, tsFullThrottle);

TVTestEventGenerator = class(TComponent)
private
    fTimer: TTimer;
    procedure TimerTriggered(Sender: TObject);
public
    constructor Create(owner: TComponent); override;
    destructor Destroy(); override;
    procedure SetEnabled(value: boolean);
    function GetEnabled(): boolean;
    procedure SetSpeed(value: TVTestSpeed);
    function GetSpeed(): TVTestSpeed;
published
    property Enabled: boolean read GetEnabled write SetEnabled;
    property Speed: TVTestSpeed read GetSpeed write SetSpeed;
end;


TVLogManager = class(TComponent)
public
    class function GetInstance(): TVLogManager;
    constructor Create(Owner: TComponent); override;
    destructor Destroy(); override;
    function GetCurrentLoggers(): TStringList;
    procedure InitializeAppenders();
    function FindAppender(name: AnsiString): TVCustomAppender;
    procedure AddAppender(appender: TVCustomAppender);
    function GetAppenderList(): TStringList;
    procedure RemoveAppender(appender: TVCustomAppender);
    function GetNdc(): AnsiString;
    procedure SetNdc(ndc: AnsiString);
    function GetNextId(): Integer;
    function NextId(): Integer;
    function GetLogger(name: AnsiString): TVLogger;
    function GetCreateLogger(loggerName: AnsiString): TVLogger;
    class function IsShuttingDown(): boolean;

    function GetNeedsAppenderInitialize(): boolean;
    procedure SetNeedsAppenderInitialize(value: boolean);
    procedure SetNextId(nextId: Integer);
    function GetDisplayManager(): TVDisplayManager;
    procedure SaveLevel(logger: TVLogger);
    procedure LoadLevel(logger: TVLogger);
    procedure SaveLevels();
    procedure LoadLevels();
    procedure FlushAllAppenders();
protected
    function FindFileComponent(fileName: String): TComponent;
    procedure RegisterFileComponent(fileComponent: TComponent; fileName: String);
    procedure UnregisterFileComponent(fileComponent: TComponent);
private
    fFileComponentList: TStringList;
    fDisplayManager: TVDisplayManager;
    fLoadedLevels: boolean;
    fAppenderList: TStringList;
    fLoggerList: TStringList;
    fNeedsAppenderInitialize : boolean;
    fNdc: AnsiString;
    function GetCategoryKeyName(): String;
end;



TVLogPersistence = class(TComponent)
private
    fFirstAppend : Boolean;
    fLevel : TVLevelValue;
    fManagedAppender : TVAppender;
    fMaxSize: Integer;
protected
    procedure AppendEvent(logEvent: TVLoggingEvent); virtual; abstract;
    function MakeEventKey(event: TVLoggingEvent): String;
    procedure OnAppend(event: TVLoggingEvent);
    procedure OnAppenderInitialized(Sender: TObject); virtual;
public
    constructor Create(Owner: TComponent); override;
    destructor Destroy(); override;

    function GetAppender(): TVAppender;
    function GetSize(): Integer; virtual; abstract;

    function FindEventRange(_startId: Integer; _endId: Integer; list: TVPageList; filter: TVFilter = nil): Integer; virtual; abstract;
    function FindEventPage(_endId: Integer; count: integer; list: TVPageList; filter: TVFilter = nil): Integer; virtual; abstract;
    function FindEvent(id: Integer): TVLoggingEvent; virtual; abstract;
    function FindFirstIndex(): Integer; virtual; abstract;
    function FindLastIndex(): Integer; virtual; abstract;
    function FindNextPageIndex(_fromIndex: Integer; pageSize: Integer; filter: TVFilter): Integer;
    function FindPreviousPageIndex(_fromIndex: Integer; pageSize: Integer; filter: TVFilter): Integer;
    function FindIndexAtPosition(position: Integer): Integer; virtual; abstract;
    function FindPositionOfIndex(currentIndex: Integer): Integer; virtual; abstract;

    function GetWriteLevel: TVLevelValue;
    procedure SetWriteLevel(level: TVLevelValue);

    function GetReadLevel(): TVLevelValue;
    procedure SetReadLevel(value: TVLevelValue);

    procedure Flush();

    procedure SetAsynchronous(value: boolean);
    function GetAsynchronous(): boolean;
    //procedure SetLevel(level: TVLevelValue);
    //function GetLevel(): TVLevelValue;

    procedure Clear(); virtual; abstract;
published
    property Asynchronous: boolean read GetAsynchronous write SetAsynchronous;
    property Appender: TVAppender read GetAppender;
    property LevelForWrite: TVLevelValue read GetWriteLevel write SetWriteLevel;
    property LevelForRead: TVLevelValue read GetReadLevel write SetReadLevel;
    property MaxSize: Integer read fMaxSize write fMaxSize;
end;


TVFileLogPersistence = class(TVLogPersistence)
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy(); override;
    procedure CloseWriter();
    procedure OpenWriter();
    procedure Clear(); override;
    function GetSize(): Integer; override;
    function FindEventPage(_endId: integer; maxCount: Integer; list: TVPageList; filter: TVFilter): Integer; override;
    function FindLastIndex(): Integer; overload; override;
    function FindLastIndexInFile(theFile: TFileStream): Integer;
    function FindFirstIndex(): Integer; overload; override;
    function FindFirstIndexInFile(theFile: TFileStream): Integer;
    function FindNextIndexInStream(seekPos: Integer; theFile: TFileStream; atLineStart: boolean): Integer; overload;
    function FindNextLineInStream(seekPos: Integer; theFIle: TFileStream; var line: String; atLineStart: boolean): Integer; overload;
    function FindEvent(id: Integer): TVLoggingEvent; override;

    function FindEventRangeInFile(theFile: TFileStream; _startId: Integer; _endId: Integer; list: TVPageList; filter: TVFilter): Integer;
    function FindEventRange(_startId: Integer; _endId: Integer; list: TVPageList; filter: TVFilter): Integer; override;
    function GetFileName(): String;
    procedure SetFileName(fileName: String);
    function FindIndexAtPosition(position: Integer): Integer; override;
    function FindPositionOfIndex(index: Integer): Integer; override;

    procedure AppendEvent(logEvent: TVLoggingEvent); override;
  published
    property FileName: String read GetFileName write SetFileName;
    property Size: Integer read GetSize;
  protected
    function FindPositionOfIndexInFile(theFile: TFileStream; index: Integer; firstIndex: Integer; lastIndex: Integer): Integer;
    function FindPosBackwards(theFile: TFileStream; startPos: Integer; backupLines: Integer): Integer;
    procedure OnAppenderInitialized(Sender: TObject); override;
  private
    //Log: TVLogger;
    fWriteFile: TFileStream;
    fFileReducePercent: Integer;
    fAppendsSinceSizeCheck: Integer;
    fFileMutex: TCriticalSection;
    fFileName: String;
    fOpenedFile: String;
    function OpenReader(): TFileStream;
    function LineToEvent(theLine: String): TVLoggingEvent;
    function Decode(before: String): String;
    function Encode(before: String): String;
    //function GetLineIndex(_line: String): Integer;
    procedure CheckReduceFileSize();
    function FindEventPageForwardInFile(theFile: TFileStream; _startId: integer;
        maxCount: Integer; list: TVPageList; filter: TVFilter; lastIndexInFile: Integer): Integer;
    procedure CreateTheFile();
end;


TVMemoryLogPersistence = class(TVLogPersistence)
  public
    constructor Create(Owner: TComponent); override;
    procedure Clear(); override;
    destructor Destroy(); override;
    function FindFirstIndex(): Integer; override;
    function GetAll(): TList;
    function GetSize(): Integer; override;
    function FindEventRange(_startId: Integer; _endId: Integer; list: TVPageList; filter: TVFilter): Integer; override;
    function FindEventPage(_endId: Integer; count: Integer; list: TVPageList; filter: TVFilter): Integer; override;
    function FindEvent(id: Integer): TVLoggingEvent; override;
    function FindIndexAtPosition(position: Integer): Integer; override;
    function FindPositionOfIndex(index: Integer): Integer; override;
    function FindLastIndex(): Integer; override;
  protected
    procedure AppendEvent(logEvent: TVLoggingEvent); override;

  private
    fLoggingEventList : TList;
    fMaxCount : Integer;
    fListMutex : TCriticalSection;
end;


{
TVLevelValuePropertyEditor = class of TPropertyEditor
public:
  function GetAttributes: TPropertyAttributes; override;

end;
}

procedure Register;
procedure FileCopy(const FromFile, ToFile: string);

const
    ALL_SYMBOL = 'ALL';
    TRACE_SYMBOL = 'TRACE';
    DEBUG_SYMBOL = 'DEBUG';
    INFO_SYMBOL = 'INFO';
    WARN_SYMBOL = 'WARN';
    ERROR_SYMBOL = 'ERROR';
    FATAL_SYMBOL = 'FATAL';
    OFF_SYMBOL = 'OFF';
    UNNAMED_LOGGER = 'unnamed';

    TAB_CHAR = #9;
    EOL_CHAR = #13;
    CR_CHAR = #10;
    EST_SIZE_PER_RECORD = 200;



implementation

uses Registry, Windows, Bitmaps;


var
  fAppenderInstanceNumber: Integer;
  REGISTRY_KEY_PREFIX : String;
  REGISTRY_KEY_SUFFIX : String;
  fInstance: TVLogManager;
  LogManagerShuttingDown: boolean;
  fNextId: Integer;



//==============================================================================
//==============================================================================
//==============================================================================
//==============================================================================
//==============================================================================



constructor TVAppender.CreateWithName(owner: TComponent; name: String);
begin
    inherited Create(TVLogManager.GetInstance());
    fAppenderInstanceNumber := fAppenderInstanceNumber + 1;
    fAppenderName := name;
    fBufferedEventList := TList.Create();
    fFilter := nil;
    fOnAppend := nil;
    fLevel := lvInherit;
    fAttachToRoot := true;
    fLayout := nil;
    fInitialized := false;
    fMutex := TCriticalSection.Create();
    fAsynchronous := false;

    if not (csDesigning in ComponentState) then begin
        TVLogManager.GetInstance().AddAppender(self);
        TVLogManager.GetInstance().SetNeedsAppenderInitialize(true);
    end;
end;


constructor TVAppender.Create(owner: TComponent);
var
  thisAppender: TVCustomAppender;
begin
    inherited Create(owner);
    fMutex := TCriticalSection.Create();
    fStopping := false;

    fMutex.Acquire();
    fAppenderInstanceNumber := fAppenderInstanceNumber + 1;
    fAppenderName :=  'APPENDER_' + IntToStr(fAppenderInstanceNumber);
    fBufferedEventList := TList.Create();
    fFilter := nil;
    fOnAppend := nil;
    fLevel := lvInherit;
    fAttachToRoot := true;
    fLayout := nil;
    fInitialized := false;
    fAsynchronous := true;
    thisAppender := self;

    if not (csDesigning in ComponentState) then begin
        TVLogManager.GetInstance().AddAppender(thisAppender);
        TVLogManager.GetInstance().SetNeedsAppenderInitialize(true);
    end;

    fMutex.Release();
end;


destructor TVAppender.Destroy();
begin
   // Flush();

    fMutex.Acquire();
    fStopping := true;
    fLevel := lvOff;
    SetAsynchronous(false);

    if not (csDesigning in ComponentState) then begin
        if (TVLogManager.IsShuttingDown() = false) then begin
            TVLogManager.GetInstance().RemoveAppender(self);
        end;
    end;




    if (fLayout <> nil) then begin
        fLayout.Destroy();
    end;

    if (fFilter <> nil) then
        fFilter.Destroy();
    fBufferedEventList.Destroy();
    fMutex.Destroy();

    inherited;
end;




procedure TVAppender.Flush();
begin
    if (fStopping = false) then begin
        if (fBufferedEventList.Count > 0) then begin
            fMutex.Acquire();
            while (fBufferedEventList.Count > 0) do begin
                _AppendFirstBufferedEvent();
            end;

            fMutex.Release();
        end;
    end;
end;


function TVAppender.GetBufferedEventListCount(): Integer;
begin
    result := fBufferedEventList.Count;
end;


function TVAppender.HasBufferedEvents(): boolean;
begin
    result := (fBufferedEventList.Count > 0);
end;


procedure TVAppender.AppendFirstBufferedEvent();
begin
    if (fBufferedEventList.Count > 0) then begin
        fMutex.Acquire();
            _AppendFirstBufferedEvent();
        fMutex.Release();
    end;
end;


// Same as previous method, just doesn't acquire the mutex.
procedure TVAppender._AppendFirstBufferedEvent();
var
    event: TVLoggingEvent;
begin
    if ((fStopping = false) and (fBufferedEventList.Count > 0)) then begin
        event := TVLoggingEvent(fBufferedEventList.Items[0]);
        Append(event);
        fBufferedEventList.Delete(0);
        event.Destroy();
    end;
end;


function TVAppender.GetAsynchronous(): boolean;
begin
    result := (fAsynchThread <> nil);
end;


procedure TVAppender.SetAsynchronous(value: boolean);
begin
    if (value = false) then begin
        if (fAsynchThread <> nil) then begin
            fAsynchThread.Stop();
            fAsynchThread.Destroy();
            fAsynchThread := nil;
        end;
        Flush();
    end
    else begin
        if (fAsynchThread = nil) then begin
            fASynchThread := TVAppenderThread.Create(false, self);
        end;
    end;
end;


function TVAppender.GetOnAppend(): TVAppendEvent;
begin
  result := fOnAppend;
end;


procedure TVAppender.SetOnAppend(event: TVAppendEvent);
begin
  fOnAppend := event;
end;

procedure TVAppender.SetOnDecide(event: TVFilterDecideEvent);
begin
    fOnDecide := event;
end;


procedure TVAppender.OnInitialize();
begin
end;

procedure TVAppender.Initialize();
begin
    fMutex.Acquire();
    if (fAttachToRoot) then begin
        TVLogger.GetRootLogger().AddAppender(self);
    end;
    OnInitialize();
    fInitialized := true;
    if (Assigned(fAfterInitialize)) then begin
      fAfterInitialize(self);
    end;

    fMutex.Release();
end;

function TVAppender.GetInitialized(): boolean;
begin
    result := fInitialized;
end;

function TVAppender.GetAppenderName(): String;
begin
  result := fAppenderName;
end;


function  TVAppender.RequiresLayout(): boolean;
begin
    result := true;
end;

procedure  TVAppender.SetLayout(layout: TLayout);
begin
    fLayout := layout;
end;


function  TVAppender.GetLayout(): TLayout;
begin
    result := fLayout;
end;


procedure TVAppender.DoAppend(event: TVLoggingEvent);
var
    newEvent: TVLoggingEvent;
    eventOk: boolean;
    filterResult: TVFilterResult;
begin
    if (fStopping = false) then begin

        fMutex.Acquire();
        if (event.GetLevel() >= fLevel) then begin
            if (Assigned(fOnDecide)) then begin
                filterResult := frAccept;
                fOnDecide(event, filterResult);
                eventOk := (filterResult <> frDeny);
            end
            else begin
                eventOk := (fFilter = nil) or (fFilter.Decide(event) <> frDeny);
            end;

            if (eventOk) then begin
                if (Asynchronous) then begin
                    newEvent := TVLoggingEvent.Create(event);
                    fBufferedEventList.Add(newEvent);
                end
                else begin
                    Append(event);
                end;
            end
        end;
        fMutex.Release();
    end;
end;


// Default handler for append, calls OnAppend event.
// Derived classes can override this for custom behavior.
procedure TVAppender.Append(event: TVLoggingEvent);
begin
    if (Assigned(fOnAppend)) then begin
      fOnAppend(event);
    end;
end;

function TVAppender.GetAttachToRoot(): boolean;
begin
    result := fAttachToRoot;
end;

procedure TVAppender.SetAttachToRoot(value: boolean);
begin
    fAttachToRoot := value;

    if (fInitialized) then begin
        fInitialized := false;
        Initialize();
    end;
end;


procedure TVAppender.SetLevel(level: TVLevelValue);
begin
    fLevel := level;
end;


function TVAppender.GetLevel(): TVLevelValue;
begin
    result := fLevel;
end;


procedure TVAppender.SetFilter(filter: TVFilter);
begin
    if (fFilter <> filter) then begin
        if (fFilter <> nil) then
            fFilter.Destroy();

        fFilter := filter;
    end;
end;


function  TVAppender.GetFilter(): TVFilter;
begin
    result := fFilter;
end;




//========================================================================================
//========================================================================================
//========================================================================================
//========================================================================================
//========================================================================================
//========================================================================================

constructor TVAppenderThread.Create(suspended: boolean; appender: TVCustomAppender);
begin
    inherited Create(suspended);
    fAppender := appender;
end;

procedure TVAppenderThread.Stop();
begin
    Terminate();
    WaitFor();
end;


procedure TVAppenderThread.Execute();
begin
    while (Terminated = false) do begin
        Sleep(10);
        
        try
            if (fAppender.HasBufferedEvents()) then begin
                if (fAppender.GetBufferedEventListCount() > 100) then begin
                    Synchronize(fAppender.Flush);
                end
                else begin
                    Synchronize(fAppender.AppendFirstBufferedEvent);
                end;
            end;
        except
            on ex: Exception do
                raise TVAppenderThreadException.Create('Exception during asynch appending (in Appender Thread). ' + ex.Message);
            else
                raise TVAppenderThreadException.Create('Exception during asynch appending (in Appender Thread). ');
        end;

    end;
end;


//========================================================================================
//========================================================================================
//========================================================================================
//========================================================================================
//========================================================================================
//========================================================================================

constructor TVConsoleAppender.Create(owner: TComponent);
begin
  inherited;
  SetLayout(TBasicLayout.Create());
end;


destructor TVConsoleAppender.Destroy();
begin
    inherited;
end;


procedure TVConsoleAppender.Reset();
begin
end;


procedure TVConsoleAppender.Append(event: TVLoggingEvent);
var
  line: String;
begin
    line := GetLayout().Format(event);
    Write(line);
end;





//========================================================================================
//========================================================================================
//========================================================================================
//========================================================================================
//========================================================================================
//========================================================================================

constructor TVFileAppender.Create(owner: TComponent);
begin
  inherited;
  fFileMutex := TCriticalSection.Create();
  fFileName := 'application.log';
  fFile := nil;
  SetLayout(TBasicLayout.Create());
end;


destructor TVFileAppender.Destroy();
begin
  if (LogManagerShuttingDown = false) then begin
    TVLogManager.GetInstance().UnregisterFileComponent(self);
  end;

  Close();
  fFileMutex.Acquire();
  fFileMutex.Release();
  inherited;
end;


function TVFileAppender.GetFile(): TFileStream;
var
    fileComponent: TComponent;
begin
    if (fFile = nil) then begin
        fileComponent := TVLogManager.GetInstance().FindFileComponent(fFileName);
        if (fileComponent = nil) then begin
            TVLogManager.GetInstance().RegisterFileComponent(self, fFileName);
        end
        else if (fileComponent = self) then begin

        end
        else begin
            //result := nil;
            TVLogManager.GetInstance().UnRegisterFileComponent(self);
            raise TVFileException.Create('TVFileAppender attempting to write to a file that is already owned by another appender or file persistence.');
        end;

        // Create the file if it doesn't exist
        CreateTheFile();

        // Open file for r/w and allow others to read it
        fFile := TFileStream.Create(fFileName, fmOpenReadWrite or fmShareDenyNone);
        fFile.Seek(0, soFromEnd);
        result := fFile;
    end
    else begin
        result := fFile;
    end;
end;

procedure TVFileAppender.CreateTheFile();
var
  theFile: TFileStream;
begin
    if not FileExists(fFileName) then begin
        theFile := TFileStream.Create(fFileName, fmCreate);
        theFile.Destroy;
    end;
end;

procedure TVFileAppender.Close();
begin
    if (fFile <> nil) then begin
        fFile.Destroy();
    end;
    fFile := nil;
end;


procedure TVFileAppender.Reset();
begin
    Close();
end;


procedure TVFileAppender.Append(event: TVLoggingEvent);
var
  line: String;
  buffer: PChar;
  theFile: TFileStream;
begin
    line := GetLayout().Format(event);
    buffer := PChar(line);
    theFile := GetFile();
    theFile.Write(buffer^, Length(line));

end;


function TVFileAppender.GetFileName(): String;
begin
  result := fFileName;
end;


procedure TVFileAppender.SetFileName(fileName: String);
begin
    fFileName := fileName;
    Reset();
end;

function TVFileAppender.GetSize(): Integer;
var size: Integer;
    theFile: TFileStream;
begin
    fFileMutex.Acquire();
    theFile := GetFile();
    if (theFile = nil) then size := 0
    else size := theFile.Size;
    fFileMutex.Release();
    result := size;
end;


//========================================================================================
//========================================================================================
//========================================================================================
//========================================================================================
//========================================================================================
//========================================================================================

constructor TVRollingFileAppender.Create(owner: TComponent);
begin
    inherited;
    fMaxSize:=5;          // Default 5MB
    fMaxBackupIndex:=3;   // Three backup files by default
    fAppendsSinceSizeCheck:=0;
end;

destructor TVRollingFileAppender.Destroy;
begin
    inherited;
end;

procedure TVRollingFileAppender.Append(event: TVLoggingEvent);
var
    line: String;
    buffer: PChar;
    theFile: TFileStream;
begin
    if (fAppendsSinceSizeCheck >= 1000) then begin
        if IsTimeForRollover() then begin
            RollOver();
        end;
        fAppendsSinceSizeCheck := 0;
    end;

    line := GetLayout().Format(event);
    buffer := PChar(line);
    theFile := GetFile();
    theFile.Write(buffer^, Length(line));

    Inc(fAppendsSinceSizeCheck);
end;

{ Checks if the maximum size is reached }
function TVRollingFileAppender.IsTimeForRollover(): Boolean;
begin
    result := GetSize > (fMaxSize * ONE_MEG);
end;

{ Rolls over:
    Log file with extension fMaxBackupIndex is deleted,
    file with extension fMaxBackupIndex-1 is renamed to fMaxBackupIndex,
    index is increased for other backed up files,
    the extension of the current log file is changed to 1,
    (a new current log file is created on next call to GetFile)
  Throws TVFileException if anything goes wrong }
procedure TVRollingFileAppender.RollOver();
var lastRollingFileName: String; i: Integer;
    f: file;
begin
    // Delete the last rolling file
    lastRollingFileName := fFileName + '.' + IntToStr(fMaxBackupIndex);
    if FileExists(lastRollingFileName) then begin
        if not SysUtils.DeleteFile(lastRollingFileName) then
            raise TVFileException.Create('RollingFileAppender unable to delete last file');
    end;

    // Rename all other rolling files
    for i:= fMaxBackupIndex-1 downto 1 do begin
        if FileExists(fFileName + '.' + IntToStr(i)) then
            if not RenameFile(fFileName + '.' + IntToStr(i), fFileName + '.' + IntToStr(i+1)) then
                raise TVFileException.Create('RollingFileAppender unable to rename rolling file '+IntToStr(i));
    end;

    // Make sure the file is closed (all events flushed and file ready for renaming)
    // The file will open (and be created if needed) on next append through GetFile()
    Close;

    // Copy the contents of the file
    fileCopy(fFileName, fFileName + '.1');

    // Reset the previous file
    AssignFile(f, fFileName);
    Rewrite(f);
    CloseFile(f);

end;

procedure FileCopy(const FromFile, ToFile: string);
 var
  FromF, ToF: file;
  NumRead, NumWritten: Integer;
  Buf: array[1..10240] of Char;
begin
  AssignFile(FromF, FromFile);
  Reset(FromF, 1);{ Record size = 1 }
  AssignFile(ToF, ToFile);{ Open output file }
  Rewrite(ToF, 1);{ Record size = 1 }
  repeat
    BlockRead(FromF, Buf, SizeOf(Buf), NumRead);
    BlockWrite(ToF, Buf, NumRead, NumWritten);
  until (NumRead = 0) or (NumWritten <> NumRead);
  CloseFile(FromF);
  CloseFile(ToF);
end;


//========================================================================================
//========================================================================================
//========================================================================================
//========================================================================================
//========================================================================================
//========================================================================================




constructor TVTestEventGenerator.Create(owner: TComponent);
begin
    inherited Create(owner);
    fTimer := TTimer.Create(self);
    fTimer.Enabled := false;
    SetEnabled(false);
    SetSpeed(tsMedium);
    fTimer.OnTimer := TimerTriggered;
end;

destructor TVTestEventGenerator.Destroy();
begin
    inherited;
end;


procedure TVTestEventGenerator.SetEnabled(value: boolean);
begin
    fTimer.Enabled := value;
end;

function TVTestEventGenerator.GetEnabled(): boolean;
begin
    result := fTimer.Enabled;
end;

procedure TVTestEventGenerator.SetSpeed(value: TVTestSpeed);
begin
        if (value = tsSlow) then begin
            fTimer.Interval := 1000;
        end
        else if (value = tsMedium) then begin
            fTimer.Interval := 100;
        end
        else if (value = tsFast) then begin
            fTimer.Interval := 10;
        end
        else if (value = tsFullThrottle) then begin
            fTimer.Interval := 1;
        end;
end;



function TVTestEventGenerator.GetSpeed(): TVTestSpeed;
begin
    if (fTimer.Interval >= 1000) then
        result := tsSlow
    else if (fTimer.Interval >= 100) then
        result := tsMedium
    else if (fTimer.Interval >= 10) then
        result := tsFast
    else
        result := tsFullThrottle;
end;

procedure TVTestEventGenerator.TimerTriggered(Sender: TObject);
begin
    if not (csDesigning in ComponentState) then begin

        TVLogger.GetInstance('fruit.banana.green').Trace('This is a trace message');

        TVLogger.GetInstance('fruit.banana.yellow').Debug('This is a debug message');
        TVLogger.GetInstance('fruit.banana.green').Info('This is an info message');
        TVLogger.GetInstance('fruit.banana.purple').Warn('This is a warning message - purple bananas are not commonly found.');

        TVLogger.GetInstance('fruit.banana.green').Error('This is an error message');

        TVLogger.GetInstance('fruit.apple.green').Debug('This is a debug message');
        TVLogger.GetInstance('fruit.apple.green').Info('This is an info message');
        TVLogger.GetInstance('fruit.apple.green').Warn('This is an info message');
        TVLogger.GetInstance('vegetables.tomatoe.green').Warn('This is a warning message - green tomatoes are not ripe.');
        TVLogger.GetInstance('fruit.apple.red').Error('This is an error message');
        TVLogger.GetInstance('vegetables.tomatoe.red').Error('This is a multi-line error message from the red tomatoe'
          + #10 + #13 + 'First Line'
          + #10 + #13 + 'Second Line'
          + #10 + #13 + 'Third Line'
          + #10 + #13 + 'Fourth Line'
          + #10 + #13 + 'Fifth Line'
          + #10 + #13 + 'Sixth Line'
          + #10 + #13 + 'Seventh Line'
          + #10 + #13 + 'Eigth Line'
          + #10 + #13 + 'Ninth Line'
          + #10 + #13 + 'Tenth Line');

        try
        raise Exception.Create('Exception generated intentionally');
        except
        on E: Exception do
          TVLogger.GetInstance('fruit.apple.red').Fatal('Exception from auto generated messages.', E);
        end;
    end;
end;




//========================================================================================
//========================================================================================
//========================================================================================
//========================================================================================
//========================================================================================
//========================================================================================


constructor TVFilter.Create(Owner: TComponent);
begin
    inherited Create(Owner);
      fOnDecide := nil;
end;

destructor TVFilter.Destroy();
begin
    inherited;
end;


function TVFilter.Decide(event: TVLoggingEvent): TVFilterResult;
begin
  Result := frDeny;
  if (Assigned(fOnDecide)) then begin
    fOnDecide(event, result);
  end;
  inherited;
end;





//==============================================================================
//==============================================================================
//==============================================================================
//==============================================================================
//==============================================================================


class function TVLevel.ToString(value: TVLevelValue): String;
begin
    Result := ValueToString(value);
end;

class function TVLevel.ValueToString(value: TVLevelValue): String;
var
    str: String;
begin
  str := 'Unknown';
    if (value = lvOff) then begin
        str := 'Off';
    end
    else if (value >= lvFatal) then begin
        str := 'Fatal';
    end
    else if (value >= lvError) then begin
        str := 'Error';
    end
    else if (value >= lvWarn) then begin
        str := 'Warn';
    end
    else if (value >= lvInfo) then begin
        str := 'Info';
    end
    else if (value >= lvDebug) then begin
        str := 'Debug';
    end
    else if (value >= lvTrace) then begin
        str := 'Trace';
    end
    else begin
        str := 'All';
    end ;

    Result := str;
end;


class function TVLevel.ToSymbol(value: TVLevelValue): String;
var
  str: String;
begin
    str := '';
    if (value >= lvOff) then begin
        str := OFF_SYMBOL;
    end
    else if (value >= lvFatal) then begin
        str := FATAL_SYMBOL;
    end
    else if (value >= lvError) then begin
        str := ERROR_SYMBOL;
    end
    else if (value >= lvWarn) then begin
        str := WARN_SYMBOL;
    end
    else if (value >= lvInfo) then begin
        str := INFO_SYMBOL;
    end
    else if (value >= lvDebug) then begin
        str := DEBUG_SYMBOL;
    end
    else if (value > lvInherit) then begin
        str := TRACE_SYMBOL;
    end
    else begin
        str := ALL_SYMBOL;
    end;
    Result := str;

end;


class function TVLevel.GetLevel(value: String): TVLevelValue;
begin

    if (value = OFF_SYMBOL) then begin
        Result := lvOff;
    end
    else if (value = FATAL_SYMBOL) then begin
        Result := lvFatal;
    end
    else if (value = ERROR_SYMBOL) then begin
        Result := lvError;
    end
    else if (value = WARN_SYMBOL) then begin
        Result := lvWarn;
    end
    else if (value = INFO_SYMBOL) then begin
        Result := lvInfo;
    end
    else if (value = DEBUG_SYMBOL) then begin
        Result := lvDebug;
    end
    else if (value = TRACE_SYMBOL) then begin
        Result := lvTrace;
    end
    else  begin
        Result := lvInherit;
    end
end;



//==============================================================================
//==============================================================================
//==============================================================================
//==============================================================================
//==============================================================================

constructor TVLoggingEvent.Create();
begin
    fTime := Now;
    fMessage := '';
    fLevel := lvInfo;
    fLogger := '';
    fIndex := -1;
end;

constructor TVLoggingEvent.Create(index: integer; time: TDateTime; logger: String; level: TVLevelValue; msg: String; ndc: String);
begin
    fTime := time;
    fIndex := index;
    fLogger := logger;
    fLevel := level;
    fMessage := msg;
    fNdc := ndc;
end;

constructor TVLoggingEvent.Create(other : TVLoggingEvent);
begin
    fMessage := '';
      if (other <> nil) then
      begin
          fMessage := other.fMessage;
          fTime := other.fTime;
          fLevel := other.fLevel;
          fIndex := other.fIndex;
          fLogger := other.fLogger;
          fException := other.fException;
          fNdc := other.fNdc;
      end;
end;


destructor TVLoggingEvent.Destroy;
begin
  inherited;
end;

function TVLoggingEvent.GetIndex(): integer;
begin
    Result := fIndex;
end;

procedure TVLoggingEvent.SetIndex(index: integer);
begin
    fIndex := index;
end;



function TVLoggingEvent.GetException(): String;
begin
  result := fException;
end;

procedure TVLoggingEvent.SetException(exception: String);
begin
  fException := exception;
end;


function TVLoggingEvent.GetLevel(): TVLevelValue;
begin
    Result := fLevel;
end;

procedure TVLoggingEvent.SetLevel(level: TVLevelValue);
begin
    fLevel := level;
end;

function TVLoggingEvent.GetTime() : TDateTime;
begin
    Result := fTime;
end;

procedure TVLoggingEvent.SetTime(time: TDateTime);
begin
    fTime := time;
end;

function TVLoggingEvent.GetLogger(): String;
begin
    Result := fLogger;
end;

procedure TVLoggingEvent.SetLogger(logger: String);
begin
    fLogger := logger;
end;

function TVLoggingEvent.GetNdc(): String;
begin
    Result := fNdc;
end;

procedure TVLoggingEvent.SetNdc(ndc: String);
begin
    fNdc := ndc;
end;



function TVLoggingEvent.GetMessage(): String;
begin
    Result := fMessage;
end;

procedure TVLoggingEvent.SetMessage(value: String);
begin
    fMessage := value;
end;

function TVLoggingEvent.Assign(other: TVLoggingEvent): TVLoggingEvent;
begin
    if (self <> other) then
    begin
        fMessage := other.fMessage;
        fTime := other.fTime;
        fLevel := other.fLevel;
        fIndex := other.fIndex;
        fException := other.fException;
        fLogger := other.fLogger;
        fNdc := other.fNdc;
    end;

    Result := self;
end;

procedure TVLoggingEvent.Copy(event: TVLoggingEvent);
begin
    fMessage := event.fMessage;
    fLogger := event.fLogger;
    fNdc := event.fNdc;
    fLevel := event.fLevel;
end;

//==============================================================================
//==============================================================================
//==============================================================================
//==============================================================================
//==============================================================================
//==============================================================================


constructor TBasicLayout.Create;
begin
end;



function TBasicLayout.Format(logEvent: TVLoggingEvent): String;
begin
    Result := DateTimeToStr(logEvent.GetTime()) + TAB_CHAR + logEvent.GetLogger() + TAB_CHAR +
            TVLevel.ToSymbol(logEvent.GetLevel()) + TAB_CHAR +
            logEvent.GetMessage() + TAB_CHAR + logEvent.GetNdc() + TAB_CHAR + EOL_CHAR
            + CR_CHAR;

end;


//==============================================================================
//==============================================================================
//==============================================================================
//==============================================================================
//==============================================================================

constructor TVLog.Create(Owner: TComponent);
begin
    inherited Create(Owner);
    TVLogManager.GetInstance();
    fName := UNNAMED_LOGGER;
//    SetLevel(lvInfo);
end;

destructor TVLog.Destroy();
begin
  inherited;
end;


function TVLog.GetCategory(): String;
begin
    result := fName;
end;

procedure TVLog.SetCategory(name: String);
begin
    fName := name;
end;

procedure TVLog.RemoveAppenderFromAll(appender: TVCustomAppender);
begin
    TVLogger.RemoveAppenderFromAll(appender);
end;


procedure TVLog.SetLevel(level: TVLevelValue);
begin
    GetBaseLogger().SetLevel(level);
end;

function TVLog.GetBaseLogger(): TVLogger;
begin
    result := TVLogger.GetInstance(fName);
end;

function TVLog.GetLevel(): TVLevelValue;
begin
    result := GetBaseLogger.GetLevel();
end;

procedure TVLog.Log(priority: TVLevelValue; msg: String);
begin
    GetBaseLogger().Log(priority, msg);
end;


function TVLog.GetEffectiveLevel(): TVLevelValue;
begin
    Result := GetBaseLogger().GetEffectiveLevel();
end;


function TVLog.IsEnabledFor(level: TVLevelValue): boolean;
begin
    result := GetBaseLogger().IsEnabledFor(level);
end;


procedure TVLog.AddAppender(appender: TVAppender);
begin
    GetBaseLogger().AddAppender(appender);
end;


function TVLog.GetAppender(name: String): TVAppender;
begin
    result := GetBaseLogger().GetAppender(name);
end;


procedure TVLog.RemoveAllAppenders();
begin
    GetBaseLogger().RemoveAllAppenders();
end;


procedure TVLog.RemoveAppender(appender: TVAppender);
begin
    GetBaseLogger().RemoveAppender(appender);
end;

procedure TVLog.SetAdditivity(additivity: boolean);
begin
    GetBaseLogger().SetAdditivity(additivity);
end;


function TVLog.GetAdditivity(): boolean;
begin
    result := GetBaseLogger().GetAdditivity();
end;


function TVLog.GetRootLogger(owner: TComponent): TVLog;
var
  root: TVLog;
begin
    root := TVLog.Create(owner);
    root.SetCategory('');
    result := root;
end;


function TVLog.GetInstance(owner: TComponent; name: String): TVLog;
begin
    result := TVLog.Create(owner);
    result.SetCategory(name);
end;


function TVLog.GetParent(owner: TComponent): TVLog;
var
  name: String;
  parent: TVLogger;
begin
    Result := nil;
    name := '';
    parent := GetBaseLogger().GetParent();
    if (parent <> nil) then begin
        Result := TVLog.Create(owner);
        Result.SetCategory(parent.GetCategory());
    end;
end;



procedure TVLog.Trace(msg: String; ex: Exception);
begin
    if (GetBaseLogger().IsEnabledFor(lvTrace)) then begin

        TVLogManager.GetInstance().InitializeAppenders();
        GetBaseLogger().Trace(msg, ex);
    end;
end;


procedure TVLog.Trace(msg: String);
begin
    if (GetBaseLogger().IsEnabledFor(lvTrace)) then begin

        TVLogManager.GetInstance().InitializeAppenders();
        GetBaseLogger().Trace(msg);
    end;
end;


function TVLog.IsTraceEnabled(): boolean;
begin
    result := GetBaseLogger().IsTraceEnabled();
end;


procedure TVLog.Debug(msg: String; ex: Exception);
begin
    if (GetBaseLogger().IsEnabledFor(lvDebug)) then begin
        TVLogManager.GetInstance().InitializeAppenders();
        GetBaseLogger().Debug(msg, ex);
    end;
end;

procedure TVLog.Debug(msg: String);
begin
    if (GetBaseLogger().IsEnabledFor(lvDebug)) then begin

        TVLogManager.GetInstance().InitializeAppenders();
        GetBaseLogger().Debug(msg);
    end;
end;


function TVLog.IsDebugEnabled(): boolean;
begin
    result := GetBaseLogger().IsDebugEnabled();
end;


procedure TVLog.Info(msg: String; ex: Exception);
begin
    if (GetBaseLogger().IsEnabledFor(lvInfo)) then begin
        TVLogManager.GetInstance().InitializeAppenders();
        GetBaseLogger().Info(msg, ex);
    end;
end;

procedure TVLog.Info(msg: String);
begin
    if (GetBaseLogger().IsEnabledFor(lvInfo)) then begin

        TVLogManager.GetInstance().InitializeAppenders();
        GetBaseLogger().Info(msg);
    end;
end;


function TVLog.IsInfoEnabled(): boolean;
begin
    result := GetBaseLogger().IsInfoEnabled();
end;

procedure TVLog.Warn(msg: String; ex: Exception);
begin
    if (GetBaseLogger().IsEnabledFor(lvWarn)) then begin
        TVLogManager.GetInstance().InitializeAppenders();
        GetBaseLogger().Warn(msg, ex);
    end;
end;

procedure TVLog.Warn(msg: String);
begin
    if (GetBaseLogger().IsEnabledFor(lvWarn)) then begin

        TVLogManager.GetInstance().InitializeAppenders();
        GetBaseLogger().Warn(msg);
    end;
end;

function TVLog.IsWarnEnabled(): boolean;
begin
    result := GetBaseLogger().IsWarnEnabled();
end;


procedure TVLog.Error(msg: String; ex: Exception);
begin
    if (GetBaseLogger().IsEnabledFor(lvError)) then begin
        TVLogManager.GetInstance().InitializeAppenders();
        GetBaseLogger().Error(msg, ex);
    end;
end;


procedure TVLog.Error(msg: String);
begin
    if (GetBaseLogger().IsEnabledFor(lvError)) then begin
        TVLogManager.GetInstance().InitializeAppenders();
        GetBaseLogger().Error(msg);
    end;
end;

function TVLog.IsErrorEnabled(): boolean;
begin
    result := GetBaseLogger().IsErrorEnabled();
end;


procedure TVLog.Fatal(msg: String);
begin
    if (GetBaseLogger().IsEnabledFor(lvFatal)) then begin
        TVLogManager.GetInstance().InitializeAppenders();
        GetBaseLogger().Fatal(msg);
    end;
end;

procedure TVLog.Fatal(msg: String; ex: Exception);
begin
    if (GetBaseLogger().IsEnabledFor(lvFatal)) then begin
        TVLogManager.GetInstance().InitializeAppenders();
        GetBaseLogger().Fatal(msg, ex);
    end;
end;


function TVLog.IsFatalEnabled(): boolean;
begin
    result := GetBaseLogger().IsFatalEnabled();
end;



//==============================================================================
//==============================================================================
//==============================================================================

constructor TVLogger.CreateBase(owner: TComponent; name: AnsiString; parent: TVLogger; level: TVLevelValue);
begin
  inherited Create(owner);
  fName := name;
  fParent := parent;
  fLevel := level;
  fAdditive := true;
  fAppenderList := nil;
  fSettingLevel := false;
end;


destructor TVLogger.Destroy();
begin
    //TVLogManager.GetInstance().RemoveLogger(this);
    RemoveAllAppenders();
    inherited;
end;


constructor TVLogger.Create(owner: TComponent);
begin
    raise TVException.Create('TVLogger cannot be created with Create(owner) - use GetInstance()');
end;


function TVLogger.GetLoggerChildren(): TList;
var
  children: TList;
  logger: TVLogger;
  allLoggers: TStringList;
  i: INteger;
begin
  children := TList.Create();
  allLoggers := TVLogManager.GetInstance().GetCurrentLoggers();
  for i := 0 to allLoggers.Count - 1 do begin
    logger := TVLogger(allLoggers.Objects[i]);
    if (logger.GetParent() = self) then begin
      children.Add(logger);
    end;
  end;


  result := children;
end;

procedure TVLogger.ForceChildrenToInherit(recurse: boolean);
var
  children: TList;
  child: TVLogger;
  i: INteger;
begin
  children := GetLoggerChildren();

  for i := 0 to children.Count - 1 do begin
    child := TVLogger(children.Items[i]);
    child.SetLevel(lvInherit);
    if (recurse) then begin
      child.ForceChildrenToInherit(recurse);
    end;
  end;
  children.Destroy;
end;


class function TVLogger.GetRootLogger(): TVLogger;
begin
    result := GetInstance('');
end;


class function TVLogger.GetInstance(name: String): TVLogger;
begin
    result := TVLogManager.GetInstance().GetCreateLogger(name);
end;


function TVLogger.Exists(name: String): TVLogger;
begin
    result := TVLogManager.GetInstance().GetLogger(name);
end;


class function TVLogger.GetCurrentLoggers(): TStringList;
begin
    result := TVLogManager.GetInstance().GetCurrentLoggers();
end;


function TVLogger.GetCategory(): String;
begin
    result := fName;
end;


procedure TVLogger.SetCategory(name: String);
begin
    fName := name;
end;

function TVLogger.GetLevel(): TVLevelValue;
begin
    result := fLevel;
end;

procedure TVLogger.SetLevel(level: TVLevelValue);
begin
    if (fSettingLevel = false) then begin
      fSettingLevel := true;
//      TVLogManager.GetInstance().LoadLevel(fName);
      fLevel := level;
      TVLogManager.GetInstance().SaveLevel(self);
      fSettingLevel := false;
    end;
end;

function TVLogger.GetEffectiveLevel(): TVLevelValue;
var
  loggerWithLevel: TVLogger;
  c: TVLogger;
begin
    // REQUIRE(rootLogger.getPriority() != Priority.NOTSET)

    loggerWithLevel := self;
    c := self;
    while (c <> nil) and (c.GetLevel() = lvInherit) do begin
        c := c.GetParent();
        if (c <> nil) then begin
            loggerWithLevel := c;
        end;
    end;

    result := loggerWithLevel.GetLevel();
end;


procedure TVLogger.AddAppender(appender: TVAppender);
var
   find: TVAppender;
begin
    if (appender <> nil) then begin
        find := GetAppender(appender.GetAppenderName());
        if (fAppenderList = nil) then begin
            fAppenderList := TStringList.Create();
        end;

        if (find = nil) then begin
            fAppenderList.AddObject(appender.GetAppenderName(), TObject(appender));
        end;
    end

end;


function TVLogger.GetAppender(name: String): TVAppender;
var
  index: Integer;
begin
    Result := nil;

    if (fAppenderList <> nil) then begin
        index := -1;
        if (fAppenderList.Find(name, index)) then begin
            result := TVAppender(fAppenderList.Objects[index]);
        end
    end

end;

procedure TVLogger.RemoveAllAppenders();
begin
    if (fAppenderList <> nil) then begin
        fAppenderList.Clear();
    end
end;

procedure TVLogger.RemoveAppender(appender: TVCustomAppender);
var
  Index: integer;
  appenderName: AnsiString;
begin
    appenderName := appender.GetAppenderName();
    if ((fAppenderList <> nil) and (fAppenderList.Find(appenderName, index))) then begin
        fAppenderList.Delete(index);
    end
end;

class procedure TVLogger.RemoveAppenderFromAll(appender: TVCustomAppender);
var
  currentLoggers: TStringList;
  i: integer;
  logger: TVLogger;
begin
    currentLoggers := GetCurrentLoggers();
    for i := 0 to currentLoggers.Count - 1 do begin
        logger := TVLogger(currentLoggers.Objects[i]);
        logger.RemoveAppender(appender);
    end
end;


procedure TVLogger.SendEventToAppenders(event: TVLoggingEvent);
var
  i: integer;
  appender: TVCustomAppender;
begin
    if (fAppenderList <> nil) then begin
        for i := 0 to fAppenderList.Count - 1 do begin
            appender := TVCustomAppender(fAppenderList.Objects[i]);
            appender.DoAppend(event);
        end;
    end;

    if ((GetAdditivity() = true) and (GetParent() <> nil)) then begin
        GetParent().SendEventToAppenders(event);
    end;
end;

procedure TVLogger.SetAdditivity(additivity: boolean);
begin
    fAdditive := additivity;
end;

function TVLogger.GetAdditivity(): boolean;
begin
    Result := fAdditive;
end;

function TVLogger.GetParent(): TVLogger;
begin
    result := fParent;
end;


procedure TVLogger.LogIt(level: TVLevelValue; msg: String);
begin
  LogIt(level, msg, nil);
end;


procedure TVLogger.LogIt(level: TVLevelValue; msg: String; ex: Exception);
var
  event: TVLoggingEvent;
begin

    if (TVLogManager.IsShuttingDown() = false) then begin
        if (TVLogManager.GetInstance().GetNeedsAppenderInitialize()) then begin
            TVLogManager.GetInstance().InitializeAppenders();
        end;

        event := TVLoggingEvent.Create();
        event.SetLogger(GetCategory());
        event.SetMessage(msg);

        if (ex <> nil) then begin
          event.SetException(ex.Message);
        end;

        event.SetNdc(TVLogManager.GetInstance().GetNdc());
        event.SetLevel(level);
        SendEventToAppenders(event);
        event.Destroy();
    end
end;


function TVLogger.IsEnabledFor(level: TVLevelValue): boolean;
begin
    result := (GetEffectiveLevel() <= level);
end;



procedure TVLogger.Log(level: TVLevelValue; msg: String);
begin
    if (IsEnabledFor(level)) then begin
        LogIt(level, msg);
    end;
end;

procedure TVLogger.Trace(msg: String; ex: Exception);
begin
    if (IsEnabledFor(lvTrace)) then begin
        LogIt(lvTrace, msg, ex);
    end
end;

procedure TVLogger.Trace(msg: String);
begin
    if (IsEnabledFor(lvTrace)) then begin
        LogIt(lvTrace, msg);
    end
end;

function TVLogger.IsTraceEnabled(): boolean;
begin
    Result := IsEnabledFor(lvTrace);
end;


procedure TVLogger.Debug(msg: String; ex: Exception);
begin
    if (IsEnabledFor(lvDebug)) then begin
        LogIt(lvDebug, msg, ex);
    end
end;

procedure TVLogger.Debug(msg: String);
begin
    if (IsEnabledFor(lvDebug)) then
        LogIt(lvDebug, msg);

end;

function TVLogger.IsDebugEnabled(): boolean;
begin
    result := IsEnabledFor(lvDebug);
end;


procedure TVLogger.Info(msg: String; ex: Exception);
begin
    if (IsEnabledFor(lvInfo)) then begin
        LogIt(lvInfo, msg, ex);
    end
end;

procedure TVLogger.Info(msg: String);
begin
    if (IsEnabledFor(lvInfo)) then
        LogIt(lvInfo, msg);
end;

function TVLogger.IsInfoEnabled(): boolean;
begin
    result := IsEnabledFor(lvInfo);
end;


procedure TVLogger.Warn(msg: String; ex: Exception);
begin
    if (IsEnabledFor(lvWarn)) then begin
        LogIt(lvWarn, msg, ex);
    end
end;

procedure TVLogger.Warn(msg: String);
begin
    if (IsEnabledFor(lvWarn)) then begin
        LogIt(lvWarn, msg);
    end;
end;

function TVLogger.IsWarnEnabled(): boolean;
begin
    Result := IsEnabledFor(lvWarn);
end;

procedure TVLogger.Error(msg: String);
begin
    if (IsEnabledFor(lvError)) then
        LogIt(lvError, msg);
end;

procedure TVLogger.Error(msg: String; ex: Exception);
begin
    if (IsEnabledFor(lvError)) then
        LogIt(lvError, msg, ex);
end;


function TVLogger.IsErrorEnabled(): boolean;
begin
    result := IsEnabledFor(lvError);
end;

procedure TVLogger.Fatal(msg: String);
begin
    if (IsEnabledFor(lvFatal)) then
        LogIt(lvFatal, msg);
end;

procedure TVLogger.Fatal(msg: String; ex: Exception);
begin
    if (IsEnabledFor(lvFatal)) then
        LogIt(lvFatal, msg, ex);
end;


function TVLogger.IsFatalEnabled(): boolean;
begin
    result := IsEnabledFor(lvFatal);
end;


//==============================================================================
//==============================================================================
//==============================================================================
//==============================================================================
//==============================================================================




constructor TVDisplayManager.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  FindResources();
end;


procedure TVDisplayManager.FindResources();
begin
  fSeverityIcons := TImageList.Create(self);
  fSeverityIcons.Masked := false;
  fSeverityIcons.Height := 16;
  fSeverityIcons.Width := 16;
  fSeverityIcons.Add(MakeBitmap(TraceBmpData), nil);
  fSeverityIcons.Add(MakeBitmap(DebugBmpData), nil);
  fSeverityIcons.Add(MakeBitmap(InfoBmpData), nil);
  fSeverityIcons.Add(MakeBitmap(WarnBmpData), nil);
  fSeverityIcons.Add(MakeBitmap(ErrorBmpData), nil);
  fSeverityIcons.Add(MakeBitmap(FatalBmpData), nil);

  fLevelInhIcons := TImageList.Create(self);
  fLevelInhIcons.Masked := false;
  fLevelInhIcons.Height := 16;
  fLevelInhIcons.Width := 16;
  fLevelInhIcons.Add(MakeBitmap(TraceBmpData), nil);
  fLevelInhIcons.Add(MakeBitmap(DebugBmpData), nil);
  fLevelInhIcons.Add(MakeBitmap(InfoBmpData), nil);
  fLevelInhIcons.Add(MakeBitmap(WarnBmpData), nil);
  fLevelInhIcons.Add(MakeBitmap(ErrorBmpData), nil);
  fLevelInhIcons.Add(MakeBitmap(FatalBmpData), nil);
  fLevelInhIcons.Add(MakeBitmap(TraceInhBmpData), nil);
  fLevelInhIcons.Add(MakeBitmap(DebugInhBmpData), nil);
  fLevelInhIcons.Add(MakeBitmap(InfoInhBmpData), nil);
  fLevelInhIcons.Add(MakeBitmap(WarnInhBmpData), nil);
  fLevelInhIcons.Add(MakeBitmap(ErrorInhBmpData), nil);
  fLevelInhIcons.Add(MakeBitmap(FatalInhBmpData), nil);
end;

function TVDisplayManager.GetPreviousButtonImage(owner: TComponent): TImage;
var
    image: TImage;
begin
  //Result := fResourceFrame.PreviousBtnImage;

    image := Timage.Create(owner);
    image.Picture.Bitmap := self.MakeBitmap(PrevBmpData);
    result := image;
end;

function TVDisplayManager.GetNextButtonImage(): TImage;
var
    image: TImage;
begin
    image := Timage.Create(owner);
    image.Picture.Bitmap := self.MakeBitmap(NextBmpData);
    result := image;
end;


function TVDisplayManager.GetSeverityIndex(severity: TVLevelValue): Integer;
var
  severityIndex: Integer;
begin
    if (severity <= lvTrace) then begin
        severityIndex := 0;
    end
    else if (severity <= lvDebug) then begin
        severityIndex := 1;
    end
    else if (severity <= lvInfo) then begin
        severityIndex := 2;
    end
    else if (severity <= lvWarn) then begin
        severityIndex := 3;
    end
    else if (severity <= lvError) then begin
        severityIndex := 4;
    end
    else  begin
        severityIndex := 5;
    end;
    result := severityIndex;
end;


function TVDisplayManager.MakeBitmap(dataConst: PAnsiChar): Graphics.TBitmap;
var
    bitmap: Graphics.TBitmap;
    memStream: TMemoryStream;
begin
    bitmap := Graphics.TBitmap.Create();
    memStream := TMemoryStream.Create();
    // The size of the binary data is half the length of the hex encoded
    //  string representation
    memStream.SetSize(Length(dataConst) div 2) ;
    HexToBin(PAnsiChar(dataConst), memStream.Memory, memStream.Size) ;

    bitmap.LoadFromStream(memStream);

    memStream.Free ;
    result := bitmap;
end;


{
function  TVDisplayManager.GetBitmap(resourceName: String): Graphics.TBitmap;
var
    bitmap: Graphics.TBitmap;//
begin
    bitmap := Graphics.TBitmap.Create();

    bitmap.LoadFromResourceName(HInstance, resourceName);
    result := bitmap;

end;
}

destructor TVDisplayManager.Destroy();
begin
    fSeverityIcons.Destroy();
    inherited;
end;


function TVDisplayManager.GetSeverityIcons(): TImageList;
begin
    result := fSeverityIcons;
end;

function TVDisplayManager.GetLevelInhIcons(): TImageList;
begin

  result := fLevelInhIcons;
end;



//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
constructor TVLogManager.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  LogManagerShuttingDown := false;
  fNextId := -1;
  fAppenderList := TStringList.Create();
  fAppenderList.Duplicates := dupError;
  fAppenderList.Sorted := true;

  fLoggerList := TStringList.Create();
  fLoggerList.Duplicates := dupError;
  fLoggerList.Sorted := true;

  fNeedsAppenderInitialize := true;
  fLoadedLevels := false;
  fDisplayManager := TVDisplayManager.Create(self);
  fFileComponentList := TStringList.Create();
  fFileComponentList.Sorted := true;
  fFileComponentList.Duplicates := dupError;
end;


function TVLogManager.FindFileComponent(fileName: String): TComponent;
var
    index: Integer;
    component: TComponent;
begin
    //result := false;
    index := fFileComponentList.IndexOf(LowerCase(fileName));

    if (index >= 0) then begin
        component := TComponent(fFileComponentList.Objects[index]);
        result := component;
    end
    else begin
        result := nil;
    end;
end;


procedure TVLogManager.RegisterFileComponent(fileComponent: TComponent; fileName: String);
begin
    UnRegisterFileComponent(fileComponent);
    if (FindFileComponent(fileName) = nil) then begin
        fFileComponentList.AddObject(LowerCase(fileName), fileComponent);
    end
    else if (FindFileComponent(fileName) = fileComponent) then begin
        // Component already exists and owns file, ignore request.
    end
    else begin
        raise TVFileException.Create('File is in use by other appender or persistence.  Failed to register component');
    end;
end;

procedure TVLogManager.UnregisterFileComponent(fileComponent: TComponent);
var
    i: integer;
    component: TComponent;
    done: boolean;
    foundIndex: Integer;
begin
    done := false;
    while (done = false) do begin
        foundIndex := -1;
        for i := 0 to fFileComponentList.Count - 1 do begin
            component := TComponent(fFileComponentList.Objects[i]);
            if (component = fileComponent) then begin
                foundIndex := i;
            end;

        end;
        if (foundIndex < 0) then begin
            done := true;
        end
        else begin
            fFileComponentList.Delete(foundIndex);
        end;
    end;
end;



destructor TVLogManager.Destroy();
begin
    FlushAllAppenders();
    LogManagerShuttingDown := true;
    fFileComponentList.Destroy();
    fAppenderList.Destroy();
    fLoggerList.Destroy();
    inherited;
end;


procedure TVLogManager.FlushAllAppenders();
var
    i: integer;
    appender: TVCustomAppender;
begin
    for i := 0 to fAppenderList.Count - 1 do begin
        appender := TVCustomAppender(fAppenderList.Objects[i]);
        if (appender.Asynchronous) then begin
            appender.Flush();
        end;
    end;

end;


class function TVLogManager.IsShuttingDown(): boolean;
begin
  result := LogManagerShuttingDown;
end;


procedure TVLogManager.SetNeedsAppenderInitialize(value: boolean);
begin
  fNeedsAppenderInitialize := value;
end;


function TVLogManager.GetNeedsAppenderInitialize(): boolean;
begin
  result := fNeedsAppenderInitialize;
end;


class function TVLogManager.GetInstance(): TVLogManager;
begin
    if (fInstance = nil) then
    begin
        // Making the Application the owner should result in the destructor being called when the
        // Application is destroyed.
        fInstance := TVLogManager.Create(Application);
    end;
    Result := fInstance;
end;


function TVLogManager.GetCurrentLoggers(): TStringList;
begin
    Result := fLoggerList;
end;

function TVLogManager.FindAppender(name: AnsiString): TVCustomAppender;
var
  index: integer;
begin
    Result := nil;
    index := -1;
    if (fAppenderList.Find(name, index)) then begin
        Result := TVCustomAppender(fAppenderList.Objects[index]);
    end
end;


procedure TVLogManager.AddAppender(appender: TVCustomAppender);
begin
    GetAppenderList().AddObject(appender.GetAppenderName(), appender);

    fNeedsAppenderInitialize := true;
end;


procedure TVLogManager.RemoveAppender(appender: TVCustomAppender);
var
  index: integer;
begin
    index := -1;
    if (fAppenderList.Find(appender.GetAppenderName(), index)) then begin
        fAppenderList.Delete(index);
    end;

    if (LogManagerShuttingDown = false) then begin
      TVLogger.RemoveAppenderFromAll(appender);
    end;

end;

function TVLogManager.GetNdc(): AnsiString;
begin
    Result := fNdc;
end;

procedure TVLogManager.SetNdc(ndc: AnsiString);
begin
    fNdc := ndc;
end;


function TVLogManager.GetCategoryKeyName(): String;
var
  appName: String;
  flags: TReplaceFlags;
begin
  flags := [rfReplaceAll];
  appName := Application.ExeName;
  appName := StringReplace(appName, '\', '_', flags);

  result := REGISTRY_KEY_PREFIX + '\' +  appName + '\categories';
end;



procedure TVLogManager.LoadLevels();
begin
  LoadLevel(nil);
end;


procedure TVLogManager.LoadLevel(logger: TVLogger);
var
  i: integer;
  _logger: TVLogger;
  registry: TRegistry;
  keyName: AnsiString;
  valueNames: TStringList;
  valueIndex: integer;
  catName: AnsiString;
  levelInt: Integer;
begin
//    if not (csDesigning in ComponentState) then begin

      fLoadedLevels := true;
      if (logger = nil) then begin
        for i := 0 to fLoggerList.Count -1 do begin
            _logger := TVLogger(fLoggerList.Objects[i]);
            if (_logger.GetCategory() <> Logger.GetRootLogger().GetCategory()) then begin
                _logger.SetLevel(lvInherit);
            end
            else begin
              // If it is the root, default to lvInfo
              //_logger.SetLevel(lvInfo);
            end
        end;
      end;


      registry := TRegistry.Create();
      if (registry <> nil) then begin
          registry.RootKey := HKEY_LOCAL_MACHINE;
          registry.Access := KEY_READ;
          keyName := GetCategoryKeyName();

          //if (tRegistry.KeyExists(iKey)) begin
          //    result := OpenKey(iKey, true);

          if (registry.OpenKey(keyName, true) = true) then begin
               valueNames := TStringList.create();
               registry.GetValueNames(valueNames);
               for valueIndex := 0 to valueNames.Count - 1 do begin
                  catName := valueNames.Strings[valueIndex];
                  _logger := TVLogger.GetInstance(catName);
                  if (logger = nil) or (logger.GetCategory = _logger.GetCategory()) then begin
                    try
                       levelInt := registry.ReadInteger(catName);

                       _logger.SetLevel(TVLevelValue(levelInt));
                    except
                        //MessageDlg('TDbLog.GetNextId() - Exception', mtError,
                        //           TMsgDlgButtons() << mbOK, 0);
                    end
                  end;

              end;
              valueNames.Destroy();
          end
          else begin
              //TVLogger.GetInstance('com.vectrics.TVLogManager').Error('SavePriorities() - failed to create registry key for Logger priorities.');
          end;
          registry.CloseKey();
      end;
      registry.Destroy();
//    end;
end;


function TVLogManager.GetDisplayManager(): TVDisplayManager;
begin
    result := fDisplayManager;
end;


procedure TVLogManager.SaveLevels();
begin
  SaveLevel(nil);
end;


procedure TVLogManager.SaveLevel(logger: TVLogger);
var
  registry: TRegistry;
  keyName: AnsiString;
  logIndex: Integer;
  level: TVLevelValue;
  cat: TVLogger;
begin
      registry := TRegistry.Create();
      if (registry <> nil) then begin
          registry.RootKey := HKEY_LOCAL_MACHINE;
          registry.Access := KEY_READ or KEY_WRITE;
          keyName := GetCategoryKeyName();

          if (logger = nil) then begin
            registry.DeleteKey(keyName);
          end;

          if (registry.OpenKey(keyName, true) = true) then begin
              if (logger = nil) then begin
                for logIndex := 0 to fLoggerList.Count - 1 do begin
                    cat := TVLogger(fLoggerList.Objects[logIndex]);
                    try
                        level := TVLevelValue(cat.GetLevel());
                        if (level > lvInherit) then begin
                            registry.WriteInteger(cat.GetCategory(), Integer(level));
                        end

                    except
                        on ex: Exception do begin
                            TVLogger.GetInstance('com.vectrics.logging.TVLogManager').Error('Exception saving log levels to registry.', ex);
                        end;
                        //MessageDlg('TDbLog.GetNextId() - Exception', mtError,
                        //           TMsgDlgButtons() << mbOK, 0);
                    end

                end
              end
              else begin
                  registry.DeleteValue(logger.GetCategory());
                  if (logger.GetCategory <> UNNAMED_LOGGER) then begin
                    level := TVLevelValue(logger.GetLevel());
                    if (level > lvInherit) then begin
                        registry.WriteInteger(logger.GetCategory(), Integer(level));
                    end
                  end;
              end;
          end
          else begin
          end;


          registry.CloseKey();
      end;
      registry.Destroy();
end;



function TVLogManager.GetNextId(): Integer;
var
  registry: TRegistry;
begin
    if (fNextId <= -1) then begin
        fNextId := 10000;
        try
            registry := TRegistry.Create();
            if (registry <> nil) then begin
                registry.RootKey := HKEY_LOCAL_MACHINE;
                registry.Access := KEY_READ or KEY_WRITE;

                try
                    if (registry.OpenKey(REGISTRY_KEY_PREFIX, true) = true) then begin
                        fNextId := registry.ReadInteger('next_log_id');
                    end
                    else begin
                    end

                except
                    //MessageDlg('TDbLog.GetNextId() - Exception', mtError,
                    //           TMsgDlgButtons() << mbOK, 0);
                end;

                registry.WriteInteger('next_log_id', fNextId);
                registry.CloseKey();

                registry.Destroy();
            end
        except

        end
    end
    else begin
    end;
    result := fNextId + 1;
end;



function TVLogManager.NextId(): Integer;
begin
    fNextId := GetNextId();
    SetNextId(fNextId);
    //fNextId := fNextId + 1;
    result := fNextId;
end;



procedure TVLogManager.SetNextId(nextId: Integer);
var
  registry: TRegistry;
begin
  fNextId := nextId;
  registry := TRegistry.Create();
  if (registry <> nil) then  begin
      registry.RootKey := HKEY_LOCAL_MACHINE;
      registry.Access := KEY_READ or KEY_WRITE;

      try
          if (registry.OpenKey(REGISTRY_KEY_PREFIX, true) = true) then begin
              registry.WriteInteger('next_log_id', fNextId + 1);
          end


      except
          //MessageDlg('TDbLog.GetNextId() - Exception', mtError,
          //           TMsgDlgButtons() << mbOK, 0);
      end;
      registry.CloseKey();

      registry.Destroy;
  end;

end;



procedure TVLogManager.InitializeAppenders();
var
  appenderIndex: Integer;
  appender: TVAppender;
begin
    if (LogManagerShuttingDown = false) then begin
        if (fNeedsAppenderInitialize = true) then begin
            for appenderIndex := 0 to fAppenderList.Count - 1 do begin
                appender := TVAppender(fAppenderList.Objects[appenderIndex]);
                if (appender.GetInitialized() = false) then begin
                    appender.Initialize();
                end
            end;
            fNeedsAppenderInitialize := false;
        end
    end
end;


function TVLogManager.GetAppenderList(): TStringList;
begin
    result := fAppenderList;
end;


function TVLogManager.GetLogger(name: AnsiString): TVLogger;
var
  i: integer;
  logger: TVLogger;
begin
  result := nil;
    for i := 0 to fLoggerList.Count  - 1 do begin
        logger := TVLogger(fLoggerList.Objects[i]);
        if (logger.GetCategory() = name) then begin
            result := logger;
        end
    end
end;



function TVLogManager.GetCreateLogger(loggerName: AnsiString): TVLogger;
  var
     name: AnsiString;
     parentName: AnsiString;
     dotIndex: Integer;
     parent: TVLogger;
     i: Integer;
  begin
    if (fLoadedLevels = false) then  begin
      LoadLevels();
    end;

    result := GetLogger(loggerName);

    if (result = nil) then begin
        if (loggerName = '') then begin
            // Create root logger
          result := TVLogger.CreateBase(Application, loggerName, nil, lvInfo);
        end
        else begin
            name := loggerName;

            dotIndex := 0;
            i := Length(name);
            while ((i > 0) and (dotIndex = 0)) do begin
              if (name[i] = '.') then begin
                dotIndex := i;
              end;

              Dec(i);
            end;


            if ((dotIndex <= 0) or (Length(name) <= dotIndex)) then begin
                parentName := '';
            end
            else begin
                parentName := Copy(name, 1 , dotIndex - 1);
            end;

            parent := GetCreateLogger(parentName);
            result := TVLogger.CreateBase(self, name, parent, lvInherit);
        end;
        fLoggerList.AddObject(loggerName, result);
  end
end;


//==============================================================================
//==============================================================================
//==============================================================================
//==============================================================================
//==============================================================================

constructor TVLogPersistence.Create(Owner: TComponent);
begin
    inherited Create(Owner);
    fFirstAppend := true;
    fLevel := lvInherit;
    fMaxSize := 2000;  // 2 MB
    fManagedAppender := TVAppender.Create(self);
    fManagedAppender.Asynchronous := true;
    fManagedAppender.AttachToRoot := true;
    fManagedAppender.AfterInitialize := OnAppenderInitialized;
    fManagedAppender.OnAppend := OnAppend;
end;

destructor TVLogPersistence.Destroy();
begin
//    fManagedAppender.Destroy();
    inherited;
end;


procedure TVLogPersistence.Flush();
begin
    Appender.Flush();
end;

procedure TVLogPersistence.SetAsynchronous(value: boolean);
begin
    Appender.Asynchronous := value;
end;

function TVLogPersistence.GetAsynchronous(): boolean;
begin
    result := Appender.Asynchronous;
end;


// Derived classes can override this function to provide behavior when TVLogManager
// Initializes the Managed Appender.
procedure TVLogPersistence.OnAppenderInitialized(Sender: TObject);
begin
end;

function TVLogPersistence.GetReadLevel(): TVLevelValue;
begin
    result := fLevel;
end;

function TVLogPersistence.MakeEventKey(event: TVLoggingEvent): String;

begin
  result := IntToStr(event.GetIndex());
  while (Length(result) < 12) do begin
    result := '0' + result;
  end;
end;

procedure TVLogPersistence.SetReadLevel(value: TVLevelValue);
begin
  if (fLevel <> value) then begin
    fLevel := value;
  end;
end;


function TVLogPersistence.GetWriteLevel(): TVLevelValue;
begin
  result := GetAppender().GetLevel();
end;

procedure TVLogPersistence.SetWriteLevel(level: TVLevelValue);
begin
  GetAppender.SetLevel(level);
end;

function TVLogPersistence.GetAppender(): TVAppender;
begin
    result := fManagedAppender;
end;


procedure TVLogPersistence.OnAppend(event: TVLoggingEvent);
var
  lastIndex : Integer;

begin
    if (fFirstAppend) then begin
      lastIndex := FindLastIndex();
      if (TVLogManager.GetInstance().GetNextId() < lastIndex) then begin
        TVLogManager.GetInstance().SetNextId(lastIndex + 1);
      end;
      fFirstAppend := false;
    end;

    if (event.GetIndex() <= 0) then begin
        event.SetIndex(TVLogManager.GetInstance().NextId());
    end;
    AppendEvent(event);
end;


function TVLogPersistence.FindNextPageIndex(_fromIndex: Integer; pageSize: Integer; filter: TVFilter): Integer;
var
  fromIndex: Integer;
  count: Integer;
  list: TVPageList;
  done: boolean;
  i, j: Integer;
  event: TVLoggingEvent;
  listSize: Integer;
  deleteEvent: TVLoggingEvent;
  lastIndex: Integer;
begin
    lastIndex := FindLastIndex();
    fromIndex := _fromIndex;
    if (fromIndex < 0) then begin
      result := TVLogManager.GetInstance().GetNextId() - 1;
    end
    else if (fromIndex >= lastIndex) then begin
      result := lastIndex;
    end
    else begin
      done := false;
      count := 0;
      result := fromIndex;
      while ((count < pageSize) and (done = false)) do begin
          list := TVPageList.Create();

          FindEventPage(fromIndex + pageSize, pageSize, list);
          for  i := list.Count - 1 downto 0 do begin
              event := TVLoggingEvent(list.Objects[list.Count - (i + 1)]);
              if (event.GetIndex() >= fromIndex) then begin
                  if ((filter = nil) or (filter.Decide(event) <> frDeny)) then begin
                      Inc(count);
                  end;
                  if (count < pageSize) then begin

                      result := event.GetIndex() + 1;
                      fromIndex := result;
                  end;

              end;
          end;

          listSize := list.Count;
          if (listSize < pageSize) then begin
              //Could not find enough members to fill list, we're at end of the storage.
              done := true;
          end;

          fromIndex := fromIndex + pageSize;
          for j := 0 to list.Count - 1 do begin
              try
                deleteEvent := TVLoggingEvent(list.Objects[j]);
                if (deleteEvent <> nil) then begin
                  deleteEvent.Destroy();
                end
                else begin
                  raise Exception.Create('FindNextPageIndex() - page list contained null value at index = ' + IntToStr(j));
                end;
              except
                raise Exception.Create('FindNextPageIndex() - exception deleting list item at index = ' + IntToStr(j));
              end;
          end;
          list.Destroy();
          if (fromIndex >= lastIndex) then begin
            result := lastIndex - 1;
            done := true;
          end;

      end;
    end;
end;




function TVLogPersistence.FindPreviousPageIndex(_fromIndex: Integer; pageSize: Integer; filter: TVFilter): Integer;
var
  fromIndex: Integer;
  list: TVPageList;
  event: TVLoggingEvent;
begin
    fromIndex := _fromIndex;
    result := fromIndex;
    list := TVPageList.Create();
    FindEventPage(fromIndex, pageSize, list);
    if (list.Count > 0)  then begin
        event := TVLoggingEvent(list.Objects[0]);
        result := event.GetIndex();
    end;
    list.DeleteMembers;
    list.Destroy
end;




//==============================================================================
//==============================================================================
//==============================================================================
//==============================================================================
//==============================================================================


constructor TVMemoryLogPersistence.Create(Owner: TComponent);
begin
    inherited Create(Owner);
    fLoggingEventList := TList.Create();
    fMaxCount := 500;
    fListMutex := TCriticalSection.Create();
end;

procedure TVMemoryLogPersistence.Clear();
var
  i: Integer;
begin
    fListMutex.Acquire();
    for  i := 0 to fLoggingEventList.Count  - 1 do begin
        TVLoggingEvent(fLoggingEventList.Items[i]).Destroy();
    end;
    fLoggingEventList.Clear();

    fListMutex.Release();
end;


destructor TVMemoryLogPersistence.Destroy();
begin
    fLoggingEventList.Destroy();

    fListMutex.Acquire();
    fListMutex.Destroy();
    inherited;
end;


function TVMemoryLogPersistence.FindLastIndex(): Integer;
var
  lastIndex: Integer;
  loggingEvent: TVLoggingEvent;
begin
    lastIndex := -1;
    if (fLoggingEventList.Count > 0) then begin
        loggingEvent := TVLoggingEvent(fLoggingEventList.Items[fLoggingEventList.Count - 1]);
        lastIndex := loggingEvent.GetIndex();
    end;
    result := lastIndex;
end;



function TVMemoryLogPersistence.FindFirstIndex(): Integer;
var
  firstIndex: Integer;
  loggingEvent: TVLoggingEvent;
begin
    firstIndex := -1;
    if (fLoggingEventList.Count > 0) then begin
        loggingEvent := TVLoggingEvent(fLoggingEventList.Items[0]);
        firstIndex := loggingEvent.GetIndex();
    end;
    result := firstIndex;
end;


function TVMemoryLogPersistence.FindIndexAtPosition(position: Integer): Integer;
var
  size: Integer;
  event: TVLoggingEvent;
  i: Integer;
begin
    result := 0;
    size := 0;
    i := 0;
    while ((i < fLoggingEventList.Count) and (Result = 0)) do begin
        event := TVLoggingEvent(fLoggingEventList.Items[i]);
        if ((size > position) or (i >= fLoggingEventList.Count - 1)) then begin
          result := event.GetIndex();
        end;
        size := size + EST_SIZE_PER_RECORD;
        Inc(i);
    end;
end;



procedure TVMemoryLogPersistence.AppendEvent(logEvent: TVLoggingEvent);
var
  event: TVLoggingEvent;
  loggingEvent: TVLoggingEvent;
  done: boolean;
begin

    try
        fListMutex.Acquire();
        event := TVLoggingEvent.Create(logEvent);
        fLoggingEventList.Add(event);
        done := false;
        while (done = false) and (GetSize() > MaxSize * 1000) do begin
            if (fLoggingEventList.Count > 0) then begin
              loggingEvent := TVLoggingEvent(fLoggingEventList.Items[0]);
              fLoggingEventList.Delete(0);
              loggingEvent.Destroy();
            end
            else begin
              done := true;
            end;
        end;
    except
        raise Exception.Create('Exception occurred in TVMemoryLogPersistence.onAppend()');
    end;
    fListMutex.Release();
end;



function TVMemoryLogPersistence.GetAll(): TList;
begin
    result := fLoggingEventList;
end;



function TVMemoryLogPersistence.FindPositionOfIndex(index: Integer): Integer;
var
  size: Integer;
  event: TVLoggingEvent;
  i: Integer;
  done: boolean;
begin
    done := false;
    size := 0;
    i := 0;
    while (i < fLoggingEventList.Count) and (done = false) do begin
        event := TVLoggingEvent(fLoggingEventList.Items[i]);
        if (event.GetIndex() > index) then begin
          done := true;
        end
        else begin
          size := size + EST_SIZE_PER_RECORD;
        end;
        Inc(i);
    end;
    result := size;
end;


function TVMemoryLogPersistence.GetSize(): Integer;
var
  size: Integer;
begin
    size := fLoggingEventList.Count * EST_SIZE_PER_RECORD;
    result := size;
end;



function TVMemoryLogPersistence.FindEventRange(_startId: Integer; _endId: Integer; list: TVPageList; filter: TVFilter): Integer;
var
  startId: Integer;
  endId: Integer;
  numberAdded: Integer;
  i: Integer;
  found: TVLoggingEvent;
  event: TVLoggingEvent;
  newEvent: TVLoggingEvent;
begin
    startId := _startId;
    endId := _endId;
    numberAdded := 0;
    try

        fListMutex.Acquire();
        found := nil;
        i := 0;
        while ((i < fLoggingEventList.Count) and (found = nil)) do begin
            event := TVLoggingEvent(fLoggingEventList.Items[i]);
            if (event.GetIndex() >= startId) then begin
                if (event.GetIndex() <= endId) then begin
                    if (Integer(event.GetLevel()) >= Integer(GetReadLevel())) and ((filter = nil)
                            or (filter.Decide(event) <> frDENY)) then begin
                        newEvent := event;
                        list.AddObject(MakeEventKey(newEvent), newEvent);
                        Inc(numberAdded);
                    end;
                end;
            end;
            Inc(i);
        end;
    except
        raise Exception.Create('Exception in TMememoryLogPersistence.findEventRange()');
    end;
    fListMutex.Release();

    result := numberAdded;
end;


function TVMemoryLogPersistence.FindEventPage(_endId: Integer; count: Integer; list: TVPageList; filter: TVFilter): Integer;
var
  endId: Integer;
  numberAdded: Integer;
  event: TVLoggingEvent;
  newEvent: TVLoggingEvent;
  i: Integer;
begin
    endId := _endId;
    if (endId < 0) then begin
        endId := TVLogManager.GetInstance().GetNextId();
    end;

    fListMutex.Acquire();
    numberAdded := 0;
    i := fLoggingEventList.Count - 1;
    while ((i >= 0) and (numberAdded <= count)) do begin
        event := TVLoggingEvent(fLoggingEventList.Items[i]);
        if (event.GetIndex() <= endId) then begin
            if (Integer(event.GetLevel()) >= Integer(GetReadLevel())) and ((filter = nil) or (filter.Decide(event) <> frDeny)) then begin
                newEvent := TVLoggingEvent.Create(event);
                list.AddObject(MakeEventKey(newEvent), newEvent);
                Inc(numberAdded);
            end;
        end;
        Dec(i);
    end;
    fListMutex.Release();
    result := numberAdded;
end;


function TVMemoryLogPersistence.FindEvent(id: INteger): TVLoggingEvent;
var
  found: TVLoggingEvent;
  i: Integer;
  event: TVLoggingEvent;
begin
    fListMutex.Acquire();
    found := nil;
    i := 0;
    while ((i < fLoggingEventList.Count) and (found = nil)) do begin
        event := TVLoggingEvent(fLoggingEventList.Items[i]);
        if (event.GetIndex() = id) then begin
            found := event;
        end;
        Inc(i);
    end;
    fListMutex.Release();

    result := found;
end;



//==============================================================================
//==============================================================================
//==============================================================================
//==============================================================================
//==============================================================================
//==============================================================================



constructor TVFileLogPersistence.Create(Owner: TComponent);
begin
    inherited Create(Owner);
    //Log := TVLogger.GetInstance('vectrics.logging.persist.TVFileLogPersistence');

    MaxSize := 5000;  // 5 MB
    fFileReducePercent := 10;

    fAppendsSinceSizeCheck := 0;

    fFileMutex := TCriticalSection.Create();
    fFileName := 'logging_data.txt';
    fOpenedFile := '';
    fWriteFile := nil;
end;


destructor TVFileLogPersistence.Destroy();
begin
    if (GetAppender().Asynchronous) then begin
        GetAppender().Flush();
    end;

    fFileMutex.Acquire();
    if (LogManagerShuttingDown = false) then begin
        TVLogManager.GetInstance().UnregisterFileComponent(self);
    end;

    CloseWriter();
    fFileMutex.Destroy();
    inherited;
end;

// Behavior when TVLogManager - Initializes the Managed Appender.
procedure TVFileLogPersistence.OnAppenderInitialized(Sender: TObject);
var
    fileComponent: TComponent;
begin
    fileComponent := TVLogManager.GetInstance().FindFileComponent(fFileName);
    if (fileComponent = nil) then begin
        TVLogManager.GetInstance().RegisterFileComponent(self, fFileName);
    end
    else if (fileComponent <> self) then begin
        raise TVFileException.Create('TVFileLogAppender - File is already used by another appender or file persistence: ' + fFileName + '  Use another file name, or eliminate redundent file appenders and file persistence instances.');
    end
end;


procedure TVFileLogPersistence.CloseWriter();
begin
    if (Length(fOpenedFile) > 0) then begin
        fWriteFile.Destroy();
        fOpenedFile := '';
    end;
end;


procedure TVFileLogPersistence.OpenWriter();
var
//  theFile: TFileStream;
  fileMode : word;
begin
  if (fOpenedFile <> fFileName) then begin
    if (fWriteFile <> nil) then begin
      fWriteFile.Destroy();
      fWriteFile := nil;
    end;
    try
      if not FileExists(fFileName) then begin
        CreateTheFile();
      end;

      fileMode := fmOpenReadWrite or fmShareDenyNone;
      fWriteFile := TFileStream.Create(fFileName, fileMode);
      fWriteFile.Seek(0, soFromEnd);
      fOpenedFile := fFileName;
    except
      fWriteFile := nil;
    end;
  end
  else begin
  end;
end;


function TVFileLogPersistence.OpenReader(): TFileStream;
var
  theFile: TFileStream;
  fileMode : word;
begin
  try
    if not FileExists(fFileName) then begin
      CreateTheFile();
    end;

    fileMode := fmOpenReadWrite or fmShareDenyNone;

    theFile := TFileStream.Create(fFileName, fileMode);

    except
        on ex: Exception do begin
            raise TVFileException('Exception opening file for reading.  File name = ' + fFileName
                + '  Exception: ' + ex.Message);
        end;
    end;

    result := theFile;
end;



procedure TVFileLogPersistence.Clear();
var
  theFile: TFileStream;
begin
    theFile := OpenReader();
    if (theFile <> nil) then begin
        theFile.Size := 0;
        theFile.Destroy();
    end;
end;


function TVFileLogPersistence.LineToEvent(theLine: String): TVLoggingEvent;
var
  index: integer;
  event: TVLoggingEvent;
  time: TDateTime;
  level: TVLevelValue;
  loggerName: String;
  msg: String;
  ex: String;
  ndc: String;
  posTab: Integer;
  segmentStr: String;
  line: String;
begin
    line := theLine;
    index := -1;
    event := TVLoggingEvent.Create();
    level := lvInfo;
    time := Now();


    try
        posTab := Pos(TAB_CHAR, line);
        if (posTab <= 0)  then begin
          msg := 'Error in line read from file: could not find field separator.';
        end
        else if (posTab > 40) then begin
          msg := 'Error in line read from file: could not find field separator within expected range.';
        end
        else begin
            segmentStr := Copy(line, 1, posTab - 1);
            index := StrToInt(segmentStr);
            line := Copy(line, posTab + 1, Length(line));

            posTab := Pos(TAB_CHAR, line);
            if (posTab > 0) then begin
                segmentStr := Copy(line, 1, posTab - 1);
                time := StrToDateTime(segmentStr);
                line := Copy(line, posTab + 1, Length(line));

                posTab := Pos(TAB_CHAR, line);
                if (posTab > 0) then begin
                    segmentStr := Copy(line, 1, posTab - 1);
                    line := Copy(line, posTab + 1, Length(line));

                    loggerName := Decode(segmentStr);

                    posTab := Pos(TAB_CHAR, line);
                    if (posTab > 0) then begin
                        segmentStr := Copy(line, 1, posTab - 1);
                        line := Copy(line, posTab + 1, Length(line));

                        level := TVLevel.GetLevel(segmentStr);

                        posTab := Pos(TAB_CHAR, line);
                        if (posTab > 0) then begin
                            segmentStr := Decode(Copy(line, 1, posTab - 1));
                            line := Copy(line, posTab + 1, Length(line));

                            msg := segmentStr;
                            posTab := Pos(TAB_CHAR, Line);
                            if (posTab <= 0) then begin
                                posTab := Pos(EOL_CHAR, line);
                            end;
                            if (posTab > 0) then begin
                                segmentStr := Decode(Copy(line, 1, posTab - 1));
                                ndc := segmentStr;
                                line := Copy(line, posTab + 1, Length(line));

                                posTab := Pos(TAB_CHAR, Line);
                                if (posTab <= 0) then begin
                                    posTab := Pos(EOL_CHAR, line);
                                end;
                                if (posTab > 0) then begin
                                    segmentStr := Decode(Copy(line, 1, posTab - 1));
                                    ex := segmentStr;
                                end;
                            end;

                        end;
                    end;
                end;

            end;
        end;
        event.SetIndex(index);
        event.SetTime(time);
        event.SetLogger(loggerName);
        event.SetMessage(msg);
        event.SetException(ex);
        event.SetLevel(level);
        event.SetNdc(ndc);
    except
        TVLogger.GetInstance('vectrics.logging.TVLogListView').Error('LineToEvent() - bad line: ' + theLine);
        event.Destroy();
        event := nil;
    end;

    result := event;
end;  // LineToEvent()



function TVFileLogPersistence.GetSize(): Integer;
var
  size: Integer;
  theFile: TFileStream;
begin
    fFileMutex.Acquire();
    theFile := OpenReader();
    if (theFile = nil) then begin
        size := 0;
    end
    else begin
        size := theFile.Size;
    end;

    fFileMutex.Release();
    result := size;
end;


procedure TVFileLogPersistence.CreateTheFile();
var
  theFile: TFileStream;
begin
    TVLogManager.GetInstance().RegisterFileComponent(self, fFileName);
    if not FileExists(fFileName) then begin
      theFile := TFileStream.Create(fFileName, fmCreate);
      theFile.Destroy;
    end;
{
    if (TVLogManager.GetInstance().IsFileInUse(self, fFileName) = false) then begin
        TVLogManager.GetInstance().RegisterFileComponent(self, fFileName);
        if not FileExists(fFileName) then begin
          theFile := TFileStream.Create(fFileName, fmCreate);
          theFile.Destroy;
        end;
    end
    else begin
        raise TVFileException.Create('TVFileLogPersistence tried to attach to file that is already managed by an appender or other file persistence component.');
    end;
    }
end;


procedure TVFileLogPersistence.CheckReduceFileSize();
var
  fileTmp: TFileStream;
  startPos: Integer;
  done: boolean;
  eolFound: boolean;
  theFile: TFileStream;
  buffer: Char;
  size: Integer;
begin
    fFileMutex.Acquire();

    if (fWriteFile <> nil) then begin
        if (fWriteFile.Size  > MaxSize * 1000) then begin
            theFile := OpenReader();
            fileTmp := TFileStream.Create('tmp.dat',  fmCreate or fmShareDenyNone);

            startPos := (1000 * MaxSize * fFileReducePercent div 100) + (fWriteFile.Size - (1000 * MaxSize));
            theFile.Seek(startPos, soFromBeginning);

            // Find start of next line
            done := false;
            eolFound := false;

            //char buffer[5];
            while ((done = false)) do begin
                size := theFile.Read(buffer, 1);
                if (size > 0) then begin
                    //buffer[1] = 0;
                    if ((buffer = EOL_CHAR) or (buffer = CR_CHAR)) then begin
                        Inc(startPos);
                        eolFound := true;
                    end
                    else begin
                        if (eolFound) then begin
                            done := true;
                        end
                        else begin
                            Inc(startPos);
                        end;
                    end;
                end
                else begin
                    //EOF
                    done := true;
                end;
            end;

            theFile.Seek(startPos, soFromBeginning);
            fileTmp.Size := 0;
            fileTmp.CopyFrom(theFile, theFile.Size - startPos);

            theFile.Size := 0;
            fileTmp.Seek(0, soFromBeginning);
            theFile.CopyFrom(fileTmp, 0);

            fileTmp.Size := 0;
            fileTmp.Destroy;
            theFile.Destroy();
            fWriteFile.Seek(0, soFromEnd);
        end;


    end;
    fFileMutex.Release();
end;



function TVFileLogPersistence.Encode(before: String): String;
var
  newData: String;
  flags: TReplaceFlags;
begin
    newData := before;
    flags := [rfReplaceAll];
    newData := StringReplace(newData, EOL_CHAR, '{eol}', flags);
    newData := StringReplace(newData, CR_CHAR, '{cr}', flags);
    newData := StringReplace(newData, TAB_CHAR, '{tab}', flags);
    result := newData;
end;


function TVFileLogPersistence.Decode(before: String): String;
var
  newData: String;
  flags: TReplaceFlags;
begin
    newData := before;
    flags := [rfReplaceAll];
    newData := StringReplace(newData, '{eol}', EOL_CHAR,  flags);
    newData := StringReplace(newData, '{cr}', CR_CHAR,  flags);
    newData := StringReplace(newData, '{tab}', TAB_CHAR,  flags);
    result := newData;
end;


//procedure TVFileLogPersistence.DoAppend(logEvent: TVLoggingEvent);
procedure TVFileLogPersistence.AppendEvent(logEvent: TVLoggingEvent);
var
  line: String;
  buffer: PChar;
begin
    if (fOpenedFile <> fFileName) then begin
        OpenWriter();
    end;

    if (fAppendsSinceSizeCheck >= 1000) then begin
        CheckReduceFileSize();
        fAppendsSinceSizeCheck := 0;
    end;

    fFileMutex.Acquire();
    line := Encode(IntToStr(logEvent.GetIndex())) + TAB_CHAR
            + Encode(DateTimeToStr(logEvent.GetTime())) + TAB_CHAR
            + Encode(logEvent.GetLogger()) + TAB_CHAR +
            Encode(TVLevel.ToSymbol(logEvent.GetLevel())) + TAB_CHAR +
            Encode(logEvent.GetMessage()) + TAB_CHAR
            + Encode(logEvent.GetNdc()) + TAB_CHAR
            + Encode(logEvent.GetException()) + TAB_CHAR
            + EOL_CHAR + CR_CHAR;

    buffer := PChar(line);
    fWriteFile.Write(buffer^, Length(line));
    Inc(fAppendsSinceSizeCheck);

    fFileMutex.Release();
end;



function TVFileLogPersistence.FindEventPageForwardInFile(theFile: TFileStream; _startId: integer;
        maxCount: Integer; list: TVPageList; filter: TVFilter; lastIndexInFile: Integer): Integer;
var
  startIndex: Integer;
  endIndex: Integer;
  count: Integer;
  countBeforeScan: INteger;
  done: boolean;
  event: TVLoggingEvent;
  newEvent: TVLoggingEvent;
  subList: TVPageList;
  i: integer;
  sampleSize: Integer;
begin
  startIndex := _startId;
  count := 0;
  done := false;
  sampleSize := maxCount;

  subList := TVPageList.Create();
  while (count < maxCount) and (done = false) do begin
    endIndex := startIndex + sampleSize;
    FindEventRangeInFile(theFile, startIndex, endIndex, subList, filter);
    for i := 0 to subList.Count - 1 do begin
      event := TVLoggingEvent(subList.Objects[i]);
      countBeforeScan := count;
      if (event.GetLevel() >= GetReadLevel()) then begin
        if (filter = nil) or (filter.Decide(event) <> frDeny) then begin
          if (count < maxCount) then begin
            newEvent := TVLoggingEvent.Create(event);
            list.AddObject(MakeEventKey(newEvent), newEvent);
            Inc(count);
          end;
        end;
      end;

      if (count - countBeforeScan < (80 * maxCount) div 100) then begin
        sampleSize := sampleSize * 2;
      end;

    end;
    subList.DeleteMembers();
    startIndex := startIndex + maxCount + 1;

    if (startIndex > lastIndexInFile) then begin
      done := true;
    end;

  end;
  list.Sort();
  subList.Destroy();
  result := count;

end;



function TVFileLogPersistence.FindEventPage(_endId: integer; maxCount: Integer; list: TVPageList; filter: TVFilter): Integer;
var
  endId: Integer;
  count: Integer;
  countBeforeScan: Integer;
  lastIndexInFile: Integer;
  theFile: TFileStream;
  reachedBof: boolean;
  firstIndexInFile: Integer;
  startIndex: Integer;
  //firstPos: Integer;
  subList: TVPageList;
  subListIndex: Integer;
  subListSize: Integer;
  newEvent: TVLoggingEvent;
  first: boolean;
  event: TVLoggingEvent;
  done: boolean;
  sampleSize: Integer;
  startPos : Integer;
  nextPos : Integer;
  tentativeStartIndex : Integer;
begin
    //TVLoggingEvent *dbgEvent = NULL;
    sampleSize := maxCount;
    //nextStartPos := -1;
    endId := _endId;
    count := 0;
    fFileMutex.Acquire();

    thefile := OpenReader();
    subList := TVPageList.Create();

    if (theFile <> nil) then begin
        firstIndexInFile := FindFirstIndexInFile(theFile);
        lastIndexInFile := FindLastIndexInFile(theFile);

        if (lastIndexInFile > TVLogManager.GetInstance().GetNextId()) then begin
          TVLogManager.GetInstance().SetNextId(lastIndexInFile + 1);
        end;

        if (endId > lastIndexInFile) then begin
            endId := lastIndexInFile;
        end;
        if (endId < firstIndexInFile) then begin
          endId := firstIndexInFile;
        end;

        reachedBof := false;
        done := false;
        startIndex := 1 + endId - maxCount;

        if (firstIndexInFile > 0) then begin

            while ((done = false) and (count < maxCount)) do begin
                  tentativeStartIndex :=  -1;

                //firstPos := - 1;

                countBeforeScan := count;
                if (reachedBof) then begin

                  FindEventPageForwardInFile(theFile, _endId + 1, maxCount - count, subList, filter, lastIndexInFile);
                  done := true;

                  subListSize := subList.Count;
                  if (subListSize > 0) then begin
                      subListIndex := 0;

                      while (subListIndex < subListSize)  do begin
                          event := TVLoggingEvent(subList.Objects[subListIndex]);
                          if (event.GetLevel() >= GetReadLevel()) then begin

                              if (count < maxCount) then begin
                                  if ((filter = nil) or (filter.Decide(event) <> frDeny)) then begin
                                      newEvent := TVLoggingEvent.Create(event);
                                      list.AddObject(MakeEventKey(newEvent), newEvent);
                                      Inc(count);
                                  end;
                              end;

                          end;
                          Inc(subListIndex);
                      end;
                  end;

                end
                else begin
                  // Read backwards to fill page.
                  FindEventRangeInFile(theFile, startIndex, endId, subList, filter);
                  subListSize := subList.Count;
                  if (subListSize > 0) then begin

                      subListIndex := subList.Count - 1;
                      first := true;

                      while (subListIndex >= 0)  do begin
                          event := TVLoggingEvent(subList.Objects[subListIndex]);
                          if (event.GetLevel() >= GetReadLevel()) then begin

                              if (count < maxCount) then begin
                                  if ((filter = nil) or (filter.Decide(event) <> frDeny)) then begin
                                      if (first) then begin
                                          startPos := FindPositionOfIndexInFile(theFile, event.GetIndex(), firstIndexInFile, lastIndexInFile);
                                          nextPos := FindPosBackwards(theFile, startPos, sampleSize - 1);
                                          theFile.Seek(soFromBeginning, nextPos);
                                          tentativeStartIndex :=  FindNextIndexInStream(nextPos, theFile, false);

                                          first := false;
                                      end;
                                      newEvent := TVLoggingEvent.Create(event);
                                      list.AddObject(MakeEventKey(newEvent), newEvent);
                                      Inc(count);
                                  end;
                              end;

                          end;
                          Dec(subListIndex);
                      end;
                  end;
                end;


                subList.DeleteMembers;
                endId := startIndex - 1;
                if (count - countBeforeScan < (80 * maxCount) div 100) then begin
                  sampleSize := sampleSize * 2;
                  if (sampleSize > 5000) then  // Don't use too much memory
                    sampleSize := 5000;
                end;

                if (tentativeStartIndex >= 0) then begin
                    startIndex := tentativeStartIndex;
                end
                else begin
                    startIndex := startIndex - sampleSize;
                end;

                if ((endId < firstIndexInFile) or (startIndex > endId)) then begin
                    reachedBof := true;
                end;



            end;
        end;

        theFile.Destroy();
    end;
    subList.Destroy();
    fFileMutex.Release();
    result := count;
    list.Sort;
end;



function TVFileLogPersistence.FindPosBackwards(theFile: TFileStream; startPos: Integer; backupLines: Integer): Integer;
var
  currentPos: Integer;
  lineCount: Integer;
  endCharCount: Integer;
  buffer: Char;
begin
    currentPos := startPos;
    lineCount := 0;
    endCharCount := 0;

    while ((currentPos > 0) and (lineCount < backupLines)) do begin
        theFile.Seek(currentPos, soFromBeginning);
        if (theFile.Read(buffer, 1) > 0) then begin
            if ((buffer = EOL_CHAR) or (buffer = CR_CHAR)) then begin
                Inc(endCharCount);
            end;
            lineCount := endCharCount div 4;
        end;
        Dec(currentPos);
    end;
    result := currentPos;
end;



function TVFileLogPersistence.FindIndexAtPosition(position: Integer): Integer;
var
  theFile: TFileStream;
begin
    fFileMutex.Acquire();
    theFile := OpenReader();
    result := FindNextIndexInStream(position, theFile, false);
    theFile.Destroy();
    fFileMutex.Release();

end;

function TVFileLogPersistence.FindNextLineInStream(seekPos: Integer; theFile: TFileStream; var line: String; atLineStart: boolean): Integer;
var
  buffer: Char;
  index: Integer;
  eolFound: boolean;
  started: boolean;
  done: boolean;
  size: Integer;
begin
    theFile.Seek(seekPos, soFromBeginning);
    line := '';

    index := seekPos;
    eolFound := false;
    started := false;
    done := false;
    if (atLineStart) then begin
        eolFound := true;
    end;

    while ((done = false)) do begin
        size := theFile.Read(buffer, 1);
        Inc(index);
        if (size > 0) then begin
            if ((buffer = EOL_CHAR) or (buffer = CR_CHAR)) then begin
                if (atLineStart) then begin
                    done := true;
                end
                else begin
                    if (started) then begin
                        done := true;  // 2nd eol found, we're done
                    end
                    else begin
                        eolFound := true;
                    end;
                end;
            end
            else begin
                if (eolFound) then begin
                    started := true;
                    line := line + buffer;
                end;
            end
        end
        else begin
            done := true;
        end;
    end;
    if (Length(line) <= 0) then begin
        index := -1;
    end;
    result := index;
end;  //FindNextLine in Stream


function TVFileLogPersistence.FindNextIndexInStream(seekPos: Integer; theFile: TFileStream; atLineStart: boolean): Integer;
var
  buffer: Char;
  index: Integer;
  indexStr: String;
  eolFound: boolean;
  started: boolean;
  done: boolean;
  size: Integer;
begin
    theFile.Seek(seekPos, soFromBeginning);
    eolFound := false;
    started := false;
    done := false;
    index := 0;
    if (atLineStart) then begin
        eolFound := true;
    end;

    while (done = false) do begin
        size := theFile.Read(buffer, 1);
        Inc(index);

        if (size > 0) then begin
            if ((buffer = EOL_CHAR) or (buffer = CR_CHAR)) then begin
                if (atLineStart) then begin
                    done := true;
                end
                else begin
                    if (started) then begin
                        done := true;  // 2nd eol found, we're done
                    end
                    else begin
                        eolFound := true;
                    end;
                end;
            end
            else begin
                if (eolFound) then begin
                    if (buffer = TAB_CHAR) then begin
                        done := true;
                    end
                    else begin
                        started := true;
                        indexStr := indexStr + buffer;
                    end;
                end;
            end
        end
        else begin
            done := true;
            index := 0;
        end;

    end;
    if (Length(indexStr) > 0) then begin
        index := StrToInt(indexStr);
    end;

    result := index;
end;

function TVFileLogPersistence.FindFirstIndex(): Integer;
var
  theFile: TFileStream;
begin
    result := -1;
    fFileMutex.Acquire();
    try
        theFile := OpenReader();
        if (theFile <> nil) then begin
            result := FindFirstIndexInFile(theFile);
            theFile.Destroy();
        end;
    except
    end;
    fFileMutex.Release();
    result := result;
end;

function TVFileLogPersistence.FindFirstIndexInFile(theFile: TFileStream): Integer;
var
  firstIndex: Integer;
begin
    firstIndex := 0;
    if (theFile <> nil) then begin
        firstIndex := FindNextIndexInStream(0, theFile, false);    // For now, we are going to skip the
    end;
    result := firstIndex;
end;

function TVFileLogPersistence.FindLastIndex(): Integer;
var
  lastIndex: Integer;
  theFile: TFileStream;
begin
    lastIndex := -1;
    fFileMutex.Acquire();
    try
        theFile := OpenReader();
        if (theFile = nil) then begin
            lastIndex := -1;
        end
        else begin
            lastIndex := FindLastIndexInFile(theFile);
            theFile.Destroy();
        end;
    except
    end;
    fFileMutex.Release();
    result := lastIndex;
end;

function TVFileLogPersistence.FindLastIndexInFile(theFile: TFileStream): Integer;
var
  lastIndex: Integer;
  fileSize: Integer;
  seekPos: Integer;
  done: boolean;
  buffer: Char;
begin

    if (theFile = nil) then begin
        lastIndex := -1;
    end
    else begin
        fileSize := theFile.Size;
        seekPos := fileSize - 5;
        if (theFile.Size > 0) then begin
            // Back up until beggining of file or
            done := false;

            while ((seekPos >= 0) and (done = false)) do begin
                theFile.Seek(seekPos, soFromBeginning);
                if (theFile.Read(buffer, 1) > 0) then begin
                    if ((buffer = EOL_CHAR) or (buffer = CR_CHAR)) then begin
                        done := true;
                    end
                    else begin
                    end;
                    Dec(seekPos);
                end
                else begin
                    done := true;
                end;
            end;
        end;

        lastIndex := FindNextIndexInStream(seekPos, theFile, false);
    end;

    result := lastIndex;
end;


function TVFileLogPersistence.FindPositionOfIndex(index: Integer): Integer;
var
  //posStart: Integer;
//  seekIncrement: Integer;
//  doneSeeking: boolean;
//  stuckOnOneCount: integer;
//  currentIndex: Integer;
  firstIndex: Integer;
  lastIndex: Integer;
  theFile: TFileStream;
begin

  result := -1;
  firstIndex := FindFirstIndex();
  lastIndex := FindLastIndex();
  theFile := OpenReader();
  if (theFile <> nil) then begin
    result := FindPositionOfIndexInFile(theFile, index, firstIndex, lastIndex);
    theFile.Destroy();
  end;

end;



function TVFileLogPersistence.FindPositionOfIndexInFile(theFile: TFileStream; index: Integer; firstIndex: Integer; lastIndex: Integer): Integer;
var
  posStart: Integer;
  seekIncrement: Integer;
  doneSeeking: boolean;
  stuckOnOneCount: integer;
  currentIndex: Integer;
  //iterations: Integer;  // FOr debugging - tells us how many times this method looped.
begin
    //iterations := 0;
    result := -1;
    //Log.Debug('FindPositionOfIndexInFile() - called.  index = ' + IntToStr(index));

    if (theFile <> nil) then begin
        posStart := 0;

        if (firstIndex >= 0) then begin
            if (index > firstIndex) then begin
                posStart := ((theFile.Size * index) div lastIndex) - 1000;
                if (posStart < 0) then
                  posStart := 0;

                currentIndex := FindNextIndexInStream(posStart, theFile, false);
                doneSeeking := false;
                if (currentIndex < index) then begin
                    seekIncrement := (theFile.Size - posStart) div 2;
                end
                else if (currentIndex > index) then begin
                    seekIncrement := 0 - (posStart div 2);
                end
                else begin
                  doneSeeking := true;
                  seekIncrement := 1;
                  result := posStart;
                end;

                stuckOnOneCount := 0;
                while (doneSeeking = false) do begin
                    //Inc(iterations);
                    posStart := posStart + seekIncrement;

                    if (posStart > theFile.Size) then begin
                        posStart := theFile.Size - 2;
                        if (seekIncrement = 1) then begin
                            doneSeeking := true;
                        end;
                    end;
                    if (posStart < 0) then begin
                        posStart := 0;
                        if (seekIncrement = -1) then begin
                            doneSeeking := true;
                        end;
                    end;

                    currentIndex := FindNextIndexInStream(posStart, theFile, false);
                    if ((stuckOnOneCount > 2)  and (currentIndex < index)) then begin
                        doneSeeking := true;
                        result := posStart;
                    end
                    else if (currentIndex > index) then begin
                        if (seekIncrement > 0) then
                            seekIncrement := 0 - seekIncrement;
                        seekIncrement := seekIncrement div 2;
                        if (seekIncrement > -1) then begin
                            seekIncrement := -1;
                            Inc(stuckOnOneCount);
                        end;
                    end
                    else if (currentIndex < index) then begin
                        if (seekIncrement < 0) then begin
                            seekIncrement := 0 - seekIncrement;
                        end;
                        seekIncrement := seekIncrement div 2;

                        if (seekIncrement < 1) then begin
                            seekIncrement := 1;
                            Inc(stuckOnOneCount);
                        end;
                    end
                    else begin
                        doneSeeking := true;
                        result := posStart;
                    end;

                end
            end
            else if (index = firstIndex) then begin
                result := posStart;
            end;
        end;
    end;
    //if (Log.IsEnabledFor(lvDebug)) then begin
    //    Log.Debug('FindPositionOfIndexInFile()   iterations = ' + IntToStr(iterations));
    //end;
end;




function TVFileLogPersistence.FindEventRange(_startId: Integer; _endId: Integer; list: TVPageList; filter: TVFilter): Integer;
var
  count: Integer;
  theFile: TFileStream;
begin
    count := 0;
    theFile := OpenReader();
    if (theFile <> nil) then begin
        count := FindEventRangeInFile(theFile, _startId, _endId, list, filter);
    end;
    theFile.Destroy();
    result := count;
end;


function TVFileLogPersistence.FindEventRangeInFile(theFile: TFileStream; _startId: Integer; _endId: Integer; list: TVPageList; filter: TVFilter): Integer;
var
  numberAdded: INteger;
  posOfIndex: Integer;
  lastIndexInFile: Integer;
  firstIndexInFile: Integer;
  startId: Integer;
  endId: Integer;
  line: String;
  nextPos: Integer;
  doneAdding: boolean;
  event: TVLoggingEvent;
  eventIndex: Integer;
  eventAdded: boolean;
begin
    numberAdded := 0;

    posOfIndex := -1;
    lastIndexInFile := FindLastIndexInFile(theFile);
    firstIndexInFile := FindFirstIndexInFile(theFile);
    startId := _startId;
    endId := _endId;
    nextPos := 0;

    if (startId < firstIndexInFile) then begin
        startId := firstIndexInFile;
    end;

    if (startId <= lastIndexInFile) then begin
        posOfIndex :=  FindPositionOfIndexInFile(theFile, startId, firstIndexInFile, lastIndexInFile);
        if (posOfIndex < 0) then begin
            posOfIndex := 0;
        end;
        nextPos := posOfIndex;
    end;

    if (endId > lastIndexInFile) then begin
        endId := lastIndexInFile;
    end;

    if (posOfIndex >= 0) then begin
        if (theFile <> nil) then begin
            theFile.Seek(posOfIndex, soFromBeginning);

            nextPos := FindNextLineInStream(nextPos, theFile, line, false);
            doneAdding := false;
            while ((nextPos >= 0) and (doneAdding = false)) do begin
                if (Length(line) > 5) then begin
                    event := LineToEvent(line);

                    eventIndex := event.GetIndex();
                    eventAdded := false;
                    if (event.GetLevel() >= GetReadLevel()) then begin
                      if ((eventIndex >= startId) and (eventIndex <= endId)) then begin
                          if ((filter = nil) or (filter.Decide(event) <> frDeny)) then begin
                              eventAdded := true;
                              list.AddObject(MakeEventKey(event), event);
                              Inc(numberAdded);
                          end;
                      end
                    end;

                    if (eventAdded = false) then begin
                        event.Destroy();

                    end;
                    if (eventIndex > endId) then begin
                      doneAdding := true;
                    end;
                end
                else begin

                end;

                nextPos := FindNextLineInStream(nextPos, theFile, line, false);
            end;

        end;
    end;

    result := numberAdded;
end;


function TVFileLogPersistence.FindEvent(id: Integer): TVLoggingEvent;
var
  found: TVLoggingEvent;
  list: TVPageList;
begin
    fFileMutex.Acquire();
    found := nil;

    list := TVPageList.Create();
    FindEventRange(id, id, list, nil);

    if (list.Count > 0) then begin
        found := TVLoggingEvent(list.Objects[0]);
    end;

    list.DeleteMembers;

    list.Destroy();
    fFileMutex.Release();

    result := found;
end;


function TVFileLogPersistence.GetFileName(): String;
begin
    result := fFileName;
end;

procedure  TVFileLogPersistence.SetFileName(fileName: String);
begin
    fFileName := fileName;
end;



procedure Register;
begin
  RegisterComponents('Logging', [TVFilter]);
  RegisterComponents('Logging', [TVLog]);
  RegisterComponents('Logging', [TVAppender]);
  RegisterComponents('Logging', [TVFileAppender]);
  RegisterComponents('Logging', [TVRollingFileAppender]);
  RegisterComponents('Logging', [TVConsoleAppender]);
  RegisterComponents('Logging', [TVFileLogPersistence]);
  RegisterComponents('Logging', [TVMemoryLogPersistence]);
  RegisterComponents('Logging', [TVTestEventGenerator]);
end;


initialization
  fAppenderInstanceNumber := 0;
  REGISTRY_KEY_PREFIX := 'SOFTWARE\\vectrics';
  REGISTRY_KEY_SUFFIX := '\\elsx';
  fInstance := nil;
  LogManagerShuttingDown := false;
  fNextId := 1000;


end.