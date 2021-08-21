(*
   TFnugryLogFile Component
   Copyright (C) 1998 Gleb Yourchenko

   Version 1.0.0.1

   Contact:
   Gleb Yourchenko
   E-mail: fnugry@null.net
   Please specify "TFnugryLogFile" in the subject string
   
*)

unit fnglogfile;

interface
uses
   Windows, SysUtils, Classes;


type

  ELogError = class(Exception);
  ELogOpenError = class(ELogError);
  ELogWriteError = class(ELogError);
  ELogBackupError = class(ELogError);

  TLogOption = (
     logTimestamp,    // timestamp messages
     logSource,       // log message source
     logCategory,     // log message category
     logSession       // log open/close messages.
     );
  TLogOptions = set of TLogOption;

  TLogArchiveMode = (
     bkNone,   // no automatic archive
     bkSize,   // move to archive when size exceeds maxsize
     bkDaily,  // daily archive
     bkWeekly, // weekly archive
     bkMonthly // monthly archive
  );


  TFnugryLogFile = class(TComponent)
  private
     FLogFile     :THandleStream;
     FOptions     :TLogOptions;
     FMaxSize     :Longint;
     FLogFileName :String;
     FArchiveDir  :String;
     FArchiveMode :TLogArchiveMode;
     FAutoOpen    :Boolean;
     FTimestamp   :String;
     FOnChange    :TNotifyEvent;
     FOnOpen      :TNotifyEvent;
     FOnClose     :TNotifyEvent;
     procedure SetMaxSize(Value :Longint);
     procedure SetLogFileName(const Value :String);
     function GetLogOpen :Boolean;
     procedure WriteLine(const Line :String);
     procedure SetOptions(const Value :TLogOptions);
     procedure SetArchiveMode(const Value :TLogArchiveMode);
     procedure SetArchiveDir(const Value :String);
  protected
     procedure BackupCheck; virtual;
     procedure Loaded; override;
     procedure Opened; virtual;
     procedure Closed; virtual;
     procedure Change; virtual;
     procedure WriteMsg(const Src, Ctg, Msg :String); virtual;
  public
     constructor Create(AOwner :TCOmponent); override;
     destructor Destroy; override;
     procedure Open;
     procedure Close;
     procedure Clear;
     procedure LogMsg(const Src, Ctg, Msg :String);
     procedure LogMsgFmt(const Src, Ctg, Msg :String;
       const Args :array of const);
     procedure LogMsgRes(const Src, Ctg :String; MsgID :Integer);
     procedure LogMsgResFmt(const Src, Ctg :String;
       MsgID :Integer; const Args :array of const);
     property LogOpen :Boolean
       read GetLogOpen;
  published
     property AutoOpen :Boolean
       read FAutoOpen write FAutoOpen;
     property Options :TLogOptions
       read FOptions write SetOptions;
     property MaxSize :Longint
       read FMaxSize write SetMaxSize;
     property LogFileName :String
       read FLogFileName write SetLogFileName;
     property ArchiveDir :String
       read FArchiveDir write SetArchiveDir;
     property ArchiveMode :TLogArchiveMode
       read FArchiveMode write SetArchiveMode;
     property TimestampFormat  :String
       read FTimestamp write FTimestamp;
     property OnChange :TNotifyEvent
       read FOnChange write FOnChange;
     property OnOpen :TNotifyEvent
       read FOnOpen write FOnOPen;
     property OnClose :TNotifyEvent
       read FOnClose write FOnCLose;
  end;

procedure Register;

implementation
uses
   filectrl, dateutil;

procedure Register;
begin
   RegisterComponents('Fnugry Tools', [TFnugryLogFile]);
end;

const

   m_fmt_timestamp      = 'dd/mmm/yyyy hh:nn:ss ';

   m_err_maxsize        = 'Invalid MaxSize value';
   m_err_invalidstate   = 'Invalid Operation';
   m_err_clear          = 'Could not clear log file';
   m_err_open           = 'Could not open log file "%s"';
   m_err_write          = 'Error writing log file (%d)';
   m_err_backup         = 'Error saving log backup';

type

  TLogFileStream = class(THandleStream)
  public
     constructor Create(const FileName :String);
     destructor Destroy; override;
  end;

destructor TLogFileStream.Destroy;
begin
   if Handle <> 0 then
      FileClose(Handle);
end;

constructor TLogFileStream.Create(const FileName :String);
var
   H :THandle;
begin
   H := CreateFile(PChar(FileName),  GENERIC_READ OR GENERIC_WRITE,
     FILE_SHARE_READ OR FILE_SHARE_WRITE, NIL, OPEN_ALWAYS,
     FILE_ATTRIBUTE_NORMAL OR FILE_FLAG_WRITE_THROUGH, 0);
   if H = 0 then raise ELogOpenError.CreateFmt(m_err_open, [FileName]);
   SetFilePointer(H, 0, nil, FILE_END);
   inherited Create(H);
end;


procedure TFnugryLogFile.Loaded;
begin
   if FAutoOpen then Open;
end;

procedure TFnugryLogFile.Opened;
begin
   if assigned(FOnOpen) then FOnOpen(Self);
end;

procedure TFnugryLogFile.Closed;
begin
  if assigned(FOnCLose) then FOnCLose(Self);
end;

procedure TFnugryLogFile.Change;
begin
  if assigned(FOnChange) then FOnChange(Self);
end;

procedure TFnugryLogFile.LogMsg(const Src, Ctg, Msg :String);
begin
  writemsg(Src, Ctg, Msg)
end;

procedure TFnugryLogFile.LogMsgFmt(const Src,
  Ctg, Msg :String;  const Args :array of const);
begin
  writemsg(Src, Ctg, format(Msg, Args));
end;

procedure TFnugryLogFile.LogMsgRes(const Src,
  Ctg :String; MsgID :Integer);
begin
  writemsg(Src, Ctg, LoadStr(MsgID))
end;

procedure TFnugryLogFile.LogMsgResFmt(const Src, Ctg :String;
  MsgID :Integer; const Args :array of const);
begin
  writemsg(Src, Ctg, FmtLoadStr(MsgID, Args));
end;

procedure TFnugryLogFile.SetMaxSize(Value :Longint);
begin
  if Value <> FMaxSize then
     begin
        if Value < 1024 then
           raise ELogError.Create(m_err_maxsize);
        FMaxSize := Value;
        Change;
     end;
end;

procedure TFnugryLogFile.SetLogFileName(const Value :String);
begin
  if LogOpen then
     raise ELogError.Create(m_err_invalidstate);
  if stricomp(PChar(FLogFileName), PChar(Value)) <> 0 then
     begin
        FLogFileName := Value;
        Change;
     end;
end;

constructor TFnugryLogFile.Create(AOwner :TCOmponent);
begin
  inherited;
  FOptions := [logTimestamp, logSession];
  FMaxSize := 1048576;
  FArchiveMode := bkSize;
  FAutoOpen := false;
  FTimestamp := m_fmt_timestamp;
end;

destructor TFnugryLogFile.Destroy;
begin
  Close;
  inherited;
end;

function TFnugryLogFile.GetLogOpen :Boolean;
begin
  result := Bool(FLogFile);
end;


procedure TFnugryLogFile.WriteMsg(const Src, Ctg, Msg :String);
var
  L :String;
begin
  l := '';
  if logSource in FOptions then
     l := l + format('%s: ', [Src]);
  if logTimestamp in FOptions then
     l := l + FormatDateTime(FTimestamp, now);
  if logCategory in FOptions then
     l := l + format('[%s] ', [Ctg]);
  WriteLine( L + Msg + #13#10 );
end;

procedure TFnugryLogFile.Close;
begin
   if assigned(FLogFile) then
      begin
         if logSession in FOptions then
            WriteLine( format(#13#10'*** Log file closed %s ***'#13#10,
             [FOrmatDateTime(FTimestamp, now)]));
         FLogFile.Free;
         FLogFile := Nil;
         Closed;
      end;
end;

procedure TFnugryLogFile.Clear;
begin
  if not assigned(FLogFile) then
     raise ELogError.Create(m_err_invalidstate);
  if SetFilePointer(FLogFile.Handle, 0, nil, FILE_BEGIN) <> 0 then
     raise ELogError.Create(m_err_invalidstate);
  if not SetEndOfFile(FLogFIle.Handle) then
     raise ELogError.Create(m_err_invalidstate);
  Change;
end;

procedure TFnugryLogFile.WriteLine(const Line :String);
begin
  if not LogOpen then
     raise ELogWriteError.Create(m_err_invalidstate);
  FLogFile.Write(PChar(Line)^, Length(Line));
  Change;
  BackupCheck;
end;

procedure TFnugryLogFile.Open;
begin
   if not LogOpen then
      begin
         try
            FLogFile := TLogFileStream.Create(FLogFileName);
         except
            raise ELogOpenError.CreateFmt(m_err_open, [FLogFileName]);
         end;
         try
           BackupCheck;
           if logSession in FOptions then
              WriteLine( format(#13#10'*** Log file opened %s ***'#13#10,
               [FOrmatDateTime(FTimestamp, now)]));
           Opened;
         except
           FLogFile.Free;
           FLogFile := Nil;
           raise;
         end;
      end;
end;

procedure TFnugryLogFile.SetOptions(const Value :TLogOptions);
begin
   if FOptions <> Value then
      begin
         FOptions := Value;
         Change;
      end;
end;

procedure TFnugryLogFile.SetArchiveMode(const Value :TLogArchiveMode);
begin
   if Value <> FArchiveMode then
      begin
         FArchiveMode := Value;
         Change;
      end;
end;


procedure TFnugryLogFile.BackupCheck;
var
   fi :TByHandleFileInformation;
   bf :Boolean;
   ft :TSystemTime;
   tm :TFileTime;
   dc :Integer;
   fn :String;
begin
   assert(LogOpen);
   if ArchiveMode = bkNone then
      exit;
   fillchar(fi, sizeof(fi), 0);
   if not GetFileInformationByHandle(FLogFile.Handle, fi) then
      raise ELogBackupError.Create(m_err_backup);
   bf := false;
   if FArchiveMode = bkSize then
      bf := fi.nFileSizeLow >= FMaxSize
   else
      begin
          if not FileTimeToSystemTime(fi.ftCreationTime, ft) then
             raise ELogBackupError.Create(m_err_backup);
          dc := DaysBetween(EncodeDate(ft.wYear, ft.wMonth, ft.wDay), Now);
          case ArchiveMode of
            bkDaily : bf := dc >= 1;
            bkWeekly : bf := dc >= 7;
            bkMonthly : bf := dc >= DaysPerMonth(ExtractYear(now), ExtractMonth(now));
          end;
      end;
   if bf then
      begin
         if trim(FArchiveDir) <> '' then
            if not DirectoryExists(FArchiveDir) then
               CreateDir(FArchiveDir);
         dc := 0;
         repeat
            inc(dc);
            fn := format('%s-%s.%d', [ExtractFileName(FLogFileName),
              FormatDateTime('mm-yyyy', now), dc]);
            if FArchiveDir <> '' then
               fn := FArchiveDir + '\' + fn;
         until not FileExists(fn);
         if not CopyFile(PChar(FLogFileName), PChar(fn), true) then
            raise ELogBackupError.Create(m_err_backup);
         Clear;
         GetSystemTime(ft);
         SystemTimeToFileTime(ft, tm);
         if not SetFileTime(FLogFile.Handle, @tm, @tm, @tm) then
            raise ELogBackupError.Create(m_err_backup);
      end;
end;


procedure TFnugryLogFile.SetArchiveDir(const Value :String);
begin
   if Value <> FArchiveDir then
      begin
         FArchiveDir := trim(Value);
         if FArchiveDir[Length(FArchiveDir)] = '\' then
            SetLength(FArchiveDir, Length(FArchiveDir)-1);
      end;
end;


end.
