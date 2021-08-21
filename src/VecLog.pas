{==============================================================================|
| Project : IMAPSize                                                           |
|==============================================================================|
| $Id: VecLog.pas,v 1.3 2004/03/31 23:27:31 Ivan Exp $
|==============================================================================|
| Copyright 2003-2004 Ivan Vecanski                                            |
| =============================================================================}

unit VecLog;

{ Class used for flexible logging }

interface

uses Logging, classes, sysutils;

const
    DEV_LOGGER = 0;
    IMAP_LOGGER = 1;
    BACKUP_LOGGER = 2;

type

    TISLayout = class(TLayout)
    private
    public
        constructor Create;
        function format(logEvent: TVLoggingEvent): String; override;
    end;

    TVecLog = class
        constructor Create(AOwner: TComponent; logDir: String);
        procedure Flush;
        destructor Destroy; override;
        procedure SetLoggerLevel(logger: Integer; level: TVLevelValue);
        procedure UpdateLoggingSettings;
    end;

var
    devLog: TVLog;
    imapLog: TVLog;
    backupLog: TVLog;
    fileAppender: TVRollingFileAppender;

implementation

uses Main;

constructor TISLayout.Create;
begin
    Inherited;
end;

function TISLayout.Format(logEvent: TVLoggingEvent): String;
begin
    Result :=
            DateTimeToStr(logEvent.GetTime()) + ' ' + logEvent.GetLogger() + ' ' +
            SysUtils.Format('%5s',[TVLevel.ToSymbol(logEvent.GetLevel())])+ ' ' +
            logEvent.GetMessage() + EOL_CHAR + CR_CHAR;
end;


constructor TVecLog.Create(AOwner: TComponent; logDir: String);
begin
    // devLog used for development logging
    devLog := TVLog.Create(AOwner);
    devLog.SetCategory('dev');

    // imapLog used for raw IMAP traffic log
    imapLog := TVLog.Create(AOwner);
    imapLog.SetCategory('imp');

    backupLog := TVLog.Create(AOwner);
    backupLog.SetCategory('bak');

    fileAppender := TVRollingFileAppender.Create(AOwner);
    fileAppender.SetLayout(TISLayout.Create);
    fileAppender.SetFileName(logDir+'\imapsize.log');
    fileAppender.SetAsynchronous(true);

    UpdateLoggingSettings;
end;

procedure TVecLog.UpdateLoggingSettings;
begin
    case settings.LogSettings.AppLog of
        0: devLog.SetLevel(lvOff);
        1: devLog.SetLevel(lvError);
        2: devLog.SetLevel(lvWarn);
        3: devLog.SetLevel(lvInfo);
        4: devLog.SetLevel(lvDebug);
        5: devLog.SetLevel(lvTrace);
        else devLog.SetLevel(lvOff);
    end;

    case settings.LogSettings.IMAPLog of
        0: imapLog.SetLevel(lvOff);
        1: imapLog.SetLevel(lvInfo);
        2: imapLog.SetLevel(lvDebug);
        else imapLog.SetLevel(lvOff);
    end;

    backupLog.SetLevel(lvInfo);

    fileAppender.MaxBackupIndex:=settings.LogSettings.FilesToKeep;
    fileAppender.MaxSize:=settings.LogSettings.MaxFileSize;
end;

{ Sets the level of the specified logger.
  @param logger - DEV_LOGGER or IMAP_LOGGER
  @param level - the new logger level }
procedure TVecLog.SetLoggerLevel(logger: Integer; level: TVLevelValue);
begin
    case logger of
        DEV_LOGGER: devLog.SetLevel(level);
        IMAP_LOGGER: imapLog.SetLevel(level);
        BACKUP_LOGGER: backupLog.SetLevel(level);
    else
        devLog.Error('Unknown logger specified');
    end;
end;

{ Flushes any content in appenders to file(s) }
procedure TVecLog.Flush;
begin
    fileAppender.Flush;
end;

destructor TVecLog.Destroy;
begin
    Flush;
    devLog.Free;
    imapLog.Free;
    backupLog.Free;
    Inherited Destroy;
end;


end.