{==============================================================================|
| Project : IMAPSize                                                           |
|==============================================================================|
| $Id: Log.pas,v 1.7 2004/03/31 23:27:36 Ivan Exp $
|==============================================================================|
| Copyright 2003-2004 Ivan Vecanski                                            |
| =============================================================================}

unit Log;

{ Class that encapsulates writting to log files. This logging is used for internal
  logging (during dev) and is being switched on/off through the DEBUG compiler directive
  below.

  Can change logger class without changing the code.
  Usage:
  - Should be created on FMain.Create
  - From this moment on, can be used from any unit that uses Log, through
    the provided interface (dbg, app, etc)
  - Should be destroyed on FMain.Destroy

This logger writes to a file as soon as it gets the log event. This is slow,
but can be usefull in certain situations. For normal logging use VecLog (vectrics log),
it is much more efficient.

}

// Here is where the change is made to enable/disable internal logging (debug.log)
// Set to {.$DEFINE DEBUG} to disable, remove dot to enable
{.$DEFINE DEBUG}

interface

uses fnglogfile, classes, sysutils;

type

    TLog = class
        constructor Create(AOwner: TComponent);
        destructor Destroy; override;
    end;

var
    debugLogger: TFnugryLogFile;
    mDebugLogEnabled: boolean;
    mDebugSessionStarted: boolean;
    procedure dbg(str: String);
    procedure EnableDebugLog(enable: Boolean);
    function IsDebugDefined: Boolean;


implementation

{ Returns true if the internal debug feature is turned on }
function IsDebugDefined: Boolean;
begin
    {$IFDEF DEBUG}
    Result:=true;
    {$ELSE}
    Result:=false;
    {$ENDIF}
end;

constructor TLog.Create(AOwner: TComponent);
begin
    debugLogger := TFnugryLogFile.Create(AOwner);
    debugLogger.LogFileName := 'debug.log';
    debugLogger.Options := debugLogger.Options - [logTimestamp];
    debugLogger.ArchiveMode := bkNone;
    mDebugLogEnabled:=false;
    mDebugSessionStarted:=false;
    EnableDebugLog(true);
end;


destructor TLog.Destroy;
begin
    if debugLogger.LogOpen then debugLogger.Close;
    debugLogger.Free;
    Inherited Destroy;
end;

procedure EnableDebugLog(enable: Boolean);
begin
    mDebugLogEnabled:=enable;

    if enable then begin
        // Open file if needed
        if not debugLogger.LogOpen then debugLogger.Open;
    end
    else begin
        // Close file if needed
        if debugLogger.LogOpen then debugLogger.Close;
    end;

    if enable and (not mDebugSessionStarted) then begin
        mDebugSessionStarted:=true;
    end;
end;


procedure dbg(str: String);
begin
    {$IFDEF DEBUG}
    debugLogger.LogMsg('IMAPSize', '*', str);
    {$ENDIF}
end;


end.