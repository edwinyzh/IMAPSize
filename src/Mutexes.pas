{==============================================================================|
| Project : IMAPSize                                                           |
|==============================================================================|
| $Id: Mutexes.pas,v 1.10 2004/04/04 20:17:12 Ivan Exp $
|==============================================================================|
| Copyright 2003-2004 Ivan Vecanski                                            |
| =============================================================================}

{ Utility class which encapsulates mutex usage
  Aim is to facilitate clean multiple synchronous mutex synchronization }
unit Mutexes;

interface

uses windows, sysutils;

const MAX_MUTEX = 20; // Maximum number of simultaneous synchronizations with mutexes

type

PHandle = ^THandle;

// THandleStatus = (thsNotCreated, thsAvailable, thsInUse);

TMutexes = class
private
    handlePtrs: array [1..MAX_MUTEX] of PHandle;
    handleInUse: array [1..MAX_MUTEX] of Boolean;
public
    constructor Create;
    destructor Destroy; override;
    function AssignMutex: Integer;
    function GetMutex(index: Integer): THandle;
    procedure ReleaseMutex(index: Integer);
end;

implementation

constructor TMutexes.Create;
var i: Integer;
begin
    for i:=1 to MAX_MUTEX do handlePtrs[i]:=nil;
    for i:=1 to MAX_MUTEX do handleInUse[i]:=false;
end;

destructor TMutexes.Destroy;
var i: Integer;
begin
    for i:=1 to MAX_MUTEX do begin
        if handleInUse[i] then begin
            CloseHandle(handlePtrs[i]^);
        end;
        Dispose(handlePtrs[i]);
    end;
end;

{ Call this method to get the first available mutex.
  Returns the index of the mutex }
function TMutexes.AssignMutex: Integer;
var i: Integer; found: Boolean; mutex: THandle;
begin
    i:=1;
    found:=false;
    while ((not found) and (i<=MAX_MUTEX)) do begin
        if not handleInUse[i] then begin
            // Create the mutex
            mutex:=CreateMutex(nil,false,PChar('IMAPSize Login Mutex '+IntToStr(i)));
            New(handlePtrs[i]);
            handlePtrs[i]:=@mutex;
            handleInUse[i]:=true;
            found:=true;
        end
        else Inc(i);
    end;
    if found then Result:=i
    else raise Exception.Create('Mutex overflow. Please report this error to bugs@broobles.com');
end;

function TMutexes.GetMutex(index: Integer): THandle;
begin
    if handleInUse[index] then Result:=handlePtrs[index]^
    else raise Exception.Create('Mutex with index '+IntToStr(index)+' does not exist');
end;

{ Blocker thread should call this method to unblock the blocked thread }
procedure TMutexes.ReleaseMutex(index: Integer);
begin
    if handleInUse[index] then begin
        ReleaseMutex(handlePtrs[index]^);
        // Clean up
        CloseHandle(handlePtrs[index]^);
        Dispose(handlePtrs[index]);
        handleInUse[index]:=false;
    end;
    // @todo Cover other cases (shouldn't happen)
end;

end.